library(shiny)
library(DT)
library(bslib)
library(devtools)
library(dplyr)
library(reshape)
library(htmltools)

if (!require(ILWheatOVT)) devtools::install_github("smallgrains-uiuc/ILWheatOVT")
library(IllinoisOVT)

#load in data to the workspace
data(WheatOVT25)


ui <- fluidPage(
  theme = bs_theme(
    primary = "#13294B",   # RGB 255,95,5 Illini Blue
    secondary = "#FF5F05"  # RGB 19,41,75 Illini Orange
  ),
  
  tags$head(
    tags$style(HTML("
      /* Prevent entire webpage scrolling */
      html, body {
        height: 100%;
        margin: 0;
        overflow: hidden;
      }
      
      .fixed-title {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        height: 60px;
        z-index: 1000;
        display: flex;
        align-items: center;
        padding: 0 20px;
      }
      
      .main-container {
        margin-top: 60px;
        height: calc(100vh - 60px);
        display: flex;
        overflow: hidden;
      }
      
      .sidebar-panel {
        width: 25%;
        height: 100%;
        overflow-y: auto;
      }
      
      .main-panel {
        flex: 1;
        height: 100%;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      
      .dataTables_wrapper {
        flex: 1;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      
      .dataTables_scroll {
        flex: 1;
        display: flex;
        flex-direction: column;
      }
      
      .dataTables_scrollHead {
        flex-shrink: 0;
      }
      
      .dataTables_scrollBody {
        flex: 1;
        overflow-y: auto !important;
      }
      
      .DTFC_LeftWrapper,
      .DTFC_LeftHeadWrapper,
      .DTFC_LeftBodyWrapper {
        z-index: 2 !important;
      }

      table.dataTable thead th {
        font-size: 12px;
        padding: 2px 4px;
        line-height: 1.1;
        vertical-align: middle !important;
        text-align: center !important;
        white-space: nowrap;
      }
      
      th.study-sep, td.study-sep {
        border-right: 2px solid #444 !important;
      }
    "))
  ),
  
  div(class = "fixed-title",
      h2("Illinois Wheat Variety Test Data Overview")
  ),
  
  div(class = "main-container",
      div(class = "sidebar-panel",
          selectInput("region", "Region:", choices = c("North", "South"), selected = "North"),
          selectInput("smry", "Summary type:", choices = c("Compact", "Detailed"), selected = "Compact"),
          br(),
          sliderInput("md_range", "Maturity Date Range", min = -50, max = 50, value = c(-50, 50)),
          actionButton("refresh_sliders", "Refresh Sliders"),
          br(), br(),
          uiOutput("scab_checkboxes"),
          uiOutput("jtd_checkboxes"),
          uiOutput("site_checkboxes")
      ),
      
      div(class = "main-panel",
          div(class = "table-container",
              DTOutput("table")
          )
      )
  )
)

server <- function(input, output, session) {
  
  table_data <- reactive({
    # pipe operators replace repeated assignments
    WheatOVT25 |>
      normalize_maturity() |>
      normalize_heading() |>
      prepare_table(Region = input$region, smryType = input$smry) |>
      scab_res_level() |>
      jointing_level()
  })
  
  # update the sliders according to region/summary type
  observe({
    req(table_data())
    ranges <- get_maturity_range (table_data(), input$region)
    
    updateSliderInput(session, "md_range",
                      min = ranges$Maturity.Date[1],
                      max = ranges$Maturity.Date[2],
                      value = ranges$md_range)
  })
  
  # dynamically update sliders' values
  observeEvent(input$md_range, {
    req(table_data())
    
    df_now <- filter_by_maturity_range(
      table_data(), input$region, input$md_range
    )
    r <- get_maturity_range (df_now, input$region)
    
    new_md <- c(
      max(min(input$md_range[1], r$Maturity.Date[2]), r$Maturity.Date[1]),
      min(max(input$md_range[2], r$Maturity.Date[1]), r$Maturity.Date[2])
    )
    
    updateSliderInput(session, "md_range", value = new_md)
  })
  
  # filter values according to the sliders
  filtered_table <- reactive({
    req(table_data())
    filter_by_maturity_range(table_data(), input$region, input$md_range)
  })

  # refresh sliders
  observeEvent(
    list(input$region, input$smry, input$refresh_sliders),
    {
      reset_sliders(session, table_data, input$region)
    }
  )
  
  output$jtd_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- c("E", "M", "L")
    checkboxGroupInput("hide_jtd", "Hide jointing cat.:", choices = choices, selected = character(0))
  })
  
  output$scab_checkboxes <- renderUI({
    checkboxInput("hide_susceptible", "Hide Scab-susceptible varieties:", FALSE)
  })
  
  output$site_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- switch(input$region,
                      South = c("Addieville", "Elkville", "StPeter"),
                      North = c("Hampshire", "Perry", "Urbana"))
    checkboxGroupInput("hide_site", "Hide test sites:", choices = choices, selected = character(0))
  })
  
  output$table <- renderDT({
    df <- filtered_table()
    req(df)
    
    # hide rows based on Jointing categories
    if (!is.null(input$hide_jtd) && length(input$hide_jtd) > 0) {
      df <- df[!df$Jointing.Category %in% input$hide_jtd, , drop = FALSE]
    }
    
    # hide rows based on Scab.Category
    if (isTRUE(input$hide_susceptible)) {
      df <- df[!df$Scab.Category %in% c("S", "MS"), , drop = FALSE]
    }
    
    # hide columns according to the site checkboxes
    if (!is.null(input$hide_site) && length(input$hide_site) > 0) {
      hide_patterns <- input$hide_site
      cols_to_hide <- unlist(sapply(hide_patterns, function(pat) grep(pat, names(df), value = TRUE)))
      df <- df[, setdiff(names(df), cols_to_hide), drop = FALSE]
    }
    
    data_cols <- names(df)[!names(df) %in% c("company","number")]
    parsed <- do.call(rbind, strsplit(data_cols, "_"))
    study <- parsed[,2]
    boundary_cols <- which(!duplicated(study, fromLast = TRUE)) + 1
    
    datatable(
      df,
      container = make_tooltip_container(df),
      rownames = FALSE,
      escape = FALSE,
      extensions = c("FixedColumns", "FixedHeader"),
      options = list(
        scrollY = "calc(100vh - 180px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        paging = FALSE,
        fixedColumns = list(leftColumns = 2),
        fixedHeader = TRUE,
        dom = 'tip',
        
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-center",
            render = JS("function(data, type, row, meta) {",
                        "return type === 'display' && data != null ?", 
                        "'<div style=\\\"padding: 2px 4px; font-size: 11px;\\\">' + data + '</div>' : data;",
                        "}")
          ),
          list(
            targets = boundary_cols,
            className = "study-sep"
          )
          
        )
      )
    ) |>
      formatStyle(columns = names(df), 
                  fontSize = '11px',
                  padding = '2px 4px')
  })
}

shinyApp(ui, server)
