library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(devtools)
library(dplyr)
library(reshape)
library(htmltools)

install_github("smallgrains-uiuc/ILWheatOVT")
library(IllinoisOVT)

#load in data to the workspace
data(WheatOVT25)

instruction_text <- HTML("
<b>*</b> You may interact with the sidebar. Select the region and table type of interest; narrow down the range of maturity or jointing date category; hide Scab-susceptible varieties; hide other test sites to compare site of interest and regional averages.<br>
Clicking on column names allows you to change the data sorting; upward arrows indicate ascending order, and downward arrows indicate descending order.<br>
<br>
<b>1.</b> Northern phenology is based on data collected in Urbana; Southern phenology is based on data collected in St. Peter.<br>
<b>2.</b> Varieties were evaluated for the heading date and the date when they reached maturity. A higher value indicates a later date, a lower value indicates an earlier date. A value of 0 indicates the earliest heading/maturing variety in the region.<br>
<b>3.</b> Varieties were evaluated for the timing of jointing and classified into categories: E = early, M = medium, L = Late. Jointing begins at Feekes growth stage 6 and marks the beginning of more rapid growth and increased vulnerability to freeze damage. Varieties that are classified as E, jointed at a time when hard freezes are probable. Varieties that are classified as M, jointed at a time when hard freezes are possible, but unlikely. Varieties classified as L, jointed at a time when hard freezes are highly unlikely.<br>
<b>4.</b> Varieties were evaluated for resistance to Scab (FHB) at the University of Illinois South Farm at Urbana, IL. To promote disease symptoms, we spread scabby corn kernels on the soil surface 3 weeks before heading and mist irrigated three times per day for 30 minutes. Ratings for each variety are based on expected vomitoxin levels under FHB epidemic conditions. Ratings are expressed on a 1 to 9 scale with 1 being the best (lowest vomitoxin) and 9 being the worst (highest vomitoxin). Varieties with ratings 4.3 or lower are considered at least moderately resistant to FHB. MR = Moderately Resistant, which is the highest level of resistance available, M = Intermediate level of resistance, MS = Moderately susceptible, S = Susceptible. Higher levels of resistance provide greater control of the disease. The best control is obtained when moderate resistance is combined with a fungicide at flowering.
")

ui <- fluidPage(
  useShinyjs(),
  
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
      
      .popover {
        max-width: 500px !important;
        font-size: 12px !important;
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
        font-size: 0.9em;
      }
      
      .toggle-rail{
        width: 20px;
        background: white;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .sidebar-arrow{
        padding: 0;
        width: 20px;
        height: 26px;
        font-size: 12px;
      }
      
      .sidebar-collapsed .sidebar-panel{
        width:0;
        overflow:hidden;
      }
      
      .sidebar-collapsed .toggle-rail{
        border-left:none;
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
      
      table.dataTable thead th {
        font-size: 18px;
        padding: 6px 10px;
        line-height: 1.1;
        text-align: center !important;
        vertical-align: middle !important;
        white-space: nowrap;
      }
      
      table.dataTable tbody td {
        padding: 4px 6px !important;
        text-align: center !important;
        vertical-align: middle !important;
      }

      th.study-sep, td.study-sep {
        border-right: 2px solid #444 !important;
      }
    "))
  ),
  
  div(class = "fixed-title",
      h2("Illinois Wheat Variety Test Data Overview"),
      popover(
        actionButton("legend_btn", label = NULL, "Instructions",
                     style = "margin-left: 10px;"),
        title = "Instructions",
        placement = "bottom",
        instruction_text,
        options = list(html = TRUE, trigger = "click")
      )
  ),
  
  div(class = "main-container",
      div(class = "sidebar-panel",
          selectInput("region", "Region:", choices = c("North", "South"), selected = "North"),
          selectInput("smry", "Summary type:", choices = c("Compact", "Detailed"), selected = "Compact"),
          
          div(
            title = "A higher value indicates a later maturity, a lower value indicates an earlier maturity. A value of 0 indicates the earliest maturing variety in the region.",
            sliderInput("md_range", "Maturity Date Range:", min = -50, max = 50, value = c(-50, 50))
          ),
          actionButton("refresh_sliders", "Refresh Sliders"),
          br(), br(),
          uiOutput("scab_checkboxes"),
          uiOutput("jtd_checkboxes"),
          uiOutput("site_checkboxes")
      ),
      
      div(class = "toggle-rail",
          actionButton("toggleSidebar", NULL, icon = icon("chevron-left"), class = "sidebar-arrow")
      ),
      
      div(class = "main-panel",
          div(class = "table-container",
              DTOutput("table")
          )
      )
  )
)

server <- function(input, output, session) {
  
  # Prepare table
  table_data <- reactive({
    # pipe operators replace repeated assignments
    WheatOVT25 |>
      normalize_maturity() |>
      normalize_heading() |>
      prepare_table(Region = input$region, smryType = input$smry) |>
      scab_res_level() |>
      jointing_level()
  })
  
  # slider debounce
  md_range_debounced <- reactive(input$md_range) |> debounce(300)
  
  row_filters <- reactive({
    list(
      jtd = input$hide_jtd %||% character(0),
      scab = isTRUE(input$hide_susceptible)
    )
  })
  
  # Basic filter: apply slider/row filtering, retaining all site columns
  data_filtered_base <- reactive({
    df <- table_data()
    req(df)
    
    df <- filter_by_maturity_range(df, input$region, md_range_debounced())
    
    filters <- row_filters()
    if (length(filters$jtd) > 0 && "Jointing.Category" %in% names(df)) {
      df <- df[!df$Jointing.Category %in% filters$jtd, , drop = FALSE]
    }
    if (filters$scab && "Scab.Category" %in% names(df)) {
      df <- df[!df$Scab.Category %in% c("S", "MS"), , drop = FALSE]
    }
    
    df
  })
  
  # Final data displayed with columns hidden
  display_data <- reactive({
    df <- data_filtered_base()
    
    if (!is.null(input$hide_site) && length(input$hide_site) > 0) {
      cols_to_hide <- unlist(lapply(input$hide_site, function(pat) {
        grep(pat, names(df), value = TRUE)
      }))
      cols_to_hide <- intersect(cols_to_hide, names(df))
      if (length(cols_to_hide) > 0) {
        df <- df[, !names(df) %in% cols_to_hide, drop = FALSE]
      }
    }
    
    df
  })
  
  # Rebuild table only when Region/Smry/column changes/hidden
  rebuild_trigger <- reactiveVal(0)
  observeEvent(list(input$region, input$smry, input$hide_site), {
    rebuild_trigger(rebuild_trigger() + 1)
  })
  
  # Cache boundary_cols calculation
  boundary_cols <- reactive({
    df <- display_data()
    data_cols <- names(df)[!names(df) %in% c("company", "number")]
    if (length(data_cols) == 0) return(NULL)
    parsed <- do.call(rbind, strsplit(data_cols, "_"))
    study <- parsed[, 2]
    which(!duplicated(study, fromLast = TRUE)) + 1
  })
  
  # Update slider according to region/summary type
  observeEvent(input$smry, {
    updateCheckboxInput(session, "hide_susceptible", value = FALSE)
    updateCheckboxGroupInput(session, "hide_jtd", selected = character(0))
    updateCheckboxGroupInput(session, "hide_site", selected = character(0))
    
    ranges <- get_maturity_range(table_data(), input$region)
    updateSliderInput(session, "md_range",
                      min = ranges$Maturity.Date[1],
                      max = ranges$Maturity.Date[2],
                      value = ranges$Maturity.Date)
  })
  
  observeEvent(input$refresh_sliders, {
    reset_sliders(session, table_data, input$region)
  })
  
  observeEvent(input$md_range, {
    req(table_data())
    
    df_now <- filter_by_maturity_range(table_data(), input$region, input$md_range)
    r <- get_maturity_range(df_now, input$region)
    
    new_md <- c(
      max(min(input$md_range[1], r$Maturity.Date[2]), r$Maturity.Date[1]),
      min(max(input$md_range[2], r$Maturity.Date[1]), r$Maturity.Date[2])
    )
    
    updateSliderInput(session, "md_range", value = new_md)
  })
  
  # Dynamic UI
  output$scab_checkboxes <- renderUI({
    div(
      title = 'Hide varieties with Scab resistance category "S" and "MS".',
      checkboxInput("hide_susceptible", "Hide Scab-susceptible varieties:", FALSE)
    )
  })
  
  output$jtd_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- c("E", "M", "L")
    checkboxGroupInput("hide_jtd", "Hide jointing categories:", choices = choices, selected = character(0))
  })
  
  output$site_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- switch(input$region,
                      South = c("Addieville", "Elkville", "StPeter"),
                      North = c("Hampshire", "Perry", "Urbana"))
    checkboxGroupInput("hide_site", "Hide test sites:", choices = choices, selected = character(0))
  })
  
  output$table <- renderDT({
    # Rely on rebuild trigger to rebuild only when necessary
    rebuild_trigger()
    
    # Avoid rebuild triggered by slider/row filtering
    df <- isolate(display_data())
    req(df)
    
    col_defs <- list(
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
    bc <- boundary_cols()
    if (!is.null(bc)) {
      col_defs <- append(col_defs, list(list(targets = bc, className = "study-sep")))
    }
    
    datatable(
      df,
      container = make_tooltip_container(df),
      rownames = FALSE,
      escape = FALSE,
      extensions = c("FixedColumns"),
      options = list(
        scrollY = "calc(100vh - 180px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        paging = FALSE,
        dom = 't',
        fixedColumns = list(leftColumns = 2),
        columnDefs = col_defs
      )
    ) |>
      formatStyle(columns = names(df), 
                  fontSize = '16px',
                  padding = '2px 4px')
  })
  
  # Sidebar collapse
  collapsed <- reactiveVal(FALSE)
  observeEvent(input$toggleSidebar, {
    collapsed(!collapsed())
    if (collapsed()) {
      addClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "toggleSidebar", icon = icon("chevron-right"))
    } else {
      removeClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "toggleSidebar", icon = icon("chevron-left"))
    }
    
    # recalculate the dt width to prevent header and column misalignment
    runjs("setTimeout(function(){ $(window).trigger('resize'); },300);")
    
  })
  
  # Table proxy for slider/row filtering
  proxy <- dataTableProxy('table')
  observeEvent(
    list(md_range_debounced(), input$hide_jtd, input$hide_susceptible),
    {
      df <- display_data()
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
