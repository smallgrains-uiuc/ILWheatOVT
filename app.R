library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(devtools)
library(dplyr)
library(reshape)
library(htmltools)
library(knitr)

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
      
      .btn {
        padding: 2px 8px !important;
        border-radius: 4px !important;
        font-size: 0.9em;
      }
      
      .toggle-rail{
        width: 26px;
        background: white;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .sidebar-arrow{
        padding: 0;
        width: 25px;
        height: 35px;
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
        line-height: 1;
        text-align: center !important;
        vertical-align: middle !important;
        white-space: nowrap;
        cursor: pointer;
      }
      
      table.dataTable tbody td {
        padding: 4px 6px !important;
        text-align: center !important;
        vertical-align: middle !important;
      }

      th.study-sep, td.study-sep {
        border-right: 2px solid #444 !important;
      }
      
      .shiny-options-group {
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 8px 14px;
      }
      
      .shiny-options-group .checkbox {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      
      #idle-warning {
        position: fixed;
        bottom: 20px;
        right: 20px;
        background: rgba(0,0,0,0.8);
        color: white;
        padding: 10px 15px;
        border-radius: 8px;
        font-size: 14px;
        display: none;
        z-index: 9999;
      }
    ")),
    
    tags$script(HTML("
      var idleTime = 600000; // 10 min
      var countdown = 10;
      var idleTimer = null;
      var countdownTimer = null;
    
      function resetTimer() {
        clearTimeout(idleTimer);
        clearInterval(countdownTimer);
        document.getElementById('idle-warning').style.display = 'none';
    
        idleTimer = setTimeout(startCountdown, idleTime);
      }
    
      function startCountdown() {
        var counter = countdown;
        var box = document.getElementById('idle-warning');
        box.style.display = 'block';
  
        box.innerHTML = 'No activity detected. Page will reload in ' + counter + 's';
  
        countdownTimer = setInterval(function() {
          counter--;
          box.innerHTML = 'No activity detected. Reloading in ' + counter + 's';
  
          if (counter <= 0) {
            clearInterval(countdownTimer);
            location.reload();
          }
        }, 1000);
      }
    
      // Monitor user activity
      ['mousemove', 'keydown', 'click', 'scroll'].forEach(function(evt) {
        document.addEventListener(evt, resetTimer, true);
      });
    
      document.addEventListener('DOMContentLoaded', function() {
        resetTimer();
      });
    "))
  ),
  
  div(id = "idle-warning"),
  
  div(class = "fixed-title",
      h2("Illinois Wheat Variety Test Data Overview"),
      popover(
        actionButton("legend_btn", label = NULL, "Instructions",
                     style = "margin-left: 10px;"),
        title = "Instructions",
        placement = "bottom",
        instruction_text,
        options = list(html = TRUE, trigger = "click")
      ),
      div(style = "margin-left: 15px;",
          downloadButton("download_html", "Download HTML Table"))
  ),
  
  div(class = "main-container",
      div(class = "sidebar-panel",
          selectInput("region", "Region:", choices = c("North", "South"), selected = "North"),
          selectInput("smry", "Summary type:", choices = c("Compact", "Detailed"), selected = "Compact"),
          
          uiOutput("search_ui"),
          actionButton("search_btn", "Search"),
          actionButton("clear_search", "Clear Search"),
          br(),br(),
          actionButton("toggle_star_all", "Star/Unstar All",
                       title = "Star/unstar all currently visible varieties."),
          actionButton("show_starred", "Show/Hide Starred",
                       title = "Click again to return to the full table."),
          br(),br(),
          
          div(
            title = "A higher value indicates a later maturity, a lower value indicates an earlier maturity. A value of 0 indicates the earliest maturing variety in the region.",
            uiOutput("md_slider_ui")
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
  search_term <- reactiveVal("")
  starred <- reactiveVal(character(0))
  show_starred_only <- reactiveVal(FALSE)
  
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
  
  observe({
    df <- table_data()
    req(df)
    
    choices <- sort(unique(c(as.character(df$company), as.character(df$number))))
    
    updateSelectizeInput(
      session, "search_text",
      choices = choices,
      selected = ""
    )
  })
  
  # slider debounce
  md_range_debounced <- reactive(input$md_range) |> debounce(300)
  
  row_filters <- reactive({
    list(
      jtd = input$show_jtd %||% character(0),
      scab = input$show_scab %||% character(0)
    )
  })
  
  # Basic filter: apply slider/row filtering, retaining all site columns
  data_filtered_base <- reactive({
    df <- table_data()
    req(df)
    
    df <- filter_by_maturity_range(df, input$region, md_range_debounced())
    
    filters <- row_filters()
    
    jtd_col <- grep("^Jointing.Category", names(df), value = TRUE)
    if (length(jtd_col) == 1) {
      df <- df[df[[jtd_col]] %in% filters$jtd, , drop = FALSE]
    }
    
    scab_col <- grep("^Scab.Category", names(df), value = TRUE)
    if (length(scab_col) == 1) {
      df <- df[df[[scab_col]] %in% filters$scab, , drop = FALSE]
    }
    
    df
  })
  
  # Search filter
  data_filtered_search <- reactive({
    filter_by_search(data_filtered_base(), search_term())
  })
  
  current_visible_varieties <- reactive({
    df <- display_data()
    req(df)
    unique(as.character(df$number))
  })
  
  # Star filter
  data_filtered_rows <- reactive({
    filter_by_starred(
      data_filtered_search(),
      starred(),
      show_starred_only()
    )
  })
  
  current_n <- reactive({
    df <- data_filtered_rows()   # include search / slider / scab / jtd / starred
    req(df)
    nrow(df)
  })
  
  # Final data displayed with columns hidden
  display_data <- reactive({
    df <- data_filtered_rows()
    
    selected_sites <- input$show_site %||% character(0)
    
    all_sites <- switch(input$region,
                        South = c("Addieville", "Elkville", "StPeter"),
                        North = c("Hampshire", "Perry", "Urbana"))
    
    sites_to_hide <- setdiff(all_sites, selected_sites)
    
    if (length(sites_to_hide) > 0) {
      cols_to_hide <- unlist(lapply(sites_to_hide, function(site) {
        grep(paste0("_", site, "$"), names(df), value = TRUE)
      }))
      cols_to_hide <- intersect(cols_to_hide, names(df))
      
      if (length(cols_to_hide) > 0) {
        df <- df[, !names(df) %in% cols_to_hide, drop = FALSE]
      }
    }
    
    df
  })
  
  # Rebuild table when Region/Smry/column changes/hidden
  rebuild_trigger <- reactiveVal(0)
  observeEvent(list(input$region, input$smry, input$show_site), {
    rebuild_trigger(rebuild_trigger() + 1)
  })
  
  # Cache boundary_cols calculation
  boundary_cols <- reactive({
    df <- display_data()
    data_cols <- names(df)[!names(df) %in% c("star", "company", "number")]
    if (length(data_cols) == 0) return(NULL)
    parsed <- do.call(rbind, strsplit(data_cols, "_"))
    study <- parsed[, 2]
    which(!duplicated(study, fromLast = TRUE)) + 2
  })
  
  observeEvent(input$search_btn, {
    search_term(trimws(input$search_text %||% ""))
  })
  
  observeEvent(input$clear_search, {
    search_term("")
    updateSelectizeInput(session, "search_text", selected = "")
  })
  
  # Star / Unstar
  observeEvent(input$toggle_star, {
    key <- as.character(input$toggle_star)
    cur <- starred()
    
    if (key %in% cur) {
      starred(setdiff(cur, key))
    } else {
      starred(c(cur, key))
    }
    
    if (show_starred_only()) {
      df <- display_data()
      df <- cbind(
        star = ifelse(df$number %in% starred(), "★", "☆"),
        df,
        stringsAsFactors = FALSE
      )
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  # Star/Unstar All
  observeEvent(input$toggle_star_all, {
    visible_keys <- current_visible_varieties()
    cur <- starred()
    
    if (length(visible_keys) == 0) return()
    
    if (all(visible_keys %in% cur)) {
      starred(setdiff(cur, visible_keys))
    } else {
      starred(union(cur, visible_keys))
    }
    
    df <- display_data()
    df <- cbind(
      star = ifelse(df$number %in% starred(), "★", "☆"),
      df,
      stringsAsFactors = FALSE
    )
    replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$show_starred, {
    show_starred_only(!show_starred_only())
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
  
  # Update input according to region and/or summary type
  observeEvent(input$region, {
    req(table_data())
    
    search_term("")
    starred(character(0))
    show_starred_only(FALSE)
    
    updateSelectizeInput(session, "search_text", selected = "")
    updateCheckboxGroupInput(
      session, "show_scab",
      selected = c("S", "MS", "M", "MR")
    )
    updateCheckboxGroupInput(session, "show_jtd", selected = c("E", "M", "L"))
    updateCheckboxGroupInput(
      session, "show_site",
      selected = switch(input$region,
                        South = c("Addieville", "Elkville", "StPeter"),
                        North = c("Hampshire", "Perry", "Urbana"))
    )
    
    ranges <- get_maturity_range(table_data(), input$region)
    updateSliderInput(
      session, "md_range",
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = ranges$Maturity.Date
    )
  })
  
  observeEvent(input$smry, {
    req(table_data())
    
    search_term("")
    updateSelectizeInput(session, "search_text", selected = "")
    
    updateCheckboxInput(session, "hide_susceptible", value = FALSE)
    updateCheckboxGroupInput(session, "show_jtd", selected = c("E", "M", "L"))
    updateCheckboxGroupInput(
      session, "show_site",
      selected = switch(input$region,
                        South = c("Addieville", "Elkville", "StPeter"),
                        North = c("Hampshire", "Perry", "Urbana"))
    )
    
    ranges <- get_maturity_range(table_data(), input$region)
    updateSliderInput(
      session, "md_range",
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = ranges$Maturity.Date
    )
  })
  
  # Dynamic UI
  output$search_ui <- renderUI({
    selectizeInput(
      "search_text",
      "Search (companies / varieties):",
      choices = NULL,
      selected = "",
      multiple = TRUE,
      options = list(
        placeholder = "Type companies / varieties"
      )
    )
  })
  
  output$md_slider_ui <- renderUI({
    df <- data_filtered_rows()
    req(df)
    ranges <- get_maturity_range(table_data(), input$region)
    sliderInput(
      "md_range",
      label = paste0("Maturity range (", nrow(df), ")"),
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = input$md_range %||% ranges$Maturity.Date
    )
  })
  
  output$scab_checkboxes <- renderUI({
    df <- data_filtered_rows()
    req(df)
    
    scab_col <- grep("^Scab.Category", names(df), value = TRUE)
    counts <- table(df[[scab_col]])
    choices <- c("S", "MS", "M", "MR")
    labels <- paste0(
      choices,
      " (", counts[choices] %||% 0, ")"
    )
    
    checkboxGroupInput("show_scab", "Show scab resistance:",
      choices = setNames(choices, labels),
      selected = input$show_scab %||% choices,
      inline = TRUE
    )
  })
  
  output$jtd_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    df <- data_filtered_rows()
    req(df)
    
    jtd_col <- grep("^Jointing.Category", names(df), value = TRUE)
    counts <- table(df[[jtd_col]])
    choices <- c("E", "M", "L")
    labels <- paste0(
      choices,
      " (", counts[choices] %||% 0, ")"
    )
    
    checkboxGroupInput(
      "show_jtd",
      "Show jointing categories:",
      choices = setNames(choices, labels),
      selected = input$show_jtd %||% choices,
      inline = TRUE
    )
  })
  
  output$site_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- switch(input$region,
                      South = c("Addieville", "Elkville", "StPeter"),
                      North = c("Hampshire", "Perry", "Urbana"))
    checkboxGroupInput("show_site", "Show test sites:",
      choices = choices,
      selected = choices,
      inline = TRUE)
  })
  
  output$download_html <- downloadHandler(
    filename = function() {
      paste0("Wheat_Varieties_", input$region, "_", input$smry, "_", ".html")
    },
    content = function(file) {
      df <- display_data()
      html <- kable(df, format = "html")
      writeLines(as.character(html), file)
    }
  )
  
  output$table <- renderDT({
    # Rely on rebuild trigger to rebuild only when necessary
    rebuild_trigger()
    
    # Avoid rebuild triggered by slider/row filtering
    df <- display_data()
    req(df)
    
    df <- cbind(
      star = ifelse(df$number %in% isolate(starred()), "★", "☆"),
      df,
      stringsAsFactors = FALSE
    )
    
    col_defs <- list(
      list(targets = "_all", className = "dt-center"),
      list(targets = 0, orderable = FALSE)
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
      callback = JS(
        "var dt = table;",
        "var bindStarClick = function(tbl) {",
        "  $(tbl.table().container()).on('click', 'tbody td:first-child', function() {",
        "    var rowData = tbl.row($(this).closest('tr')).data();",
        "    if (!rowData) return;",
        "    var key = rowData[2];",
        "    var current = $(this).text().trim();",
        "    $(this).text(current === '★' ? '☆' : '★');",
        "    Shiny.setInputValue('toggle_star', key, {priority: 'event'});",
        "  });",
        "};",
        "bindStarClick(dt);",
        "setTimeout(function() {",
        "  $('.DTFC_Cloned').each(function() {",
        "    $(this).on('click', 'tbody td:first-child', function() {",
        "      var rowText = $(this).closest('tr').find('td').eq(2).text().trim();",
        "      var current = $(this).text().trim();",
        "      $(this).text(current === '★' ? '☆' : '★');",
        "      Shiny.setInputValue('toggle_star', rowText, {priority: 'event'});",
        "    });",
        "  });",
        "}, 300);"
      ),
      options = list(
        scrollY = "calc(100vh - 180px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        paging = FALSE,
        dom = 't',
        fixedColumns = list(leftColumns = 3),
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
  proxy <- dataTableProxy("table")
  
  observeEvent(
    list(
      md_range_debounced(),
      input$hide_susceptible,
      input$show_jtd,
      input$show_site,
      input$search_btn,
      input$toggle_star_all,
      input$show_starred,
      input$smry,
      input$region
    ),
    {
      df <- display_data()
      df <- cbind(
        star = ifelse(df$number %in% isolate(starred()), "★", "☆"),
        df,
        stringsAsFactors = FALSE
      )
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)

# star/unstar all
# checkbox layout
# show the filtered row number
# search multiple companies/varieties
# export option: kable()