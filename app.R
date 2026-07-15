library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(devtools)
library(dplyr)
library(reshape)
library(htmltools)
library(knitr)

if (!requireNamespace("IllinoisOVT", quietly = TRUE)) {
  devtools::install_github("smallgrains-uiuc/ILWheatOVT")
}
library(IllinoisOVT)



# Load the current-year wheat trial dataset used by this browser.
data(WheatOVT26)

# User-facing help Text
instruction_text <- HTML("
<b>*</b> You may interact with the sidebar. Select the region and table type of interest; narrow down the range of maturity or jointing date category; hide Scab-susceptible varieties; hide other test sites to compare site of interest and regional averages.<br>
Clicking on column names allows you to change the data sorting; upward arrows indicate ascending order, and downward arrows indicate descending order.<br>
<br>
<b>1.</b> Northern phenology is based on data collected in Urbana; Southern phenology is based on data collected in St. Peter.<br>
<b>2.</b> Varieties were evaluated for the heading date and the date when they reached maturity. A greater value indicates a later date, a smaller value indicates an earlier date. A value of 0 indicates the earliest heading/maturing variety in the region.<br>
<b>3.</b> Varieties were evaluated for the timing of jointing and classified into categories: E = early, M = medium, L = Late. Jointing begins at Feekes growth stage 6 and marks the beginning of more rapid growth and increased vulnerability to freeze damage. Varieties that are classified as E, jointed at a time when hard freezes are probable. Varieties that are classified as M, jointed at a time when hard freezes are possible, but unlikely. Varieties classified as L, jointed at a time when hard freezes are highly unlikely.<br>
<b>4.</b> Varieties were evaluated for resistance to Scab (FHB) at the University of Illinois South Farm at Urbana, IL. To promote disease symptoms, we spread scabby corn kernels on the soil surface 3 weeks before heading and mist irrigated three times per day for 30 minutes. Ratings for each variety are based on expected vomitoxin levels under FHB epidemic conditions. Ratings are expressed on a 1 to 9 scale with 1 being the best (lowest vomitoxin) and 9 being the worst (highest vomitoxin). Varieties with ratings 4.3 or lower are considered at least moderately resistant to FHB. MR = Moderately Resistant, which is the highest level of resistance available, M = Intermediate level of resistance, MS = Moderately susceptible, S = Susceptible. Higher levels of resistance provide greater control of the disease. The best control is obtained when moderate resistance is combined with a fungicide at flowering.
")

# ------------------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------------------

ui <- fluidPage(
  
  useShinyjs(),
  div(style = "display: none;", icon("star")),
  
  theme = bs_theme(
    primary = "#13294B",   # Illini Blue
    secondary = "#FF5F05"  # Illini Orange
  ),
  
  # Page-level CSS and browser-side helpers for fixed layout, screenshot export,
  # sidebar behavior, and idle-session refresh.
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"),
    # Format and font
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
        width: 24px;
        height: 36px;
        font-size: 26px !important;
        display: flex !important;
        align-items: center;
        justify-content: center;
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

      /* More compact table formatting used only in Detailed view */
      body.detailed-view table.dataTable thead th {
        font-size: 16px !important;
        padding: 3px 6px !important;
        line-height: 1 !important;
      }

      body.detailed-view table.dataTable tbody td {
        padding: 1px 4px !important;
        line-height: 1.1 !important;
      }

      table.dataTable td.company-column,
      table.dataTable th.company-column,
      table.dataTable td.variety-column,
      table.dataTable th.variety-column {
        white-space: nowrap !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
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
      
      #table-loading {
        display: flex;
        position: fixed;
        inset: 60px 0 0 0;
        z-index: 15000;
        align-items: center;
        justify-content: center;
        min-height: 180px;
        background: #ffffff;
        font-size: 16px;
        color: #555555;
        pointer-events: none;
      }

      #table-container {
        visibility: hidden;
      }

      body.table-ready #table-container {
        visibility: visible;
      }

      body.table-ready #table-loading {
        display: none;
      }

      body:not(.table-ready) td.dataTables_empty {
        visibility: hidden !important;
      }

      /* Keep the loading overlay visible during startup. Hide only unfinished
         controls and the table instead of making the entire main container
         transparent, which can leave phones displaying a blank page. */
      body:not(.table-ready) .sidebar-panel,
      body:not(.table-ready) .toggle-rail,
      body:not(.table-ready) .mobile-filter-panel,
      body:not(.table-ready) #table-container {
        visibility: hidden !important;
        pointer-events: none !important;
      }

      body.table-ready .sidebar-panel,
      body.table-ready .toggle-rail,
      body.table-ready .mobile-filter-panel,
      body.table-ready #table-container {
        visibility: visible !important;
        pointer-events: auto !important;
      }

      /* Reserve maturity-filter space during startup to prevent flicker */
      
      /* Narrow mobile dropdowns for a cleaner layout */
      .mobile-filter-row-main .shiny-input-container {
        width: 85%;
        margin-left: auto;
        margin-right: auto;
      }

      .mobile-filter-row-search .shiny-input-container {
        width: 90%;
        margin-left: auto;
        margin-right: auto;
      }

      .mobile-filter-row-main .form-control,
      .mobile-filter-row-search .form-control {
        width: 100%;
      }

@media screen and (max-width: 900px) {
        body:not(.table-ready) .mobile-maturity-section {
          visibility: hidden !important;
          min-height: 76px !important;
        }

        body.table-ready .mobile-maturity-section {
          visibility: visible !important;
        }
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
      
      body.export-shot .main-container,
      body.export-shot .main-panel,
      body.export-shot .table-container,
      body.export-shot .dataTables_wrapper,
      body.export-shot .dataTables_scroll,
      body.export-shot .dataTables_scrollBody {
        height: auto !important;
        max-height: none !important;
        overflow: visible !important;
      }
      
      body.export-shot {
        overflow: visible !important;
      }

      /* Mobile-only controls are hidden on desktop */
      .mobile-only {
        display: none !important;
      }

      .desktop-only {
        display: block;
      }

      .mobile-title {
        display: none;
      }

      .desktop-title {
        display: inline;
      }

      /* Mobile layout for phones and small tablets */
      @media screen and (max-width: 900px) {
        html, body {
          height: 100%;
          overflow: hidden;
        }

        .mobile-only {
          display: flex !important;
        }

        .desktop-only {
          display: none !important;
        }

        #table-loading {
          inset: 60px 0 0 0;
        }

        .fixed-title {
          position: relative;
          height: 60px;
          min-height: 60px;
          padding: 5px 8px;
          gap: 6px;
          flex-wrap: nowrap;
        }

        .fixed-title h2 {
          font-size: 20px;
          line-height: 1.05;
          font-weight: 700;
          margin: 0;
          flex: 1;
          min-width: 0;
          white-space: normal;
          overflow: visible;
          text-overflow: clip;
        }

        .fixed-title .desktop-title {
          display: none !important;
        }

        .fixed-title .mobile-title {
          display: inline !important;
        }

        .fixed-title .btn {
          font-size: 12px !important;
          padding: 2px 6px !important;
          white-space: nowrap;
        }

        .main-container {
          margin-top: 0;
          height: calc(100vh - 60px);
          height: calc(100dvh - 60px);
          display: flex;
          flex-direction: column;
          overflow: hidden;
        }

        .sidebar-panel,
        .toggle-rail {
          display: none !important;
        }

        .mobile-filter-panel {
          width: 100%;
          height: min(72vh, 430px);
          height: min(72dvh, 430px);
          max-height: calc(100vh - 76px);
          max-height: calc(100dvh - 76px);
          min-height: 0;
          overflow-y: scroll;
          overflow-x: hidden;
          -webkit-overflow-scrolling: touch;
          overscroll-behavior-y: contain;
          touch-action: pan-y;
          scrollbar-gutter: stable;
          padding: 8px 10px 12px 10px;
          border-bottom: 1px solid #cccccc;
          background: #f4f6f8;
          flex-direction: column;
          gap: 8px;
          flex-shrink: 0;
        }

        .mobile-filter-card {
          width: 100%;
          display: block !important;
          padding: 8px 10px 10px 10px;
          border: 1px solid #d9dee4;
          border-radius: 8px;
          background: #ffffff;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
        }

        .mobile-card-title {
          display: block !important;
          width: 100%;
          margin: 0 0 7px 0;
          padding-bottom: 5px;
          border-bottom: 1px solid #e3e6e9;
          color: #13294B;
          font-size: 14px;
          font-weight: 700;
          line-height: 1.2;
        }

        body.mobile-filters-hidden .mobile-filter-panel {
          display: none !important;
        }

        .mobile-show-filters {
          display: none !important;
        }

        body.mobile-filters-hidden .mobile-show-filters {
          display: inline-flex !important;
        }

        .mobile-hide-filters-btn {
          position: sticky;
          bottom: 0;
          z-index: 20;
          width: 100%;
          min-height: 40px !important;
          margin-top: 2px;
          justify-content: center;
          background: #13294B !important;
          border-color: #13294B !important;
          color: #ffffff !important;
          font-weight: 600;
          flex-shrink: 0;
          box-shadow: 0 -4px 10px rgba(244, 246, 248, 0.95);
        }

        .mobile-filter-row {
          width: 100%;
          display: grid !important;
          gap: 8px;
          align-items: end;
        }

        .mobile-filter-row-main {
          grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        }

        .mobile-filter-row-display-options {
          grid-template-columns: minmax(0, 1.35fr) minmax(0, 1fr);
          margin-top: 8px;
          align-items: center;
        }

        .mobile-filter-row-display-options .shiny-options-group {
          flex-wrap: nowrap !important;
        }

        .mobile-filter-row-display-options .checkbox {
          padding-top: 18px;
        }

        .mobile-filter-row-search {
          grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        }

        .mobile-filter-row-categories {
          grid-template-columns: 1fr 1fr;
          align-items: start;
          gap: 18px;
        }

        .mobile-filter-section {
          width: 100%;
          display: block !important;
        }

        .mobile-section-header {
          display: flex !important;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
          width: 100%;
          margin-bottom: 2px;
          padding-bottom: 3px;
          border-bottom: 1px solid #d8d8d8;
          color: #13294B;
          font-size: 13px;
          font-weight: 600;
        }

        .mobile-section-header .btn {
          min-height: 28px !important;
          padding: 2px 7px !important;
          font-size: 11px !important;
        }

        .mobile-filter-panel .form-group,
        .mobile-filter-panel .checkbox,
        .mobile-filter-panel .radio {
          margin: 0 !important;
          min-width: 0;
        }

        .mobile-filter-panel label,
        .mobile-filter-panel .control-label {
          font-size: 12px !important;
          margin-bottom: 2px !important;
        }

        .mobile-filter-panel select {
          width: 100%;
          min-height: 34px;
          font-size: 14px;
          padding: 3px 24px 3px 6px;
        }

        .mobile-filter-panel .btn {
          min-height: 34px;
          padding: 3px 8px !important;
          font-size: 12px !important;
        }

        .mobile-filter-panel .shiny-options-group {
          display: flex !important;
          flex-wrap: wrap !important;
          gap: 3px 10px;
        }

        .mobile-filter-row-categories .control-label {
          display: block;
          width: 100%;
          color: #13294B;
          font-size: 13px !important;
          font-weight: 600;
          margin-bottom: 5px !important;
        }

        #mobile_search_btn.mobile-apply-btn {
          background: #13294B !important;
          border-color: #13294B !important;
          color: #ffffff !important;
          font-weight: 600;
        }

        #mobile_clear_search.mobile-clear-btn {
          background: #e9ecef !important;
          border-color: #ced4da !important;
          color: #333333 !important;
        }

        .mobile-maturity-section {
          display: block !important;
          width: 100% !important;
          min-height: 76px;
          padding: 2px 4px 8px 4px;
          margin-bottom: 14px;
          overflow: visible !important;
        }

        .mobile-maturity-section .shiny-input-container,
        .mobile-maturity-section .form-group,
        .mobile-maturity-section .irs,
        .mobile-maturity-section .irs--shiny {
          display: block !important;
          width: 100% !important;
          max-width: none !important;
          visibility: visible !important;
        }

        .mobile-maturity-section #mobile_refresh_sliders {
          display: block;
          margin-top: 12px;
          min-height: 30px;
          width: auto;
        }

        .mobile-filter-row-categories {
          margin-top: 16px;
          padding-top: 4px;
        }

        .mobile-filter-panel .irs {
          width: 100% !important;
        }

        /* Keep the table initialized and measurable while filters are open,
           but move the entire data area far off-screen so no part is visible. */
        .main-panel {
          display: flex !important;
          position: absolute !important;
          left: -100000px !important;
          top: 0;
          width: 100% !important;
          height: calc(100dvh - 60px);
          min-height: 0;
          flex: 1;
          overflow: hidden;
          opacity: 0 !important;
          pointer-events: none !important;
        }

        body.mobile-filters-hidden .main-panel {
          display: flex !important;
          position: relative !important;
          left: 0 !important;
          top: auto !important;
          width: 100% !important;
          height: auto !important;
          opacity: 1 !important;
          visibility: visible !important;
          pointer-events: auto !important;
          z-index: 1;
        }

        /* Hide only the loading overlay while filters are open. DataTables stays
           mounted off-screen so it can initialize normally. */
        body:not(.mobile-filters-hidden) #table-loading {
          display: none !important;
        }

        .table-container,
        .dataTables_wrapper,
        .dataTables_scroll {
          height: 100% !important;
          min-height: 0;
        }

        .dataTables_scrollBody {
          height: auto !important;
          max-height: calc(100vh - 345px) !important;
          flex: 1;
        }

        body.mobile-filters-hidden .dataTables_scrollBody {
          max-height: calc(100vh - 70px) !important;
        }

        table.dataTable thead th {
          font-size: 12px !important;
          padding: 2px 4px !important;
          padding-right: 4px !important;
          line-height: 1 !important;
          text-align: center !important;
          cursor: default !important;
          pointer-events: none !important;
        }

        /* Hide DataTables sorting arrows on mobile */
        table.dataTable thead .sorting:before,
        table.dataTable thead .sorting:after,
        table.dataTable thead .sorting_asc:before,
        table.dataTable thead .sorting_asc:after,
        table.dataTable thead .sorting_desc:before,
        table.dataTable thead .sorting_desc:after,
        table.dataTable thead .sorting_disabled:before,
        table.dataTable thead .sorting_disabled:after {
          display: none !important;
          content: none !important;
        }

        table.dataTable tbody td {
          font-size: 11px !important;
          padding: 1px 3px !important;
          line-height: 1.05 !important;
        }

        table.dataTable td.company-column,
        table.dataTable th.company-column {
          max-width: 150px !important;
        }

        table.dataTable td.variety-column,
        table.dataTable th.variety-column {
          max-width: 160px !important;
        }
      }

      @media screen and (max-width: 700px) {
        .fixed-title {
          height: 56px;
          min-height: 56px;
        }

        .fixed-title h2 {
          font-size: 17px;
        }

        .main-container {
          height: calc(100vh - 56px);
          height: calc(100dvh - 56px);
        }

        .mobile-filter-row-main,
        .mobile-filter-row-display-options,
        .mobile-filter-row-search,
        .mobile-filter-row-categories {
          grid-template-columns: 1fr;
        }

        .mobile-filter-row-display-options .checkbox {
          padding-top: 2px;
        }

        .mobile-filter-panel {
          height: calc(100vh - 56px);
          height: calc(100dvh - 56px);
          max-height: calc(100vh - 56px);
          max-height: calc(100dvh - 56px);
        }

        .dataTables_scrollBody {
          max-height: calc(100vh - 95px) !important;
          max-height: calc(100dvh - 95px) !important;
        }

        body.mobile-filters-hidden .main-panel {
          height: calc(100vh - 56px);
          height: calc(100dvh - 56px);
        }
      }
    ")),
    
    # Screenshot download
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.addEventListener('click', function(e) {
          var btn = e.target.closest('#download_png');
          if (!btn) return;
    
          var target = document.querySelector('.main-container');
          if (!target) return;
    
          document.body.classList.add('export-shot');
    
          setTimeout(function() {
            html2canvas(target, {
              backgroundColor: '#ffffff',
              useCORS: true,
              scale: 2,
              windowWidth: target.scrollWidth,
              windowHeight: target.scrollHeight
            }).then(function(canvas) {
              var link = document.createElement('a');
              link.download = 'wheat_tool_screenshot.png';
              link.href = canvas.toDataURL('image/png');
              link.click();
    
              document.body.classList.remove('export-shot');
            }).catch(function(err) {
              console.error(err);
              document.body.classList.remove('export-shot');
            });
          }, 300);
        });
      });
    ")),
    
    # Allow the server to actively restore regional-yield sorting after the
    # user checks Always sort by yield.
    tags$script(HTML("
      Shiny.addCustomMessageHandler('sortTableByColumn', function(message) {
        var tableElement = document.querySelector('#table table.dataTable');
        if (!tableElement || !$.fn.dataTable.isDataTable(tableElement)) return;

        var dt = $(tableElement).DataTable();
        dt.order([[message.column, message.direction || 'desc']]).draw();
      });
    ")),
    
    # Loading-state watchdog: the server normally reveals the interface as soon
    # as valid data are available. This final fallback prevents an indefinite
    # loading screen if a browser-side DataTables event is missed.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(function() {
          if (!document.body.classList.contains('table-ready')) {
            document.body.classList.add('table-ready');
            window.dispatchEvent(new Event('resize'));
          }
        }, 12000);
      });
    ")),
    
    # Inactivity detection and reloading
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
  
  # main page
  div(class = "fixed-title",
      h2(
        span(class = "desktop-title", "2026 Illinois Wheat Variety Data"),
        span(class = "mobile-title", "2026 Illinois Wheat Variety Data")
      ),
      
      popover(
        actionButton("legend_btn", label = NULL, "Instructions",
                     style = "margin-left: 10px;"),
        title = "Instructions",
        placement = "bottom",
        instruction_text,
        options = list(html = TRUE, trigger = "click")
      ),
      div(
        class = "desktop-only",
        style = "margin-left: 15px;",
        actionButton("download_png", "Download Screenshot")
      ),
      actionButton(
        "showMobileFilters",
        "Show Filters",
        icon = icon("filter"),
        class = "mobile-only mobile-show-filters",
        title = "Return to the filter panel"
      )
  ),
  
  div(class = "main-container",
      
      div(
        class = "mobile-filter-panel mobile-only",
        
        div(
          class = "mobile-filter-card",
          div(class = "mobile-card-title", "Display"),
          div(
            class = "mobile-filter-row mobile-filter-row-main",
            selectInput(
              "mobile_region",
              "Region:",
              choices = c("North", "South"),
              selected = "North",
              selectize = FALSE
            ),
            selectInput(
              "mobile_smry",
              "Summary:",
              choices = c("Compact", "Detailed"),
              selected = "Compact",
              selectize = FALSE
            )
          ),
          div(
            class = "mobile-filter-row mobile-filter-row-display-options",
            radioButtons(
              "mobile_value_display",
              "Values:",
              choices = c("Actual" = "raw", "% mean" = "percent"),
              selected = "raw",
              inline = TRUE
            ),
            checkboxInput(
              "mobile_sort_by_yield",
              "Sort by yield",
              value = FALSE
            )
          )
        ),
        
        div(
          class = "mobile-filter-card",
          uiOutput("mobile_maturity_title"),
          div(
            class = "mobile-filter-section mobile-maturity-section",
            uiOutput("mobile_md_slider_ui")
          )
        ),
        
        div(
          class = "mobile-filter-card",
          div(class = "mobile-card-title", "Search"),
          div(
            class = "mobile-filter-row mobile-filter-row-search",
            selectInput(
              "mobile_company",
              "Company:",
              choices = c("All companies" = ""),
              selected = "",
              selectize = FALSE
            ),
            selectInput(
              "mobile_variety",
              "Variety:",
              choices = c("All varieties" = ""),
              selected = "",
              selectize = FALSE
            ),
            div()
          )
        ),
        
        div(
          class = "mobile-filter-card",
          uiOutput("mobile_trait_filter_title"),
          div(
            class = "mobile-filter-row mobile-filter-row-categories",
            uiOutput("mobile_scab_checkboxes"),
            uiOutput("mobile_jtd_checkboxes")
          )
        ),
        actionButton(
          "hideMobileFilters",
          "Hide Filters and Show Data",
          icon = icon("table"),
          class = "mobile-hide-filters-btn",
          title = "Hide the filter panel and show the data table"
        )
      ),
      
      div(class = "sidebar-panel",
          selectInput("region", "Region:", choices = c("North", "South"), selected = "North", selectize = FALSE),
          selectInput("smry", "Summary type:", choices = c("Compact", "Detailed"), selected = "Compact", selectize = FALSE),
          
          # Switch display
          div(
            title = "For Grain Yield and Test Weight, switch between raw data and percentages of the mean within each study/site. Mean value is set to 100%.",
            radioButtons(
              "value_display",
              "Display values:",
              choices = c("Actual values" = "raw", "Percentage of mean" = "percent"),
              selected = "raw",
              inline = TRUE
            )
          ),
          
          checkboxInput(
            "sort_by_yield",
            "Always sort by yield",
            value = FALSE
          ),
          
          # Maturity slider
          div(
            title = "A higher value indicates a later maturity, a lower value indicates an earlier maturity. A value of 0 indicates the earliest maturing variety in the region.",
            uiOutput("md_slider_ui")
          ),
          actionButton("refresh_sliders", "Refresh Slider"),
          br(), br(),
          
          
          # Desktop search
          div(
            class = "desktop-only",
            selectizeInput(
              "search_text",
              label = "Search (companies / varieties):",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Type companies / varieties"
              )
            ),
            div(
              style = "display: flex; gap: 8px; align-items: center;",
              div(
                title = "Only companies and varieties matching the currently applied filters are shown.",
                actionButton("search_btn", "Search")
              ),
              actionButton("clear_search", "Clear Search")
            ),
            br()
          ),
          
          # Star
          div(
            style = "display: flex; gap: 8px; align-items: center;",
            actionButton("toggle_star_all", "Star/Unstar All",
                         title = "Star/unstar all currently visible varieties."),
            actionButton("show_starred", "Show/Hide Starred",
                         title = "Click again to return to the full table.")
          ),
          br(),
          
          
          uiOutput("scab_checkboxes"),
          uiOutput("jtd_checkboxes"),
          uiOutput("site_checkboxes")
      ),
      
      div(class = "toggle-rail",
          actionButton("toggleSidebar", NULL, icon = HTML("&lsaquo;"), class = "sidebar-arrow")
      ),
      
      div(
        class = "main-panel",
        div(
          id = "table-loading",
          icon("spinner", class = "fa-spin"),
          span(style = "margin-left: 8px;", "Loading table...")
        ),
        div(
          id = "table-container",
          class = "table-container",
          DTOutput("table")
        )
      )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Session-local state for search text, starred varieties, and starred-only mode.
  # These values reset when the Shiny session restarts.
  search_term <- reactiveVal("")
  starred <- reactiveVal(character(0))
  show_starred_only <- reactiveVal(FALSE)
  mobile_filters_visible <- reactiveVal(TRUE)
  table_initialized <- reactiveVal(FALSE)
  structural_rebuild <- reactiveVal(FALSE)
  rendered_table_columns <- reactiveVal(character(0))
  
  # Use mobile trait-filter inputs directly whenever they are available.
  effective_scab_filter <- reactive({
    input$mobile_show_scab %||%
      input$show_scab %||%
      c("S", "MS", "M", "MR", "pending")
  })
  
  effective_jointing_filter <- reactive({
    input$mobile_show_jtd %||%
      input$show_jtd %||%
      c("E", "M/E", "M", "M/L", "L")
  })
  
  
  
  # Sort by the regional-average grain-yield column when yield sorting is active.
  apply_yield_sort <- function(df) {
    if (!isTRUE(input$sort_by_yield)) {
      return(df)
    }
    
    yield_col <- grep(
      "^Grain\\.Yield_.*RegionalAverage$",
      names(df),
      value = TRUE
    )
    
    if (length(yield_col) != 1) {
      return(df)
    }
    
    df[
      order(
        suppressWarnings(as.numeric(df[[yield_col]])),
        decreasing = TRUE,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
  }
  
  # ------------------------------------------------------------------------------
  # Data Pipeline
  #
  # Start from the long-format source data, normalize phenology values, optionally
  # convert target traits to percentages, then reshape into the wide table consumed
  # by the UI.
  # ------------------------------------------------------------------------------
  table_data <- reactive({
    # pipe operators replace repeated assignments
    raw_df <- WheatOVT26 |>
      normalize_maturity() |>
      normalize_heading()
    
    if (identical(input$value_display, "percent")) {
      raw_df <- normalize_percentage(raw_df)
    }
    
    raw_df |>
      prepare_table(Region = input$region, smryType = input$smry) |>
      scab_res_level() |>
      jointing_level()
  })
  
  # Search Choices
  observeEvent(table_data(), {
    df <- table_data()
    req(df)
    
    choices <- sort(unique(c(as.character(df$company), as.character(df$number))))
    
    current_selected <- isolate(input$search_text %||% character(0))
    current_selected <- intersect(current_selected, choices)
    
    updateSelectizeInput(
      session, "search_text",
      choices = choices,
      selected = current_selected,
      server = TRUE
    )
    
    company_choices <- sort(unique(as.character(df$company)))
    variety_choices <- sort(unique(as.character(df$number)))
    
    current_company <- isolate(input$mobile_company %||% "")
    current_variety <- isolate(input$mobile_variety %||% "")
    
    if (!current_company %in% company_choices) current_company <- ""
    if (!current_variety %in% variety_choices) current_variety <- ""
    
    updateSelectInput(
      session,
      "mobile_company",
      choices = c("All companies" = "", company_choices),
      selected = current_company
    )
    
    updateSelectInput(
      session,
      "mobile_variety",
      choices = c("All varieties" = "", variety_choices),
      selected = current_variety
    )
  }, ignoreInit = FALSE)
  
  # Slider debounce
  # On mobile, filter directly from the visible mobile slider. The hidden
  # desktop slider remains the fallback for desktop use.
  effective_md_range <- reactive({
    if (length(input$mobile_md_range) == 2 &&
        all(is.finite(input$mobile_md_range))) {
      input$mobile_md_range
    } else {
      input$md_range
    }
  })
  
  md_range_debounced <- reactive(effective_md_range()) |> debounce(300)
  
  row_filters <- reactive({
    list(
      # Mobile filters are read directly; desktop inputs remain the fallback.
      jtd = effective_jointing_filter(),
      scab = effective_scab_filter()
    )
  })
  
  # Basic filter: apply slider/row filtering, retaining all site columns
  data_filtered_base <- reactive({
    df <- table_data()
    req(df)
    
    # During startup the dynamically generated slider can briefly be NULL or
    # incomplete. Do not apply the maturity filter until a valid two-value
    # range is available; otherwise the app can initialize with zero rows.
    maturity_range <- md_range_debounced()
    if (length(maturity_range) == 2 && all(is.finite(maturity_range))) {
      df <- filter_by_maturity_range(df, input$region, maturity_range)
    }
    
    filters <- row_filters()
    
    jtd_col <- grep("^Jointing\\.Category", names(df), value = TRUE)
    if (length(jtd_col) == 1 && length(filters$jtd) > 0) {
      all_jtd_levels <- c("E", "M/E", "M", "M/L", "L")
      keep_rows <- df[[jtd_col]] %in% filters$jtd
      
      # Preserve varieties with no jointing rating when all categories are
      # selected. Otherwise Detailed view can lose varieties that are present
      # in Compact view solely because their jointing value is missing.
      if (setequal(filters$jtd, all_jtd_levels)) {
        keep_rows <- keep_rows | is.na(df[[jtd_col]]) | df[[jtd_col]] == ""
      }
      
      df <- df[keep_rows, , drop = FALSE]
    }
    
    scab_col <- grep("^Scab\\.Category", names(df), value = TRUE)
    if (length(scab_col) == 1 && length(filters$scab) > 0) {
      df <- df[df[[scab_col]] %in% filters$scab, , drop = FALSE]
    }
    
    df
  })
  
  # Search filter
  # Only Show/Hide Starred re-calculate data_filtered_rows()
  data_filtered_search <- reactive({
    filter_by_search(data_filtered_base(), search_term())
  })
  
  current_visible_varieties <- reactive({
    df <- apply_yield_sort(display_data())
    req(df)
    unique(as.character(df$number))
  })
  
  # Star filter
  data_filtered_rows <- reactive({
    if (isTRUE(show_starred_only())) {
      filter_by_starred(
        data_filtered_search(),
        starred(),
        TRUE
      )
    } else {
      data_filtered_search()
    }
  })
  
  current_n <- reactive({
    df <- data_filtered_rows()   # include search / slider / scab / jtd / starred
    req(df)
    nrow(df)
  })
  
  # Apply column-level site visibility after row filters.
  display_data <- reactive({
    df <- data_filtered_rows()
    
    all_sites <- switch(
      input$region,
      South = c("Addieville", "Elkville", "StPeter"),
      North = c("Hampshire", "Perry", "Urbana")
    )
    
    # The mobile interface has no location checkboxes. When the hidden desktop
    # site input has not initialized, show every location rather than treating
    # NULL as though all locations were unchecked.
    selected_sites <- input$show_site
    
    if (is.null(selected_sites)) {
      selected_sites <- all_sites
    }
    
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
  
  
  # Reveal the app from the server as soon as a valid table is available.
  # This avoids relying entirely on a browser-side DataTables event.
  observeEvent(display_data(), {
    df_ready <- display_data()
    
    if (!is.null(df_ready) && ncol(df_ready) > 0 && nrow(df_ready) > 0) {
      session$onFlushed(
        function() {
          addClass(selector = "body", class = "table-ready")
          table_initialized(TRUE)
          runjs(
            "setTimeout(function(){
               $(window).trigger('resize');
               if ($.fn.dataTable) {
                 $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
               }
             }, 100);"
          )
        },
        once = TRUE
      )
    }
  }, ignoreInit = FALSE)
  
  # Rebuild table when Region/Smry/column changes/hidden/display mode changes
  rebuild_trigger <- reactiveVal(0)
  
  observeEvent(
    list(input$region, input$smry, input$show_site, input$value_display),
    {
      structural_rebuild(TRUE)
      rendered_table_columns(character(0))
      rebuild_trigger(rebuild_trigger() + 1)
    },
    ignoreInit = FALSE,
    priority = 200
  )
  
  # Cache boundary_cols calculation
  boundary_cols <- reactive({
    df <- display_data()
    data_cols <- names(df)[!names(df) %in% c("star", "company", "number")]
    if (length(data_cols) == 0) return(NULL)
    parsed <- do.call(rbind, strsplit(data_cols, "_"))
    study <- parsed[, 2]
    which(!duplicated(study, fromLast = TRUE)) + 2
  })
  
  
  
  observeEvent(input$smry, {
    if (identical(input$smry, "Detailed")) {
      addClass(selector = "body", class = "detailed-view")
    } else {
      removeClass(selector = "body", class = "detailed-view")
    }
    
    runjs("setTimeout(function(){ $(window).trigger('resize'); },150);")
  }, ignoreInit = FALSE)
  
  resize_mobile_table <- function() {
    runjs(
      "setTimeout(function(){
         $(window).trigger('resize');
         if ($.fn.dataTable) {
           $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
         }
       }, 250);"
    )
  }
  
  observeEvent(input$hideMobileFilters, {
    mobile_filters_visible(FALSE)
    addClass(selector = "body", class = "mobile-filters-hidden")
    addClass(selector = "body", class = "table-ready")
    
    runjs(
      paste0(
        "document.body.classList.add('mobile-filters-hidden');",
        "document.body.classList.add('table-ready');",
        "setTimeout(function(){",
        "  $(window).trigger('resize');",
        "  try {",
        "    if ($.fn.dataTable) {",
        "      var tables = $.fn.dataTable.tables({api: true});",
        "      tables.columns.adjust().draw(false);",
        "      tables.tables().every(function(){",
        "        try {",
        "          var dt = $(this).DataTable();",
        "          dt.columns.adjust().draw(false);",
        "          if (dt.fixedColumns && dt.fixedColumns().relayout) {",
        "            dt.fixedColumns().relayout();",
        "          }",
        "        } catch(e) {}",
        "      });",
        "    }",
        "  } catch(e) {}",
        "}, 300);"
      )
    )
  })
  
  observeEvent(input$showMobileFilters, {
    mobile_filters_visible(TRUE)
    removeClass(selector = "body", class = "mobile-filters-hidden")
    
    runjs(
      paste0(
        "document.body.classList.remove('mobile-filters-hidden');",
        "setTimeout(function(){",
        "  $(window).trigger('resize');",
        "}, 150);"
      )
    )
  })
  
  observeEvent(
    list(
      input$region,
      input$smry,
      input$value_display,
      input$sort_by_yield,
      input$md_range,
      input$show_scab,
      input$show_jtd,
      input$show_site
    ),
    {
      updateSelectInput(session, "mobile_region", selected = input$region)
      
      updateRadioButtons(session, "mobile_value_display", selected = input$value_display)
      updateCheckboxInput(
        session,
        "mobile_sort_by_yield",
        value = isTRUE(input$sort_by_yield)
      )
      
      if (!is.null(input$md_range)) {
        updateSliderInput(session, "mobile_md_range", value = input$md_range)
      }
      if (!is.null(input$show_scab)) {
        updateCheckboxGroupInput(session, "mobile_show_scab", selected = input$show_scab)
      }
      if (!is.null(input$show_jtd)) {
        updateCheckboxGroupInput(session, "mobile_show_jtd", selected = input$show_jtd)
      }
    },
    ignoreInit = FALSE
  )
  
  observeEvent(input$mobile_region, {
    req(input$mobile_region)
    if (!identical(input$region, input$mobile_region)) {
      updateSelectInput(session, "region", selected = input$mobile_region)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_smry, {
    req(input$mobile_smry)
    
    if (!identical(input$smry, input$mobile_smry)) {
      updateSelectInput(
        session,
        "smry",
        selected = input$mobile_smry
      )
    }
  }, ignoreInit = TRUE, priority = 300)
  
  observeEvent(input$mobile_value_display, {
    req(input$mobile_value_display)
    if (!identical(input$value_display, input$mobile_value_display)) {
      updateRadioButtons(
        session,
        "value_display",
        selected = input$mobile_value_display
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_sort_by_yield, {
    mobile_sort_on <- isTRUE(input$mobile_sort_by_yield)
    
    if (!identical(isTRUE(input$sort_by_yield), mobile_sort_on)) {
      updateCheckboxInput(
        session,
        "sort_by_yield",
        value = mobile_sort_on
      )
    }
  }, ignoreInit = TRUE)
  
  # If the user manually sorts a non-yield column, turn off the persistent
  # yield-sort option on both the desktop and mobile controls.
  observeEvent(input$manual_non_yield_sort, {
    if (isTRUE(input$sort_by_yield)) {
      updateCheckboxInput(session, "sort_by_yield", value = FALSE)
      updateCheckboxInput(session, "mobile_sort_by_yield", value = FALSE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_company, {
    company <- input$mobile_company %||% ""
    variety <- input$mobile_variety %||% ""
    terms <- c(company, variety)
    terms <- terms[nzchar(terms)]
    search_term(if (length(terms) == 0) "" else terms)
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_variety, {
    company <- input$mobile_company %||% ""
    variety <- input$mobile_variety %||% ""
    terms <- c(company, variety)
    terms <- terms[nzchar(terms)]
    search_term(if (length(terms) == 0) "" else terms)
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_md_range, {
    req(input$mobile_md_range)
    
    if (length(input$mobile_md_range) == 2 &&
        !identical(input$md_range, input$mobile_md_range)) {
      updateSliderInput(
        session,
        "md_range",
        value = input$mobile_md_range
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_show_scab, {
    req(input$mobile_show_scab)
    
    if (!identical(input$show_scab, input$mobile_show_scab)) {
      updateCheckboxGroupInput(
        session,
        "show_scab",
        selected = input$mobile_show_scab
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mobile_show_jtd, {
    req(input$mobile_show_jtd)
    
    if (!identical(input$show_jtd, input$mobile_show_jtd)) {
      updateCheckboxGroupInput(
        session,
        "show_jtd",
        selected = input$mobile_show_jtd
      )
    }
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$search_btn, {
    search_term(trimws(input$search_text %||% ""))
  })
  
  observeEvent(input$clear_search, {
    search_term("")
    updateSelectizeInput(session, "search_text", selected = character(0))
  })
  
  # Starred varieties are tracked by variety number and updated from both the main
  # table and the fixed-column clone created by DataTables.
  # Star/Unstar
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
        star = ifelse(df$number %in% starred(),
                      '<i class="fa fa-star"></i>',
                      '<i class="fa fa-star-o"></i>'),
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
      star = ifelse(df$number %in% starred(),
                    '<i class="fa fa-star"></i>',
                    '<i class="fa fa-star-o"></i>'),
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
  
  observeEvent(input$mobile_refresh_sliders, {
    req(table_data())
    
    ranges <- get_maturity_range(table_data(), input$region)
    
    updateSliderInput(
      session,
      "md_range",
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = ranges$Maturity.Date
    )
    
    updateSliderInput(
      session,
      "mobile_md_range",
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = ranges$Maturity.Date
    )
  })
  
  
  # Update input based on region and/or summary type
  observeEvent(input$region, {
    req(table_data())
    
    search_term("")
    starred(character(0))
    show_starred_only(FALSE)
    
    updateSelectizeInput(session, "search_text", selected = character(0))
    updateSelectInput(session, "mobile_company", selected = "")
    updateSelectInput(session, "mobile_variety", selected = "")
    updateCheckboxGroupInput(
      session, "show_scab",
      selected = c("S", "MS", "M", "MR", 'pending')
    )
    updateCheckboxGroupInput(session, "show_jtd", selected = c("E", 'M/E', "M","M/L", "L"))
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
    updateSelectizeInput(session, "search_text", selected = character(0))
    updateSelectInput(session, "mobile_company", selected = "")
    updateSelectInput(session, "mobile_variety", selected = "")
    
    updateCheckboxGroupInput(
      session,
      "show_scab",
      selected = c("S", "MS", "M", "MR", "pending")
    )
    updateCheckboxGroupInput(
      session,
      "show_jtd",
      selected = c("E", "M/E", "M", "M/L", "L")
    )
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
  
  # ------------------------------------------------------------------------------
  # Dynamic Sidebar Controls
  #
  # Checkbox labels and maturity range controls are rebuilt from the current
  # filtered data so counts and slider limits remain accurate.
  # ------------------------------------------------------------------------------
  output$md_slider_ui <- renderUI({
    df <- data_filtered_rows()
    req(df)
    
    ranges <- get_maturity_range(table_data(), input$region)
    
    sliderInput(
      "md_range",
      label = paste0(
        "Filter by maturity (",
        nrow(df),
        " remaining; lower = earlier)"
      ),
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = isolate(input$md_range) %||% ranges$Maturity.Date
    )
  })
  
  output$scab_checkboxes <- renderUI({
    df <- table_data()
    req(df)
    
    scab_col <- grep("^Scab\\.Category", names(df), value = TRUE)
    if (length(scab_col) != 1) return(NULL)
    
    choices <- c("S", "MS", "M", "MR", "pending")
    counts <- table(df[[scab_col]])
    count_values <- setNames(integer(length(choices)), choices)
    matching <- intersect(names(counts), choices)
    count_values[matching] <- as.integer(counts[matching])
    labels <- paste0(choices, " (", count_values[choices], ")")
    
    checkboxGroupInput(
      "show_scab",
      "Choose scab resistance level (MR = Best, S= Worst):",
      choices = setNames(choices, labels),
      selected = isolate(input$show_scab) %||% choices,
      inline = TRUE
    )
  })
  
  output$jtd_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    df <- table_data()
    req(df)
    
    jtd_col <- grep("^Jointing\\.Category", names(df), value = TRUE)
    if (length(jtd_col) != 1) return(NULL)
    
    choices <- c("E", "M/E", "M", "M/L", "L")
    counts <- table(df[[jtd_col]])
    count_values <- setNames(integer(length(choices)), choices)
    matching <- intersect(names(counts), choices)
    count_values[matching] <- as.integer(counts[matching])
    labels <- paste0(choices, " (", count_values[choices], ")")
    
    checkboxGroupInput(
      "show_jtd",
      "Choose jointing categories:",
      choices = setNames(choices, labels),
      selected = isolate(input$show_jtd) %||% choices,
      inline = TRUE
    )
  })
  
  output$site_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    choices <- switch(
      input$region,
      South = c("Addieville", "Elkville", "StPeter"),
      North = c("Hampshire", "Perry", "Urbana")
    )
    
    checkboxGroupInput(
      "show_site",
      "Show test sites:",
      choices = choices,
      selected = isolate(input$show_site) %||% choices,
      inline = TRUE
    )
  })
  
  mobile_count_data <- reactive({
    df <- table_data()
    req(df)
    
    mobile_range <- input$mobile_md_range
    
    if (length(mobile_range) == 2 &&
        all(is.finite(mobile_range))) {
      df <- filter_by_maturity_range(
        df,
        input$region,
        mobile_range
      )
    }
    
    mobile_scab <- input$mobile_show_scab %||%
      c("S", "MS", "M", "MR", "pending")
    
    scab_col <- grep("^Scab\\.Category", names(df), value = TRUE)
    if (length(scab_col) == 1 && length(mobile_scab) > 0) {
      df <- df[df[[scab_col]] %in% mobile_scab, , drop = FALSE]
    }
    
    if (identical(input$smry, "Detailed")) {
      mobile_jtd <- input$mobile_show_jtd %||%
        c("E", "M/E", "M", "M/L", "L")
      
      jtd_col <- grep("^Jointing\\.Category", names(df), value = TRUE)
      if (length(jtd_col) == 1 && length(mobile_jtd) > 0) {
        all_jtd_levels <- c("E", "M/E", "M", "M/L", "L")
        keep_rows <- df[[jtd_col]] %in% mobile_jtd
        
        if (setequal(mobile_jtd, all_jtd_levels)) {
          keep_rows <- keep_rows | is.na(df[[jtd_col]]) | df[[jtd_col]] == ""
        }
        
        df <- df[keep_rows, , drop = FALSE]
      }
    }
    
    selected_company <- input$mobile_company %||% ""
    selected_variety <- input$mobile_variety %||% ""
    
    if (nzchar(selected_company)) {
      df <- df[
        as.character(df$company) == selected_company,
        ,
        drop = FALSE
      ]
    }
    
    if (nzchar(selected_variety)) {
      df <- df[
        as.character(df$number) == selected_variety,
        ,
        drop = FALSE
      ]
    }
    
    df
  })
  
  output$mobile_trait_filter_title <- renderUI({
    df <- mobile_count_data()
    req(df)
    
    div(
      class = "mobile-card-title",
      paste0(
        "Trait filters (",
        nrow(df),
        " remaining)"
      )
    )
  })
  
  output$mobile_maturity_title <- renderUI({
    df <- mobile_count_data()
    req(df)
    
    div(
      class = "mobile-card-title",
      paste0(
        "Filter by maturity (",
        nrow(df),
        " remaining; lower = earlier)"
      )
    )
  })
  
  output$mobile_md_slider_ui <- renderUI({
    req(table_data())
    ranges <- get_maturity_range(table_data(), input$region)
    
    sliderInput(
      "mobile_md_range",
      label = NULL,
      min = ranges$Maturity.Date[1],
      max = ranges$Maturity.Date[2],
      value = isolate(input$md_range) %||% ranges$Maturity.Date
    )
  })
  
  output$mobile_scab_checkboxes <- renderUI({
    df <- table_data()
    req(df)
    
    scab_col <- grep("^Scab\\.Category", names(df), value = TRUE)
    if (length(scab_col) != 1) return(NULL)
    
    choices <- c("S", "MS", "M", "MR", "pending")
    checkboxGroupInput(
      "mobile_show_scab",
      "Choose scab resistance level (MR = Best)",
      choices = choices,
      selected = isolate(input$show_scab) %||% choices,
      inline = TRUE
    )
  })
  
  output$mobile_jtd_checkboxes <- renderUI({
    req(input$smry == "Detailed")
    df <- table_data()
    req(df)
    
    jtd_col <- grep("^Jointing\\.Category", names(df), value = TRUE)
    if (length(jtd_col) != 1) return(NULL)
    
    choices <- c("E", "M/E", "M", "M/L", "L")
    checkboxGroupInput(
      "mobile_show_jtd",
      "Choose jointing categories",
      choices = choices,
      selected = isolate(input$show_jtd) %||% choices,
      inline = TRUE
    )
  })
  
  
  
  
  # These outputs begin inside a CSS-hidden mobile panel. Prevent Shiny from
  # suspending them so they are already rendered when a phone enters landscape.
  outputOptions(output, "site_checkboxes", suspendWhenHidden = FALSE)
  outputOptions(output, "mobile_trait_filter_title", suspendWhenHidden = FALSE)
  outputOptions(output, "mobile_maturity_title", suspendWhenHidden = FALSE)
  outputOptions(output, "mobile_md_slider_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "mobile_scab_checkboxes", suspendWhenHidden = FALSE)
  outputOptions(output, "mobile_jtd_checkboxes", suspendWhenHidden = FALSE)
  
  # ------------------------------------------------------------------------------
  # Table Rendering
  #
  # Build the display table, add the star column, attach grouped headers/tooltips,
  # configure DataTables behavior, and apply conditional formatting.
  # ------------------------------------------------------------------------------
  output$table <- renderDT({
    rebuild_trigger()
    
    # Row filters are updated through the DataTables proxy below. Isolating
    # display_data() prevents those changes from destroying and recreating the widget.
    df <- isolate(apply_yield_sort(display_data()))
    req(df)
    
    # A group of synchronized inputs can briefly produce an empty filtered
    # data frame during the first browser flush. Build the initial widget from
    # the unfiltered regional table instead of allowing DataTables to become
    # permanently initialized with "No data available in table".
    if (nrow(df) == 0 && !isTRUE(table_initialized())) {
      fallback_df <- isolate(table_data())
      if (!is.null(fallback_df) && nrow(fallback_df) > 0) {
        df <- apply_yield_sort(fallback_df)
      }
    }
    req(ncol(df) > 0)
    
    df <- cbind(
      star = ifelse(df$number %in% isolate(starred()),
                    '<i class="fa fa-star"></i>',
                    '<i class="fa fa-star-o"></i>'),
      df,
      stringsAsFactors = FALSE
    )
    
    # Save the exact schema used to build this DataTables widget.
    rendered_table_columns(names(df))
    
    yield_col <- grep(
      "^Grain\\.Yield_.*RegionalAverage$",
      names(df),
      value = TRUE
    )
    
    # DataTables uses zero-based column indexes.
    yield_col_index <- if (length(yield_col) == 1) {
      match(yield_col, names(df)) - 1L
    } else {
      integer(0)
    }
    
    col_defs <- list(
      list(targets = "_all", className = "dt-center"),
      list(targets = 0, orderable = FALSE),
      list(targets = 1, className = "dt-left company-column", width = "220px"),
      list(targets = 2, className = "dt-left variety-column", width = "220px")
    )
    
    bc <- isolate(boundary_cols())
    if (!is.null(bc)) {
      col_defs <- append(col_defs, list(list(targets = bc, className = "study-sep")))
    }
    
    value_cols <- grep("^(Grain\\.Yield|Test\\.Weight)_", names(df), value = TRUE)
    
    dt_options <- list(
      scrollY = "calc(100vh - 180px)",
      scrollX = TRUE,
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      paging = FALSE,
      dom = "t",
      fixedColumns = list(leftColumns = 3),
      columnDefs = col_defs
    )
    
    if (isTRUE(isolate(input$sort_by_yield)) && length(yield_col_index) == 1) {
      dt_options$order <- list(list(yield_col_index, "desc"))
    }
    
    dt <- datatable(
      df,
      container = make_tooltip_container(df),
      rownames = FALSE,
      escape = FALSE,
      extensions = c("FixedColumns"),
      callback = JS(
        "var dt = table;",
        "var revealWhenReady = function() {",
        "  document.body.classList.add('table-ready');",
        "  Shiny.setInputValue('table_client_initialized', Date.now(), {priority: 'event'});",
        "  setTimeout(function(){",
        "    $(window).trigger('resize');",
        "    try { dt.columns.adjust(); } catch (e) {}",
        "  }, 50);",
        "};",
        "dt.one('draw.dt', revealWhenReady);",
        "setTimeout(revealWhenReady, 250);",
        sprintf(
          paste0(
            "$(dt.table().header()).on('click.yieldSortOverride', 'th', function() {",
            "  if (window.matchMedia('(max-width: 900px)').matches) return;",
            "  var sortedCol = dt.column(this).index();",
            "  var yieldSortChecked = $('#sort_by_yield').prop('checked');",
            "  if (yieldSortChecked && sortedCol !== %s) {",
            "    Shiny.setInputValue('manual_non_yield_sort', Date.now(), {priority: 'event'});",
            "  }",
            "});"
          ),
          if (length(yield_col_index) == 1) yield_col_index else -1L
        ),
        "var bindStarClick = function(tbl) {",
        "  $(tbl.table().container()).on('click', 'tbody td:first-child', function() {",
        "    var rowData = tbl.row($(this).closest('tr')).data();",
        "    if (!rowData) return;",
        "    var key = rowData[2];",
        "    var isStar = $(this).find('i').hasClass('fa-star');",
        "    if (isStar) {",
        "      $(this).html('<i class=\"fa fa-star-o\"></i>');",
        "    } else {",
        "      $(this).html('<i class=\"fa fa-star\"></i>');",
        "    }",
        "    Shiny.setInputValue('toggle_star', key, {priority: 'event'});",
        "  });",
        "};",
        "bindStarClick(dt);",
        "setTimeout(function() {",
        "  $('.DTFC_Cloned').each(function() {",
        "    $(this).on('click', 'tbody td:first-child', function() {",
        "      var rowText = $(this).closest('tr').find('td').eq(2).text().trim();",
        "      var isStar = $(this).find('i').hasClass('fa-star');",
        "      if (isStar) {",
        "        $(this).html('<i class=\"fa fa-star-o\"></i>');",
        "      } else {",
        "        $(this).html('<i class=\"fa fa-star\"></i>');",
        "      }",
        "      Shiny.setInputValue('toggle_star', rowText, {priority: 'event'});",
        "    });",
        "  });",
        "}, 300);"
      ),
      options = dt_options
    )
    
    table_font_size <- if (identical(input$smry, "Detailed")) "14px" else "16px"
    table_padding <- if (identical(input$smry, "Detailed")) "1px 4px" else "2px 4px"
    
    dt <- dt |>
      formatStyle(
        columns = names(df),
        fontSize = table_font_size,
        padding = table_padding
      )
    
    # Percent mode highlights values above 100. Raw mode highlights values above the
    # current column mean from the unsearched table for the selected region/summary.
    if (identical(input$value_display, "percent")) {
      dt <- dt |>
        formatStyle(
          columns = value_cols,
          color = styleInterval(c(100), c("#000000", "#d55e00"))
        ) |>
        formatString(value_cols, suffix = "%")
    } else {
      threshold_source <- table_data()
      threshold_cols <- intersect(value_cols, names(threshold_source))
      
      if (length(threshold_cols) > 0) {
        thresholds <- sapply(threshold_cols, function(col) {
          mean(as.numeric(threshold_source[[col]]), na.rm = TRUE)
        })
        
        for (col in threshold_cols) {
          threshold <- thresholds[[col]]
          
          if (is.finite(threshold)) {
            dt <- dt |>
              formatStyle(
                columns = col,
                color = styleInterval(c(threshold), c("#000000", "#d55e00"))
              )
          }
        }
      }
    }
    
    session$onFlushed(
      function() {
      },
      once = TRUE
    )
    
    dt
  })
  
  
  # Keep the table rendering while the mobile filters are open so it is ready
  # immediately when the user chooses to show the data.
  outputOptions(output, "table", suspendWhenHidden = FALSE)
  
  # Sidebar collapse
  collapsed <- reactiveVal(FALSE)
  observeEvent(input$toggleSidebar, {
    collapsed(!collapsed())
    if (collapsed()) {
      addClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "toggleSidebar", icon = HTML("&rsaquo;"))
    } else {
      removeClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "toggleSidebar", icon = HTML("&lsaquo;"))
    }
    
    # recalculate the dt width to prevent header and column misalignment
    runjs("setTimeout(function(){ $(window).trigger('resize'); },300);")
    
  })
  
  # Mark the table as initialized only after the browser has created the
  # DataTables widget. This prevents startup-only empty states from replacing
  # the valid initial data.
  observeEvent(input$table_client_initialized, {
    table_initialized(TRUE)
    structural_rebuild(FALSE)
    addClass(selector = "body", class = "table-ready")
  }, ignoreInit = TRUE)
  
  # Proxy of lightweight table refresh for row filters and star actions.
  proxy <- dataTableProxy("table")
  
  observeEvent(
    list(
      md_range_debounced(),
      input$mobile_md_range,
      input$show_scab,
      input$show_jtd,
      input$mobile_show_scab,
      input$mobile_show_jtd,
      input$mobile_sort_by_yield,
      search_term(),
      input$toggle_star_all,
      input$show_starred,
      input$sort_by_yield
    ),
    {
      # Compact/Detailed, North/South, site, and value-mode changes rebuild
      # the complete widget. Do not send row data into the old schema.
      if (isTRUE(structural_rebuild())) {
        return()
      }
      
      df <- apply_yield_sort(display_data())
      
      # Ignore a transient zero-row result before the client-side table has
      # initialized. After initialization, genuine user filters that produce
      # zero matches are still allowed to display an empty table.
      if (nrow(df) == 0 && !isTRUE(table_initialized())) {
        return()
      }
      
      df <- cbind(
        star = ifelse(df$number %in% starred(),
                      '<i class="fa fa-star"></i>',
                      '<i class="fa fa-star-o"></i>'),
        df,
        stringsAsFactors = FALSE
      )
      current_schema <- rendered_table_columns()
      
      # replaceData() can change rows but cannot change the table's columns.
      # During Compact/Detailed or North/South transitions, wait for renderDT()
      # to create the widget with the new schema.
      if (length(current_schema) == 0 ||
          !identical(names(df), current_schema)) {
        return()
      }
      
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
      
      # replaceData preserves the user's current DataTables header ordering.
      # When yield sorting is switched back on, explicitly restore descending
      # ordering on the regional-average grain-yield column.
      if (isTRUE(input$sort_by_yield)) {
        yield_col <- grep(
          "^Grain\\.Yield_.*RegionalAverage$",
          names(df),
          value = TRUE
        )
        
        if (length(yield_col) == 1) {
          session$sendCustomMessage(
            "sortTableByColumn",
            list(
              column = match(yield_col, names(df)) - 1L,
              direction = "desc"
            )
          )
        }
      }
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
