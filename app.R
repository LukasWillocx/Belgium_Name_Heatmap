# ==============================================================================
# BELGIUM NAME HEATMAP - MAIN APP WITH A/B COMPARISON
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)
library(plotly)
library(markdown)

# Source external files
source("database_functions.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=6"),
    tags$style(HTML("
      .about-modal-content img {
        max-width: 100%;
        height: auto;
        display: block;
        margin: 15px auto;
      }
      .about-modal-content {
        line-height: 1.6;
      }
      .about-modal-content h1 {
        margin-top: 0;
      }
      .about-modal-content h2 {
        margin-top: 20px;
        border-bottom: 1px solid #e0e0e0;
        padding-bottom: 5px;
      }
    "))
  ),
  
  tags$button(class = "dark-mode-toggle", id = "darkModeToggle", "🌙"),
  
  tags$div(class = "sidebar-toggle hidden", id = "sidebarToggle", "☰"),
  tags$div(class = "metrics-toggle hidden", id = "metricsToggle", "📊"),
  
  # Left Panel
  tags$div(
    class = "left-panel",
    id = "leftPanel",
    tags$button(class = "close-sidebar", id = "closeLeftPanel", "×"),
    tags$div(class = "panel-title", "Name Comparison"),
    
    tags$div(
      class = "filter-group",
      tags$label("Group A", style = "color: #e74c3c; font-weight: bold;"),
      selectizeInput("names_a", NULL, 
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Type to search...',
                       maxItems = 5
                     ))
    ),
    
    tags$div(
      class = "filter-group",
      tags$label("Group B", style = "color: #3498db; font-weight: bold;"),
      selectizeInput("names_b", NULL, 
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Type to search...',
                       maxItems = 5
                     ))
    ),
    
    tags$div(
      class = "filter-group",
      actionButton("reset", "Clear Selection", class = "btn-secondary btn-block")
    ),
    tags$div(
      class = "filter-group",
      actionButton("aboutBtn", "About", class = "btn-info btn-block", icon = icon("info-circle"))),
  ),
  
  # Right Panel
  tags$div(
    class = "right-panel",
    id = "rightPanel",
    tags$button(class = "close-sidebar", id = "closeRightPanel", "×"),
    tags$div(class = "panel-title", "Comparison Statistics"),
    
    conditionalPanel(
      condition = "output.show_metrics",
      uiOutput("metrics_display")
    )
  ),
  
  # Map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      let darkMode = false;
      $('#darkModeToggle').click(function() {
        darkMode = !darkMode;
        $('body').toggleClass('dark-mode');
        Shiny.setInputValue('darkModeState', darkMode);
      });
      
      $('#closeLeftPanel').click(function() {
        $('#leftPanel').addClass('closed');
        $('#sidebarToggle').removeClass('hidden');
      });
      
      $('#sidebarToggle').click(function() {
        $('#leftPanel').removeClass('closed');
        $('#sidebarToggle').addClass('hidden');
      });
      
      $('#closeRightPanel').click(function() {
        $('#rightPanel').addClass('closed');
        $('#metricsToggle').removeClass('hidden');
      });
      
      $('#metricsToggle').click(function() {
        $('#rightPanel').removeClass('closed');
        $('#metricsToggle').addClass('hidden');
      });
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  conn <- get_db_connection()
  onStop(function() { dbDisconnect(conn) })
  
  darkMode <- reactiveVal(FALSE)
  
  observeEvent(input$darkModeState, {
    darkMode(input$darkModeState)
  })
  
  # ===========================================================================
  # ABOUT MODAL
  # ===========================================================================
  
  observeEvent(input$aboutBtn, {
    # Read the markdown file
    about_content <- tryCatch({
      if (file.exists("about.md")) {
        markdownToHTML(file = "about.md", fragment.only = TRUE)
      } else {
        "<p>About content not found. Please create an <code>about.md</code> file.</p>"
      }
    }, error = function(e) {
      paste("<p>Error loading about content:</p><pre>", e$message, "</pre>")
    })
    
    showModal(modalDialog(
      title = "About This Application",
      tags$div(class = "about-modal-content", HTML(about_content)),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  # ===========================================================================
  # NAME AUTOCOMPLETE
  # ===========================================================================
  
  all_names <- reactive({
    get_all_names(conn, NULL)
  })
  
  observe({
    names_list <- all_names()
    updateSelectizeInput(session, "names_a", choices = names_list, server = TRUE)
    updateSelectizeInput(session, "names_b", choices = names_list, server = TRUE)
  })
  
  # ===========================================================================
  # MAP RENDERING
  # ===========================================================================
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
  })
  
  # Dark mode tiles
  observeEvent(darkMode(), {
    if (darkMode()) {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.DarkMatter)
    } else {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
  # Debounced reactive values for names to prevent race conditions
  names_a_debounced <- debounce(reactive(input$names_a), 500)
  names_b_debounced <- debounce(reactive(input$names_b), 500)
  
  # Update map when names are selected - with debouncing
  observe({
    names_a <- names_a_debounced()
    names_b <- names_b_debounced()
    
    if (length(names_a) == 0 && length(names_b) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    # Get data for both groups with error handling
    data_a <- NULL
    data_b <- NULL
    
    tryCatch({
      if (length(names_a) > 0) {
        data_a <- get_name_heatmap_data_detailed(conn, names_a, "a")
      }
    }, error = function(e) {
      showNotification(paste("Error loading Group A data:", e$message), type = "error")
    })
    
    tryCatch({
      if (length(names_b) > 0) {
        data_b <- get_name_heatmap_data_detailed(conn, names_b, "b")
      }
    }, error = function(e) {
      showNotification(paste("Error loading Group B data:", e$message), type = "error")
    })
    
    # Clear existing layers
    map_proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()
    
    # Merge and process data
    combined_sf <- merge_group_data(data_a, data_b)
    
    if (is.null(combined_sf)) return()
    
    # Add to map
    map_proxy %>%
      addPolygons(
        data = combined_sf,
        fillColor = ~color,
        fillOpacity = 0.75,
        color = ~color,
        weight = 1.5,
        label = ~lapply(label_text, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#fff",
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
  })
  
  # ===========================================================================
  # RESET
  # ===========================================================================
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "names_a", selected = character(0))
    updateSelectizeInput(session, "names_b", selected = character(0))
  })
  
  # ===========================================================================
  # METRICS PANEL
  # ===========================================================================
  
  output$show_metrics <- reactive({
    length(input$names_a) > 0 || length(input$names_b) > 0
  })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    if (length(input$names_a) == 0 && length(input$names_b) == 0) {
      return(tags$p("Select names to compare"))
    }
    
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    tagList(
      # Summary comparison - side by side boxes
      tags$div(class = "comparison-grid",
               if (!is.null(stats_a)) {
                 tags$div(class = "group-box group-a-box",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-count", format(sum(stats_a$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", 
                                   paste(input$names_a, collapse = ", ")
                          )
                 )
               } else {
                 tags$div(class = "group-box group-a-box empty",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-empty", "No names selected")
                 )
               },
               if (!is.null(stats_b)) {
                 tags$div(class = "group-box group-b-box",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-count", format(sum(stats_b$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", 
                                   paste(input$names_b, collapse = ", ")
                          )
                 )
               } else {
                 tags$div(class = "group-box group-b-box empty",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-empty", "No names selected")
                 )
               }
      ),
      
      tags$hr(style = "margin: 20px 0;"),
      
      # Interactive frequency chart
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Frequency Comparison by Name", style = "margin-bottom: 15px;"),
          plotlyOutput("comparison_chart", height = "300px")
        )
      },
      
      tags$hr(style = "margin: 20px 0;"),
      
      # Interactive regional chart
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Regional Distribution", style = "margin-bottom: 15px;"),
          plotlyOutput("regional_chart", height = "250px")
        )
      }
    )
  })
  
  # Interactive comparison chart with Plotly
  output$comparison_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    create_comparison_chart(stats_a, stats_b, darkMode())
  })
  
  # Interactive regional chart with Plotly
  output$regional_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    create_regional_chart(stats_a, stats_b, darkMode())
  })
}

shinyApp(ui, server)