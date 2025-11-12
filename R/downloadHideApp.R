#' @rdname downloadApp
#' @export
downloadHideApp <- function() {
  ui <- bslib::page_sidebar(
    title = "Test Download Hiding Dims",
    sidebar = bslib::sidebar("side_panel", width = 400,
                             # Set up arguments for `downloadServer`.
                             shiny::uiOutput("selected_plot"),
                             shiny::uiOutput("selected_table")
    ),
    downloadInput("download"), # inputs for Plot or Table
#    downloadUI("download"),    # width and height for plot
    bslib::card(
      bslib::card_header("Download Preview"),
      downloadOutput("download")    # Only for Preview of downloadApp().
    )
  )
  server <- function(input, output, session) { 
    # Test Plots
    download_Plot <- shiny::reactiveValues(
      none = shiny::reactive(plot_null("none")),
      some = shiny::reactive(plot_null("some")))
    output$selected_plot <- shiny::renderUI({
      choices <- names(download_Plot)
      shiny::selectInput("selected_plot", "", choices)
    })
    selected_plot <- shiny::reactive({
      download_Plot[[shiny::req(input$selected_plot)]]()
    })
    # Test Tables
    download_Table <- shiny::reactiveValues(
      twelve = shiny::reactive(matrix(1:12,nrow=3)),
      twenty = shiny::reactive(matrix(1:20,nrow=4)))
    output$selected_table <- shiny::renderUI({
      choices <- names(download_Table)
      shiny::selectInput("selected_table", "", choices)
    })
    selected_table <- shiny::reactive({
      download_Table[[shiny::req(input$selected_table)]]()
    })
    download_Filename <- shiny::reactive({
      c(Table = input$selected_table,
        Plot = input$selected_plot)
    })
    
    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Plot = selected_plot,
      Table = selected_table)
    
    downloadServer("download", download_list, addDate = FALSE)
  }
  shiny::shinyApp(ui, server)
}
