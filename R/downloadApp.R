#' Download App
#'
#' Simple app to download plot or table with filename.
#' The `downloadUI()` function, if used in `ui()`, shows width and height of plot.
#' See `downloadHideApp()` for app without this function.
#' 
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#' @param addDate add date to filename if `TRUE`
#'
#' @importFrom shiny actionButton br checkboxInput div downloadButton
#'             downloadHandler h4 isTruthy moduleServer NS numericInput
#'             observeEvent reactive reactiveVal reactiveValues renderPlot
#'             renderUI req selectInput shinyApp tagList textAreaInput textInput
#'             uiOutput updateNumericInput
#' @importFrom bslib card card_header page_sidebar sidebar
#' @importFrom utils write.csv    
#' @importFrom ggplot2 ggsave
#' @export
downloadApp <- function() {
  ui <- bslib::page_sidebar(
    title = "Test Download",
    sidebar = bslib::sidebar("side_panel", width = 400,
      # Set up arguments for `downloadServer`.
      shiny::uiOutput("selected_plot"),
      shiny::uiOutput("selected_table")
    ),
    downloadInput("download"), # inputs for Plot or Table
    downloadUI("download"),    # width and height for plot
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
    
    downloadServer("download", download_list, addDate = TRUE)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, download_list, addDate = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Download Filename.
    base_filename <- shiny::reactive({
      filename <- shiny::req(download_list$Filename())
      filename <- filename[shiny::req(input$plot_table)]
      if(addDate) {
        filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d"))
      }
      filename
    })
    # Optional UI to edit filename
    output$filename <- renderUI({
      filename <- shiny::req(base_filename())
      shiny::textInput(ns("filename"), "", filename)
    })
    download_filename <- shiny::reactive({
      dated_filename(shiny::req(base_filename()), input$filename)
    })
    
    downloadPlotServer("download_plot", download_list$Plot, download_filename)
    downloadTableServer("download_table", download_list$Table, download_filename)
    
    ## Switch between `Plot` or `Table`.
    output$buttons <- shiny::renderUI({
      shiny::uiOutput(ns(paste0("choices_", shiny::req(input$plot_table))))
    })
    
    # Download handler for table
    output$choices_Table <- shiny::renderUI({
      # Row for download button
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center; margin-bottom: 10px;",
                      "flex-wrap: wrap;"),
        downloadTableInput(ns("download_table"))
      )
    })
    
    # Download handlers for plot
    output$choices_Plot <- shiny::renderUI({
      # Row for plot title, download buttons, and preset buttons
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center; margin-bottom: 10px;",
                      "flex-wrap: wrap;"),
        downloadPlotInput(ns("download_plot"))
      )
    })
    output$dims <- shiny::renderUI({
      switch(shiny::req(input$plot_table),
             Plot = downloadPlotUI(ns("download_plot"))
      )
    })

    # Preview download app.
    output$preview <- shiny::renderUI({
      plot_table <- shiny::req(input$plot_table)
      list(
        "Filename",
        download_list$Filename()[plot_table],
        shiny::br(),
        switch(plot_table,
               Plot  = downloadPlotOutput(ns("download_plot")),
               Table = downloadTableOutput(ns("download_table")))
      )
    })
  })
}
#' @rdname downloadApp
#' @export
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "d-flex flex-wrap align-items-center gap-2 mb-2",
    # Plot/Table selector (compact, no label spacing)
    shiny::div(class = "mb-0",
      shiny::selectInput(ns("plot_table"), label = NULL,
                         choices = c("Plot","Table"),
                         width = "120px")),
    # Buttons (rendered by server UI)
    shiny::uiOutput(ns("buttons")),
    # Filename on the right
    shiny::div(class = "ms-auto mb-0", shiny::uiOutput(ns("filename")))
  )
}
#' @rdname downloadApp
#' @export
downloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dims"))
}
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview"))
}
  