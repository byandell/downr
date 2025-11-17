#' Download App
#'
#' Simple app to download plot or table with filename.
#' The `downloadUI()` function, if used in `ui()`, shows width and height of plot.
#' See `downloadHideApp()` for app without this function;
#' the latter also removes the date on filename.
#' 
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#' @param addDate add date to filename if `TRUE`
#' @param showFilename show filename in UI if `TRUE`
#'
#' @importFrom shiny actionButton br checkboxInput div downloadButton
#'             downloadHandler h4 isTruthy moduleServer NS numericInput
#'             observeEvent reactive reactiveVal reactiveValues renderPlot
#'             renderText renderUI req selectInput shinyApp tagList textOutput
#'             uiOutput updateNumericInput
#' @importFrom bslib card card_header page_sidebar sidebar
#' @importFrom utils write.csv    
#' @importFrom ggplot2 ggsave
#' @export
downloadApp <- function(addDate = FALSE, showFilename = FALSE) {
  ui <- bslib::page_sidebar(
    title = "Test Download",
    sidebar = bslib::sidebar("side_panel", width = 400,
      # Set up arguments for `downloadServer`.
      shiny::uiOutput("selected_plot"),
      shiny::uiOutput("selected_table"),
      shiny::uiOutput("download_type")
    ),
    downloadInput("download"), # inputs for Plot or Table
    downloadUI("download"),    # (optional) width and height for plot
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
      shiny::selectInput("selected_plot", "Plot:", choices)
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
      shiny::selectInput("selected_table", "Table:", choices)
    })
    selected_table <- shiny::reactive({
      download_Table[[shiny::req(input$selected_table)]]()
    })
    download_Filename <- shiny::reactive({
      c(Table = shiny::req(input$selected_table),
        Plot  = shiny::req(input$selected_plot))
    })
    output$download_type <- shiny::renderUI({
      shiny::selectInput("download_type", "Choose Type Plot/Table Here or in Strip:",
                         c("Strip","Plot","Table"))
    })

    download_type <- shiny::reactive(shiny::req(input$download_type))
    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Plot = selected_plot,
      Table = selected_table,
      Type = download_type)
    
    downloadServer("download", download_list, addDate = addDate,
                   showFilename = showFilename)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, download_list, addDate = FALSE,
                           showFilename = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    download_type <- shiny::reactive({
      out <- download_list$Type
      if(is.reactive(out)) {
        out <- out()
      }
      if(!shiny::isTruthy(out) || !(out %in% c("Plot", "Table"))) {
        out <- NULL
      }
      out
    })
    output$show_plot_table <- shiny::renderUI({
      if(!shiny::isTruthy(download_type())) {
        shiny::div(class = "mb-0",
                   shiny::selectInput(ns("plot_table"), label = NULL,
                                      choices = c("Plot","Table"),
                                      width = "120px"))
      }
    })
    plot_table <- shiny::reactive({
      if(shiny::isTruthy(download_type())) {
        download_type()
      } else {
        shiny::req(input$plot_table)
      }
    })
    # Optional UI to edit filename
    output$filename <- shiny::renderText({
      shiny::req(filename_base())[shiny::req(plot_table())]
    })
    # Download Filename.
    filename_base <- shiny::reactive({
      filename <- shiny::req(download_list$Filename())
      if(addDate) {
        for(i in seq_along(filename))
          filename[i] <- paste0(filename[i], "_", format(Sys.time(), "%Y%m%d"))
      }
      filename
    })
    filename_plot <- shiny::reactive({
      shiny::req(download_list$Filename())
      shiny::req(filename_base())["Plot"]})
    filename_table <- shiny::reactive(shiny::req(filename_base())["Table"])
    
    downloadPlotServer("download_plot", download_list$Plot, filename_plot)
    downloadTableServer("download_table", download_list$Table, filename_table)
    
    ## Switch between `Plot` or `Table`.
    output$buttons <- shiny::renderUI({
      shiny::uiOutput(ns(paste0("choices_", shiny::req(plot_table()))))
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
      switch(shiny::req(plot_table()),
             Plot = downloadPlotUI(ns("download_plot"))
      )
    })

    # Preview download app.
    output$preview <- shiny::renderUI({
      plot_table <- shiny::req(plot_table())
      list(
        "Filename",
        download_list$Filename()[plot_table],
        shiny::br(),
        switch(plot_table,
               Plot  = downloadPlotOutput(ns("download_plot")),
               Table = downloadTableOutput(ns("download_table")))
      )
    })
    output$filename_output <- shiny::renderUI({
      if(showFilename) {
        shiny::div(class = "ms-auto mb-0", shiny::textOutput(ns("filename")))
      }
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
    shiny::uiOutput(ns("show_plot_table")),
    # Buttons (rendered by server UI)
    shiny::uiOutput(ns("buttons")),
    # Filename on the right if desired
    shiny::uiOutput(ns("filename_output"))
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
  