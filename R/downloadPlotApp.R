#' Download Plot App
#'
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#'
#' @importFrom shiny  div downloadButton downloadHandler moduleServer NS
#'             numericInput observeEvent reactive reactiveVal reactiveValues
#'             renderPlot renderUI req shinyApp tagList uiOutput
#'             updateNumericInput
#' @importFrom bslib page
#' @importFrom ggplot2 ggsave
#' @export
downloadPlotApp <- function() {
  ui <- bslib::page(
    title = "Test Download Plot",
    downloadPlotInput("download"),         # choices_Plot
    downloadPlotUI("download"),            # plot_width, plot_height
    downloadPlotOutput("download")         # download_plot
  )
  server <- function(input, output, session) { 
    download_Plot <- shiny::reactive(plot_null("none"))
    download_Filename <- shiny::reactive(c(Plot = "none"))
    downloadPlotServer("download", download_Plot, download_Filename)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadPlotApp
#' @export
downloadPlotServer <- function(id, download_Plot, download_Filename) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Plot `width` and `height`.
    plot_width_rv <- shiny::reactiveVal(1200)
    plot_height_rv <- shiny::reactiveVal(600)
    shiny::observeEvent(input$plot_width, {
      plot_width_rv(input$plot_width) },
      ignoreNULL = TRUE)
    shiny::observeEvent(input$plot_height, {
      plot_height_rv(input$plot_height) },
      ignoreNULL = TRUE)
    
    # Plot dimension presets.
    # Following works whether or not `downloadPlotUI()` is used.
    update_preset_width <- function(value) {
      if(shiny::isTruthy(input$plot_width)) {
        shiny::updateNumericInput(session, "plot_width", value = value)
      } else {
        plot_width_rv(value)
      }
    }
    update_preset_height <- function(value) {
      if(shiny::isTruthy(input$plot_height)) {
        shiny::updateNumericInput(session, "plot_height", value = value)
      } else {
        plot_height_rv(value)
      }
    }
    shiny::observeEvent(input$preset_1to1, {
      update_preset_width(800)
      update_preset_height(800)
    })
    shiny::observeEvent(input$preset_3to2, {
      update_preset_width(900)
      update_preset_height(600)
    })
    shiny::observeEvent(input$preset_16to9, {
      update_preset_width(1280)
      update_preset_height(720)
    })
    
    # Preview download app.
    output$preview_plot <- shiny::renderUI({
      shiny::req(plot_width_rv(), plot_height_rv())
      width <- shiny::reactive(plot_width_rv() / 2)
      height <- shiny::reactive(plot_height_rv() / 2)
      shiny::renderPlot({
        shiny::req(download_Plot())
      }, width = width, height = height)
    })

    ## Plot buttons.
    output$choices_Plot <- shiny::renderUI({
      # Use supplied `create_` functions or standard `shiny`.
      if (!exists("create_button", mode = "function")) {
        create_button <- shiny::actionButton
      }
      if (!exists("create_download_button", mode = "function")) {
        create_download_button <- shiny::downloadButton
        button_class <- "btn-sm"
      } else {
        button_class <- "btn-sm btn-light"
      }
      # Row for plot title, download buttons, and preset buttons
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center; margin-bottom: 10px;",
                      "flex-wrap: wrap;"),
        shiny::div(
          style = paste("display: flex; align-items: center; gap: 10px;",
                        "flex-grow: 1; justify-content: flex-end;"),
          # Preset Aspect Ratio Buttons
          shiny::div(
            style = "display: flex; gap: 5px; margin-right: 15px;",
            shiny::tagList(
              create_button(ns("preset_1to1"), "1:1", class = button_class),
              create_button(ns("preset_3to2"), "3:2", class = button_class),
              create_button(ns("preset_16to9"), "16:9", class = button_class)
            )
          ),
          # Download Buttons
          shiny::tagList(
            create_download_button(ns("download_plot_png"), "PNG",
                                   class = "btn-sm"),
            create_download_button(ns("download_plot_pdf"), "PDF",
                                   class = "btn-sm")
          )
        )
      )
    })
    output$dims_Plot <- shiny::renderUI({
      if (!exists("create_numeric_input", mode = "function")) {
        create_numeric_input <- shiny::numericInput
      }
      if (!exists("create_lever_switch", mode = "function")) {
        create_lever_switch <- shiny::checkboxInput
      }
      # Row for plot dimension controls and color toggle
      shiny::div(
        style = "display: flex; gap: 10px; align-items: center; margin-bottom: 5px; flex-wrap: wrap;",
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          create_numeric_input(ns("plot_width"), "Width:",
                               value = 1200, min = 400, max = 2000, step = 50, width = "100px"),
          create_numeric_input(ns("plot_height"), "Height:",
                               value = 600, min = 300, max = 1200, step = 50, width = "100px")
        )
      )
    })

    # Download handlers for plot
    output$download_plot_png <- shiny::downloadHandler(
      paste0(shiny::req(download_Filename())["Plot"], ".png"),
      content = function(file) {
        selected_plot <- download_Plot()
        # Use dynamic width/height for saving
        ggplot2::ggsave(file, plot = selected_plot, 
          width = plot_width_rv()/96, # Assuming 96 DPI for conversion from px
          height = plot_height_rv()/96, 
          dpi = 300, units = "in")
      }
    )
    output$download_plot_pdf <- shiny::downloadHandler(
      paste0(shiny::req(download_Filename())["Plot"], ".pdf"),
      content = function(file) {
        selected_plot <- download_Plot()
        ggplot2::ggsave(file, plot = selected_plot, 
          width = plot_width_rv()/96, 
          height = plot_height_rv()/96, 
          device = cairo_pdf, units = "in") # Use cairo_pdf for better PDF quality
      }
    )
  })
}
#' @rdname downloadPlotApp
#' @export
downloadPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("choices_Plot"))
}
#' @rdname downloadPlotApp
#' @export
downloadPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dims_Plot"))
}
downloadPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview_plot"))
}
  