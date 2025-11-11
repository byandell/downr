#' Download Table App
#'
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#'
#' @importFrom shiny downloadButton downloadHandler isTruthy moduleServer NS
#'             reactive reactiveValues req shinyApp
#' @importFrom bslib page
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom utils write.csv    
#' @export
downloadTableApp <- function() {
  ui <- bslib::page(
    title = "Test Download Table",
    downloadTableInput("download"),         # inputs for Plot or Table
    downloadTableOutput("download")         # download_table
  )
  server <- function(input, output, session) { 
    download_Table <- shiny::reactive(matrix(1:12,nrow=3))
    download_Filename <- shiny::reactive(c(Table = "twelve"))
    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Table = download_Table)
    
    downloadTableServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadTableApp
#' @export
downloadTableServer <- function(id, download_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render table
    output$download_table <- DT::renderDataTable({
      shiny::req(download_list$Table())
    })

    # Download handler for table
    output$Table <- shiny::downloadHandler(
      filename = paste0(shiny::req(download_list$Filename()), ".csv"),
      content = function(file) {
        table <- shiny::req(download_list$Table())
        utils::write.csv(table, file, row.names = FALSE)
      }
    )
  })
}
#' @rdname downloadTableApp
#' @export
downloadTableInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("Table"), "CSV", class = "btn-sm")
}
#' @rdname downloadTableApp
#' @export
downloadTableOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("download_table"))
}
  