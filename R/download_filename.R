download_filename <- function(base_filename, input_filename, mime = "png") {
  function() {
    filename <- shiny::req(base_filename())
    if(shiny::isTruthy(input_filename))
      filename <- input_filename
    paste0(filename, ".", mime)
  }
}
