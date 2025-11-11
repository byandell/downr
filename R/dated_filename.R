dated_filename <- function(base_filename, input_filename) {
  filename <- base_filename
  # If user modifies filename ...
  if(shiny::isTruthy(input_filename))
    filename <- input_filename
  filename
}
