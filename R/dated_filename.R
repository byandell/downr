dated_filename <- function(base_filename, plot_table, input_filename) {
  filename <- base_filename
  # If user modifies filename ...
  if(shiny::isTruthy(input_filename))
    filename[plot_table] <- input_filename
  filename
}
