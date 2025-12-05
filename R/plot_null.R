#' Null GG Plot
#' 
#' Empty plot with optional message.
#' Useful for shiny apps with plot output.
#' @param msg character message
#' 
#' @importFrom ggplot2 ggplot aes geom_text
#' @importFrom rlang .data
#' @export
plot_null <- function(msg = "no data") {
  ggplot2::ggplot(data.frame(x = 1, y = 1), 
                  ggplot2::aes(.data$x, .data$y, label = msg)) +
    ggplot2::geom_text(size = 10)
}
