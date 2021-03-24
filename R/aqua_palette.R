aqua_palette <- c(
  "#B46843", "#C0AB45", "#5F8841", "#32956F", "#31A6B3",
  "#1EA8CF", "#2A638E", "#834583", "#BB3855"
)

#' @title aqua palette
#' @description aqua palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname aqua_pal
#' @examples
#' library(scales)
#' show_col(aqua_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

aqua_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
  aqua <- aqua_palette

  if (reverse == TRUE) {
    aqua <- rev(aqua)
  }

  if (missing(n)) {
    n <- length(aqua)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(aqua)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(aqua)}!"))
  }

  aqua <- switch(type,
                            continuous = grDevices::colorRampPalette(aqua)(n),
                            discrete = aqua[1:n])

  aqua <- scales::manual_pal(aqua)

  return(aqua)
}

#' @title scale_color_aqua
#' @rdname aqua_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_aqua()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_aqua <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "aqua",
                            aqua_pal(n = n, type = type,
                                                reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = aqua_pal(n = n, type = type,
                                                                reverse = reverse)(256))
  }
}

#' @title scale_colour_aqua
#' @rdname aqua_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_aqua()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_aqua <- scale_color_aqua

#' @title scale_fill_aqua
#' @rdname aqua_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_aqua()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_aqua <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "aqua",
                            aqua_pal(n = n, type = type,
                                                reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = aqua_pal(n = n, type = type,
                                                               reverse = reverse)(256))
  }
}
