deeelite_palette <- c(
  "#365A4C", "#473D8C", "#90D044", "#DB616D", "#64403E",
  "#5E3C80", "#9C8F42", "#8F2975", "#3A5A3C"
)

#' @title deeelite palette
#' @description deeelite palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname deeelite_pal
#' @examples
#' library(scales)
#' show_col(deeelite_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

deeelite_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  deeelite <- deeelite_palette

  if (reverse == TRUE) {
    deeelite <- rev(deeelite)
  }

  if (missing(n)) {
    n <- length(deeelite)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(deeelite)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(deeelite)}!"))
  }

  deeelite <- switch(type,
                 continuous = grDevices::colorRampPalette(deeelite)(n),
                 discrete = deeelite[1:n])

  deeelite <- scales::manual_pal(deeelite)

  return(deeelite)
}

#' @title scale_color_deeelite
#' @rdname deeelite_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_deeelite()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_deeelite <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "deeelite",
                            deeelite_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = deeelite_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_deeelite
#' @rdname deeelite_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_deeelite()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_deeelite <- scale_color_deeelite

#' @title scale_fill_deeelite
#' @rdname deeelite_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_deeelite()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_deeelite <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "deeelite",
                            deeelite_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = deeelite_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
