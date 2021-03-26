beck_palette <- c("#8A4736", "#C1CE7D", "#87E04D", "#92DD8E",
                  "#9BC3D7", "#A67FC3", "#B03387", "#BA2874","#A53252"
)

#' @title beck palette
#' @description beck palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname beck_pal
#' @examples
#' library(scales)
#' show_col(beck_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

beck_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  beck <- beck_palette

  if (reverse == TRUE) {
    beck <- rev(beck)
  }

  if (missing(n)) {
    n <- length(beck)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(beck)) {
    stop(glue::glue("I'm a loser baby. Palette does not have {n} colors, maximum is {length(beck)}!"))
  }

  beck <- switch(type,
                 continuous = grDevices::colorRampPalette(beck)(n),
                 discrete = beck[1:n])

  beck <- scales::manual_pal(beck)

  return(beck)
}

#' @title scale_color_beck
#' @rdname beck_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_beck()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_beck <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "beck",
                            beck_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = beck_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_beck
#' @rdname beck_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_beck()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_beck <- scale_color_beck

#' @title scale_fill_beck
#' @rdname beck_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_beck()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_beck <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "beck",
                            beck_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = beck_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
