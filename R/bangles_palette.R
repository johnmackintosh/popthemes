bangles_palette <- c("#6D3A34", "#825137", "#A47850", "#AD9E70", "#9DA49D",
                     "#7C708E", "#7E5876", "#7C5366", "#5F383F"
)

#' @title bangles palette
#' @description bangles palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname bangles_pal
#' @examples
#' library(scales)
#' show_col(bangles_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

bangles_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  bangles <- bangles_palette

  if (reverse == TRUE) {
    bangles <- rev(bangles)
  }

  if (missing(n)) {
    n <- length(bangles)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(bangles)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(bangles)}!"))
  }

  bangles <- switch(type,
                 continuous = grDevices::colorRampPalette(bangles)(n),
                 discrete = bangles[1:n])

  bangles <- scales::manual_pal(bangles)

  return(bangles)
}

#' @title scale_color_bangles
#' @rdname bangles_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_bangles()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_bangles <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "bangles",
                            bangles_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = bangles_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_bangles
#' @rdname bangles_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_bangles()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_bangles <- scale_color_bangles

#' @title scale_fill_bangles
#' @rdname bangles_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_bangles()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_bangles <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "bangles",
                            bangles_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = bangles_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
