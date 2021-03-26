sclub_palette <- c(
  "#945644","#C59F6F", "#9E8F64", "#5D6B58", "#496569",
  "#50768C", "#6288A6", "#7589A4", "#834370"
)

#' @title sclub palette
#' @description sclub palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname sclub_pal
#' @examples
#' library(scales)
#' show_col(sclub_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

sclub_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  sclub <- sclub_palette

  if (reverse == TRUE) {
    sclub <- rev(sclub)
  }

  if (missing(n)) {
    n <- length(sclub)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(sclub)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(sclub)}!"))
  }

  sclub <- switch(type,
                 continuous = grDevices::colorRampPalette(sclub)(n),
                 discrete = sclub[1:n])

  sclub <- scales::manual_pal(sclub)

  return(sclub)
}

#' @title scale_color_sclub
#' @rdname sclub_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_sclub()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_sclub <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "sclub",
                            sclub_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = sclub_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_sclub
#' @rdname sclub_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_sclub()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_sclub <- scale_color_sclub

#' @title scale_fill_sclub
#' @rdname sclub_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_sclub()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_sclub <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "sclub",
                            sclub_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = sclub_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
