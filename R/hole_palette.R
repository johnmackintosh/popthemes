hole_palette <- c(
  "#67332C" ,"#904B32", "#A3663E", "#B98855", "#C9A466",
  "#D4BB6F", "#738664", "#5C4966", "#6D4254"
)

#' @title hole palette
#' @description hole palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname hole_pal
#' @examples
#' library(scales)
#' show_col(hole_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

hole_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  hole <- hole_palette

  if (reverse == TRUE) {
    hole <- rev(hole)
  }

  if (missing(n)) {
    n <- length(hole)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(hole)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(hole)}!"))
  }

  hole <- switch(type,
                 continuous = grDevices::colorRampPalette(hole)(n),
                 discrete = hole[1:n])

  hole <- scales::manual_pal(hole)

  return(hole)
}

#' @title scale_color_hole
#' @rdname hole_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_hole()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_hole <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "hole",
                            hole_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = hole_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_hole
#' @rdname hole_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_hole()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_hole <- scale_color_hole

#' @title scale_fill_hole
#' @rdname hole_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_hole()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_hole <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "hole",
                            hole_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = hole_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
