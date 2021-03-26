steps_h_palette <- c(
  "#A65A57", "#D18C7E", "#F2BEA3", "#E5DFBD", "#85BACB",
  "#8895C4", "#BB6CA6", "#BA638C", "#9B535A"
)

#' @title steps_h palette
#' @description steps_h palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname steps_h_pal
#' @examples
#' library(scales)
#' show_col(steps_h_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

steps_h_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  steps_h <- steps_h_palette

  if (reverse == TRUE) {
    steps_h <- rev(steps_h)
  }

  if (missing(n)) {
    n <- length(steps_h)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(steps_h)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(steps_h)}!"))
  }

  steps_h <- switch(type,
                 continuous = grDevices::colorRampPalette(steps_h)(n),
                 discrete = steps_h[1:n])

  steps_h <- scales::manual_pal(steps_h)

  return(steps_h)
}

#' @title scale_color_steps_h
#' @rdname steps_h_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_steps_h()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_steps_h <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "steps_h",
                            steps_h_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = steps_h_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_steps_h
#' @rdname steps_h_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_steps_h()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_steps_h <- scale_color_steps_h

#' @title scale_fill_steps_h
#' @rdname steps_h_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_steps_h()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_steps_h <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "steps_h",
                            steps_h_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = steps_h_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
