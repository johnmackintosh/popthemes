kelly_palette <- c("#ed5a7f" ,"#f0a93b", "#dbc83f", "#a4966a", "#60a984",
                   "#99715f", "#8b565e", "#696969", "#0b0c09")

#' @title kelly palette
#' @description kelly palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname kelly_pal
#' @examples
#' library(scales)
#' show_col(kelly_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

kelly_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  kelly <- kelly_palette

  if (reverse == TRUE) {
    kelly <- rev(kelly)
  }

  if (missing(n)) {
    n <- length(kelly)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(kelly)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(kelly)}!"))
  }

  kelly <- switch(type,
                 continuous = grDevices::colorRampPalette(kelly)(n),
                 discrete = kelly[1:n])

  kelly <- scales::manual_pal(kelly)

  return(kelly)
}

#' @title scale_color_kelly
#' @rdname kelly_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_kelly()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_kelly <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "kelly",
                            kelly_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = kelly_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_kelly
#' @rdname kelly_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_kelly()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_kelly <- scale_color_kelly

#' @title scale_fill_kelly
#' @rdname kelly_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_kelly()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_kelly <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "kelly",
                            kelly_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = kelly_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
