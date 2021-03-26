rickroll_palette <- c(
  "#1E3850" ,"#39315E", "#ECC639", "#E15368", "#E76A42" ,
  "#97548E", "#364A32", "#644278","#2A3E32"
)

#' @title rickroll palette
#' @description rickroll palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname rickroll_pal
#' @examples
#' library(scales)
#' show_col(rickroll_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

rickroll_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  rickroll <- rickroll_palette

  if (reverse == TRUE) {
    rickroll <- rev(rickroll)
  }

  if (missing(n)) {
    n <- length(rickroll)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(rickroll)) {
    stop(glue::glue("I've given you up. I've let you down. Now I'm deserting you. Palette does not have {n} colors, maximum is {length(rickroll)}!"))
  }

  rickroll <- switch(type,
                 continuous = grDevices::colorRampPalette(rickroll)(n),
                 discrete = rickroll[1:n])

  rickroll <- scales::manual_pal(rickroll)

  return(rickroll)
}

#' @title scale_color_rickroll
#' @rdname rickroll_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_rickroll()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_rickroll <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "rickroll",
                            rickroll_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = rickroll_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_rickroll
#' @rdname rickroll_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_rickroll()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_rickroll <- scale_color_rickroll

#' @title scale_fill_rickroll
#' @rdname rickroll_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_rickroll()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_rickroll <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "rickroll",
                            rickroll_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = rickroll_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
