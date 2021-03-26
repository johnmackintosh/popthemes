spice_palette <- c(
  "#C39384", "#DAC191", "#E8DE97", "#DDE4A2", "#B5D8C6",
  "#ABCEDF", "#BBC2E1", "#E0BFDF", "#CC798C"
)

#' @title spice palette
#' @description spice palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname spice_pal
#' @examples
#' library(scales)
#' show_col(spice_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

spice_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  spice <- spice_palette

  if (reverse == TRUE) {
    spice <- rev(spice)
  }

  if (missing(n)) {
    n <- length(spice)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(spice)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(spice)}!"))
  }

  spice <- switch(type,
                 continuous = grDevices::colorRampPalette(spice)(n),
                 discrete = spice[1:n])

  spice <- scales::manual_pal(spice)

  return(spice)
}

#' @title scale_color_spice
#' @rdname spice_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_spice()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_spice <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "spice",
                            spice_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = spice_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_spice
#' @rdname spice_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_spice()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_spice <- scale_color_spice

#' @title scale_fill_spice
#' @rdname spice_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_spice()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_spice <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "spice",
                            spice_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = spice_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
