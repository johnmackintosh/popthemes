bwitched_palette <- c("#40234E", "#B09B59", "#732F4D" ,"#D29965", "#5B274B",
                  "#C89C93", "#572F58", "#71773E", "#446E80"
)

#' @title bwitched palette
#' @description bwitched palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname bwitched_pal
#' @examples
#' library(scales)
#' show_col(bwitched_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

bwitched_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  bwitched <- bwitched_palette

  if (reverse == TRUE) {
    bwitched <- rev(bwitched)
  }

  if (missing(n)) {
    n <- length(bwitched)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(bwitched)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(bwitched)}!"))
  }

  bwitched <- switch(type,
                 continuous = grDevices::colorRampPalette(bwitched)(n),
                 discrete = bwitched[1:n])

  bwitched <- scales::manual_pal(bwitched)

  return(bwitched)
}

#' @title scale_color_bwitched
#' @rdname bwitched_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_bwitched()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_bwitched <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "bwitched",
                            bwitched_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = bwitched_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_bwitched
#' @rdname bwitched_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_bwitched()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_bwitched <- scale_color_bwitched

#' @title scale_fill_bwitched
#' @rdname bwitched_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_bwitched()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_bwitched <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "bwitched",
                            bwitched_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = bwitched_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
