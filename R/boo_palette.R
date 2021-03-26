boo_palette <- c(
  "#B04D49", "#C06A51", "#C89B68", "#A79F62" ,"#4D6D7B",
  "#42496C", "#633D5A", "#934B63", "#A34351"
)

#' @title boo palette
#' @description boo palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname boo_pal
#' @examples
#' library(scales)
#' show_col(boo_pal()(9))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

boo_pal <- function(n, type = c("discrete", "continuous"),
                     reverse = FALSE){
  boo <- boo_palette

  if (reverse == TRUE) {
    boo <- rev(boo)
  }

  if (missing(n)) {
    n <- length(boo)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(boo)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(boo)}!"))
  }

  boo <- switch(type,
                 continuous = grDevices::colorRampPalette(boo)(n),
                 discrete = boo[1:n])

  boo <- scales::manual_pal(boo)

  return(boo)
}

#' @title scale_color_boo
#' @rdname boo_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_boo()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_boo <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "boo",
                            boo_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = boo_pal(n = n, type = type,
                                                     reverse = reverse)(256))
  }
}

#' @title scale_colour_boo
#' @rdname boo_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_boo()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_boo <- scale_color_boo

#' @title scale_fill_boo
#' @rdname boo_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_boo()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_boo <- function(n, type = "discrete",
                            reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "boo",
                            boo_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = boo_pal(n = n, type = type,
                                                    reverse = reverse)(256))
  }
}
