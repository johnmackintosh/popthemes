#' print_pal

#' @param x palette name
#'
#' @param ... additional arguments
#'
#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#'
#' @examples
#' #'\donttest{
#' print_pal('aqua')
#'}
#'
print_pal <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
