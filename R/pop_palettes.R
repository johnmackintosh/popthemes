pop_palettes <- list(
  aqua = c("#B46843", "#C0AB45", "#5F8841", "#32956F", "#31A6B3",
           "#1EA8CF", "#2A638E", "#834583", "#BB3855"),
  bangles = c("#6D3A34", "#825137", "#A47850", "#AD9E70", "#9DA49D",
              "#7C708E", "#7E5876", "#7C5366", "#5F383F"),
  beck = c("#8A4736", "#C1CE7D", "#87E04D", "#92DD8E",
           "#9BC3D7", "#A67FC3", "#B03387", "#BA2874","#A53252"),
  boo = c("#B04D49", "#C06A51", "#C89B68", "#A79F62" ,"#4D6D7B",
          "#42496C", "#633D5A", "#934B63", "#A34351"),
  bwitched = c("#40234E", "#B09B59", "#732F4D" ,"#D29965", "#5B274B",
                "#C89C93", "#572F58", "#71773E", "#446E80"),
  deeelite = c("#365A4C", "#473D8C", "#90D044", "#DB616D", "#64403E",
               "#5E3C80", "#9C8F42", "#8F2975", "#3A5A3C"),
  kelly = c("#ed5a7f" ,"#f0a93b", "#dbc83f", "#a4966a", "#60a984",
           "#99715f", "#8b565e", "#696969", "#0b0c09"),
  nodoubt = c("#2D4F81" ,"#5BB0BB", "#87A884", "#A93842", "#BA8E53",
              "#7C5A72", "#ABBE7A", "#4D4574","#81A591"),
  rickroll = c( "#1E3850" ,"#39315E", "#ECC639", "#E15368", "#E76A42" ,
              "#97548E", "#364A32", "#644278","#2A3E32"),

  sclub7 = c("#945644","#C59F6F", "#9E8F64", "#5D6B58", "#496569",
             "#50768C", "#6288A6", "#7589A4", "#834370"),
  spice = c("#C39384", "#DAC191", "#E8DE97", "#DDE4A2", "#B5D8C6",
            "#ABCEDF", "#BBC2E1", "#E0BFDF", "#CC798C"),
  steps = c("#A65A57", "#D18C7E", "#F2BEA3", "#E5DFBD", "#85BACB",
            "#8895C4", "#BB6CA6", "#BA638C", "#9B535A")

)



#' Color Palettes based on pop album covers
#'
#' R package that contains color palettes based on pop album covers.
#' See also: https://github.com/johnmackintosh/metallicaRt for metallica palettes
#'
#' and https://github.com/johnmackintosh/rockthemes for rock palettes
#'
#' @param name Name of palette. Select one:
#' \code{aqua}, \code{bangles}, \code{beck}, \code{boo},
#' \code{bwitched}, \code{deeelite},\code{kelly}, \code{nodoubt},
#'  \code{rickroll},\code{sclub7}, \code{spice},\code{steps}
#'
#' @param n Number of colors desired.
#'
#'
#' @param type Either continuous or discrete.
#'
#' @return A vector of colors.
#' @export
#'
#' @examples
#' pop_palette("aqua")
#'
pop_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- pop_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found")

  if (missing(n)) {
    n = length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop(paste("You have requested", n, "colors, but this palette only contains", length(pal), "colors."))
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
