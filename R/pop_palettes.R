pop_palettes <- list(
  aqua = c("#B46843", "#C0AB45", "#5F8841", "#32956F", "#31A6B3",
           "#1EA8CF", "#2A638E", "#834583", "#BB3855"),
  bangles = c("#6D3A34", "#825137", "#A47850", "#AD9E70", "#9DA49D",
              "#7C708E", "#7E5876", "#7C5366", "#5F383F"),
  beck = c("#8A4736", "#C1CE7D", "#87E04D", "#92DD8E",
           "#9BC3D7", "#A67FC3", "#B03387", "#BA2874","#A53252"),
  boo = c("#B04D49", "#C06A51", "#C89B68", "#A79F62" ,"#4D6D7B",
          "#42496C", "#633D5A", "#934B63", "#A34351"),
  # boo2 = c("#97807D", "#B69789", "C7B3A2", "#CEC9B6", "#98B9B7",
  #          "#6A97A6", "#6F7C89", "#544E58", "#A95E71"),
  breeders = c("#792F28", "#763F25", "#856426", "#818425", "#799821",
               "#3C7135", "#5F253B", "#89172A", "#93242A"),
  bwitched = c("#B1473C", "#BB553E", "#C65B3F", "#C86141", "#D26D48",
               "#CB7749", "#9C8F49", "#374E5D", "#5F2A60"),
  deeelite = c("#BB923A", "#8DB23C", "#83C044", "#518755" ,"#324976",
               "#3F3788", "#4B3181", "#8C3684", "#E1577D"),
  hole = c("#67332C" ,"#904B32", "#A3663E", "#B98855", "#C9A466",
           "#D4BB6F", "#738664", "#5C4966", "#6D4254"),
  nodoubt = c("#B27058", "#BB915D", "#BAAD73", "#A4B988", "#73AFA0",
              "#65ADBC", "#446F96", "#344681", "#874F8B"),
  sclub7 = c("#945644","#C59F6F", "#9E8F64", "#5D6B58", "#496569",
             "#50768C", "#6288A6", "#7589A4", "#834370"),
  spice = c("#C39384", "#DAC191", "#E8DE97", "#DDE4A2", "#B5D8C6",
            "#ABCEDF", "#BBC2E1", "#E0BFDF", "#CC798C"),
  steps = c("#A65A57", "#D18C7E", "#F2BEA3", "#E5DFBD", "#85BACB",
            "#8895C4", "#BB6CA6", "#BA638C", "#9B535A")
)


#' Color Palettes based on 90s pop album covers
#'
#' R package that contains color palettes based on 90s pop album covers.
#'
#'
#' See also: https://github.com/johnmackintosh/metallicaRt for metallica palettes
#'
#' and https://github.com/johnmackintosh/rockthemes for rock palettes
#'
#' @param name Name of palette. Select one:
#' \code{aqua}, \code{bangles}, \code{beck}, \code{boo},
#' \code{breeders}, \code{bwitched}, \code{deeelite},\code{hole},
#'  \code{nodoubt}, \code{sclub7}, \code{spice},\code{steps}
#'
#'
#' @param n Number of colors desired.
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



