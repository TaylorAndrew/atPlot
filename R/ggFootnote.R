#' ggFootnote
#'
#' ggFootnote adds a footnote or, more generally, text around the margins of a ggplot2 object
#'
#' @param plotObject A ggplot2 object
#' @param fileName filename to save the output object to. If NULL, then the object will be printed to the plots viewer and not saved.
#' @param xLocation x axis location where the text should be added. Default is 'left'. Options are 'left', 'center', or 'right', or a numeric x-axis entry.
#' @param fontface Font-face to be used. Default is 'plain', other options include 'bold', 'italic', 'bold.italic', etc
#' @param fonttype Font-type to be used. Default is 'HersheySerif'
#' @param fontcolor Font color to be used. Default is 'black'
#' @param fontsize Fond size to use. Default is 10.
#' @param hjust Horizontal fine-adjustment to alignment. Default is -.1.
#' @param vjust Vertical fine-adjustment to alignment. Default is .1. Note that if you want the footnote at the top, you can use vjust to allow for gross-adjustment to the vertical alignment as well.
#' @param width_inch If fileName is non-NULL, what should the width of the output plot file be.
#' @param height_inch If fileName is non-NULL, what should the height of the output plot file be.
#' @param dpi If fileName is non-NULL, what should the dpi of the output plot file be.
#'
#' @return A ggplot2/gridExtra object
#' @export
#'
#' @examples
#' #NULL
ggFootnote <- function(plotObject,
                       footnote,
                       fileName = NULL,
                       xLocation = "left",
                       fontface = "plain",
                       fonttype = "HersheySerif",
                       fontcolor = "black",
                       fontsize = 10,
                       hjust = -0.1,
                       vjust = 0.1,
                       width_inch = 5,
                       height_inch = 5,
                       dpi = 600) {
  k <- sessionInfo()
  if (k$otherPkgs$gridExtra$Version != "0.9.1") {
    return(cat(
      paste0(
        "Please install gridExtra version 0.9.1 prior to using this function\n",
        "This can be done using the devtools package:\n",
        "packageurl <- 'http://cran.r-project.org/src/contrib/Archive/gridExtra/gridExtra_0.9.1.tar.gz'\n",
        "install.packages(packageurl, repos=NULL, type='source')"
      )
    ))

  }
  xLocation <- if (xLocation == "left") {
    0
  } else if (xLocation == "right") {
    1
  } else if (xLocation == "center") {
    .5
  } else if (as.character(xLocation)) {
    0
  } else {
    xLocation
  }
  j <- arrangeGrob(plotObject,
                   sub = textGrob(
                     footnote,
                     x = xLocation,
                     hjust = hjust,
                     vjust = vjust,
                     gp = gpar(
                       fontface = fontface,
                       fontsize = fontsize,
                       fontfamily = fonttype,
                       col = fontcolor
                     )
                   ))
  if (!is.null(fileName)) {
    ggsave(
      filename = fileName,
      plot = j,
      height = height_inch,
      width = width_inch,
      dpi = dpi,
      units = "in"
    )
  } else {
    j
  }
}
