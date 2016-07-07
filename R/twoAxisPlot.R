#' twoAxisPlot
#'
#' twoAxisPlot plots two different y vectors to a single plot with two separate y-axes on the left and right of the plot.
#'
#' @param y1 Vector of y1
#' @param y2 Vector of y2
#' @param x1 Vector of x1
#' @param x2 Vector of x2 (if same x should be used for both, can just specify x1's vector a second time)
#' @param main Main Title
#' @param ylab1 Y-axis label for y1
#' @param ylab2 Y-axis label for y2
#' @param xlab X-axis label
#' @param ylim1 Upper and lower limits for y1
#' @param ylim2 Upper and lower limits for y2
#' @param xBreaks Breaks for X-axis. If NULL, then all unique levels across x1 and x2 are used
#' @param xLabels Labels for X-axis values. If NULL, then actual values are used.
#' @param legendNames Names of y1 and y2 for legend
#'
#' @return Plot object printed to plots panel
#' @export
#'
#' @examples
#' x1 <- c(0, 35, 100, 300)
#' x2 <- c(0, 100, 300)
#' y1 <- c(10, 12, 16, 13)
#' y2 <- c(900, 850, 825)
#' twoAxisPlot(y1 = y1, y2 = y2, x1 = x1, x2 = x2,
#'             main = 'Example plot',
#'             ylab1 = 'Y Variable 1',
#'             ylab2 = 'Y Variable 2',
#'             xlab = 'X Variable',
#'             ylim1 = c(10, 20), ylim2 = c(800, 1000),
#'             xBreaks = NULL, xLabels = NULL,
#'             legendNames = c("Variable 1", "Variable 2"))
twoAxisPlot <- function(y1, y2, x1, x2,
                        main, ylab1, ylab2, xlab,
                        ylim1 = NULL, ylim2 = NULL,
                        xBreaks = NULL, xLabels = NULL,
                        legendNames) {
par(mar=c(5, 4, 4, 6) + 0.1)
levs <- unique(c(x1, x2))
## Plot first set of data and draw its axis
plot(x1, y1, pch=16, axes=FALSE, ylim=ylim1, xlab="", ylab="",
   type="b",col="black", main=main)
axis(2, ylim=ylim1,col="black",las=1)  ## las=1 makes horizontal labels
mtext(ylab1, side=2, line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(x2, y2, pch=15,  xlab="", ylab="", ylim=ylim2,
    axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext(ylab2 ,side=4, col="red", line=4)
axis(4, ylim=ylim2, col="red",col.axis="red",las=1)

## Draw the time axis
axis(side=1,at=if(is.null(xBreaks)){levs} else {xBreaks},
     labels=if(is.null(xLabels)){levs} else {xLabels})
mtext(xlab,side=1,col="black",line=2.5)

## Add Legend
legend("top",legend=legendNames,
  text.col=c("black","red"),pch=c(16,15),col=c("black","red"))
}
