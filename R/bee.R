#' bee
#'
#' bee is a convenience wrapper for the beeswarm::beeswarm function
#'
#' @param y Name of the y variable
#' @param grouping Name of the grouping variable
#' @param data Input dataframe where y and grouping are found
#' @param logtransform Should the data be log transformed
#' @param xlab X-axis Label
#' @param ylab Y-axis Label
#' @param main Main Title
#' @param grouplabels Label for the grouping variable
#' @param color Vector for the dot colors
#' @param dotshape Vecor for the dot shapes
#' @param boxplot Should a boxplot be plotted along with beeswarm plot?
#' @param boxplotRange How long should the whiskers be? Default is 1.5
#' @param method Beeswarm plotting method. Default is 'swarm'. Options include: 'swarm', 'center', 'hex', 'square'
#' @param corral Further plotting method. Default is 'wrap', Options include: 'none', 'gutter', 'wrap', 'random', 'omit'
#'
#' @return A beeswarm plot
#' @export
#'
#' @examples
#' #dat<-data.frame(group=rep(c(1,2,3),40),x1=rnorm(120,18,5))
#' #names(dat)<-c("group","x1")
#' #bee(y="x1",
#' #    grouping="group",
#' #    data=dat,
#' #    boxplot=TRUE,
#' #    xlab="Severity",
#' #    ylab="Score",
#' #    main="Some Made Up Data",
#' #    grouplabels=c("Low","Medium","High"),
#' #    logtransform=FALSE)
bee <- function(y,
                grouping = NULL,
                data,
                logtransform = FALSE,
                xlab = grouping,
                ylab = y,
                main = if (!is.null(grouping)) {
                  rbind(ylab,"by", xlab)
                } else {
                  ylab
                },
                grouplabels = FALSE,
                color = 1,
                dotshape = 16,
                boxplot = FALSE,
                boxplotRange = 1.5,
                method = "swarm",
                corral = "wrap")
{
   if (!is.null(grouping)) {
    if (boxplot == TRUE) {
      boxplot(
        data[,y] ~ data[,grouping], data = data, outline = FALSE,xlab = xlab, ylab = ylab,
        names = if (grouplabels != FALSE) {
          grouplabels
        } else {
          levels(factor(data[,grouping]))
        },main = main,range = boxplotRange
      )
    }
    beeswarm(
      data[,y] ~ data[,grouping], data = data, log = logtransform, pch = dotshape, xlab = xlab, ylab = ylab,
      labels = if (grouplabels != FALSE) {
        grouplabels
      } else {
        levels(factor(data[,grouping]))
      }
      , method = method, corral = corral, col = color, main = main, add =
        boxplot
    )
  }
  if (is.null(grouping)) {
    if (boxplot == TRUE) {
      boxplot(
        data[,y], data = data, outline = FALSE,xlab = xlab, ylab = ylab,
        main = main,range = boxplotRange
      )
    }
    beeswarm(
      data[,y], data = data, log = logtransform, pch = dotshape, xlab = xlab, ylab = ylab,
      method = method, corral = corral, col = color, main = main, add =
        boxplot
    )
  }
}
