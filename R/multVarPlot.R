#' multVarPlot
#'
#' multVarPlot provides a simple interface for plotting multiple columns in a data.frame across common X and Y axes.
#'
#' @param data data.frame containing variables for plotting
#' @param varList Vector of variable names to plot across the x-axis
#' @param grouping Variable name to use for grouping data
#' @param legendTitle Title for the legend, given a grouping variable
#' @param legendLabels Labels for the legend, given a grouping variable
#' @param xlab X-axis label
#' @param xlabels X-axis value labels to match to the varList variable names
#' @param ylab Y-axis label
#' @param ylabels Y-axis value labels
#' @param colourManual Manual colour pallet for grouping variable levels
#' @param shapeManual Manual shape pallet for grouping variable levels
#' @param title Main title for plot
#' @param errorBars If TRUE, error bars will be added to the plot
#' @param errorBarWidth If errorBars==TRUE, width of the error bars
#' @param errorBarType  If errorBars==TRUE, error bar type: 'sd', 'se', or 'ci'
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' #data(iris)
#' #l <- multVarPlot(data=iris,
#' #              varList= c("Sepal.Length", "Sepal.Width",
#' #                         "Petal.Length", "Petal.Width"),
#' #              grouping = "Species",
#' #              colourManual = c("blue", "red", "black"),
#' #              shapeManual = c(15, 16, 17),
#' #              errorBars = T,
#' #              errorBarType = "sd")
#' #l + theme(legend.position="bottom") +
#' #   scale_x_discrete("Measurement Type",
#' #                    labels=c("Sepal.Length"="SL",
#' #                             "Sepal.Width"="SW",
#' #                             "Petal.Length"="PL",
#' #                             "Petal.Width"="PW"))
multVarPlot <- function(data,
                         varList,
                         grouping,
                         legendTitle = NULL,
                         legendLabels = NULL,
                         xlab=NULL,
                         xlabels=NULL,
                         ylab=NULL,
                         ylabels=NULL,
                         colourManual=NULL,
                         shapeManual=NULL,
                         title=NULL,
                         errorBars=FALSE,
                         errorBarWidth=.1,
                         errorBarType="se") {
  if(is.null(xlab)) xlab="Variables"
  if(is.null(ylab)) ylab="Average"
  if(!errorBarType%in%c("sd", "se", "ci")) {
    return("errorbarType must be one of: 'sd', 'se', or 'ci'")
  }
  ##Load summarySE function##
  summarySE <-
  function(data = NULL, measurevars, groupvars = NULL, na.rm = FALSE,
           conf.interval = .95, .drop = TRUE,digits = 3) {
    detach_package <- function(pkg, character.only = FALSE)
    {
      if (!character.only)
      {
        pkg <- deparse(substitute(pkg))
      }
      search_item <- paste("package", pkg, sep = ":")
      while (search_item %in% search())
      {
        detach(search_item, unload = TRUE, character.only = TRUE)
      }
    }
    detach_package(dplyr)
    print(
      "Warning: dplyr will be detached in order to run this function
      You must re-attach manually"
    )

    do_one <- function(measurevar) {
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm = FALSE) {
        if (na.rm)
          sum(!is.na(x))
        else
          length(x)
      }
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- ddply(
        data, groupvars, .drop = .drop,
        .fun = function(xx, col) {
          c(
            N    = length2(as.numeric(as.character(xx[[col]])), na.rm = na.rm),
            mean = mean(as.numeric(as.character(xx[[col]])), na.rm = na.rm),
            sd   = sd(as.numeric(as.character(xx[[col]])), na.rm = na.rm)
          )
        },
        measurevar
      )
      datac <- data.frame(Var = measurevar,datac)
      # Rename the "mean" column
      datac$mean <- round(datac$mean,digits = digits)
      datac$sd <- round(datac$sd,digits = digits)
      # datac <- rename(datac, c("mean" = measurevar))
      datac$se <-
        round(datac$sd / sqrt(datac$N),digits = digits)  # Calculate standard error of the mean
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval:
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- round(qt(conf.interval / 2 + .5, datac$N - 1),digits = 3)
      datac$ci <- round(datac$se * ciMult,digits = digits)
      return(datac)
    }
    detac_full <- do.call(rbind,lapply(measurevars,do_one))
    return(detac_full)
  }
  plotDat <- summarySE(data=data,
                       measurevars = varList,
                       groupvars = grouping)
  if(is.null(legendTitle)) legendTitle = names(plotDat)[2]
  breaks=levels(plotDat[,2])
  labels=if(is.null(legendLabels)) {levels(plotDat[,2])} else {legendLabels}
  if(is.null(colourManual)) colourManual=c(1:100)[1:length(labels)]
  if(is.null(shapeManual)) shapeManual=c(1:100)[1:length(labels)]
  g <- ggplot(plotDat, aes(x=Var, y=mean,
                           shape=plotDat[,2],
                           colour=plotDat[,2],
                           group=plotDat[,2])) +
         scale_x_discrete(xlab) +
         scale_y_continuous(ylab) +
         scale_colour_manual(name=legendTitle,
                          breaks=breaks,
                          labels=labels,
                          values=colourManual) +
         scale_shape_manual(name=legendTitle,
                          breaks=breaks,
                          labels=labels,
                          values=shapeManual) +
         geom_line() +
         geom_point() +
         theme_classic()
  if(!is.null(title)) g <- g + ggtitle(title)
  if(errorBars==T) g <-
    g +
    geom_errorbar(aes(
      ymin=mean-if(errorBarType=="se") {
        se} else if (errorBarType=="sd") {
          sd} else {ci},
      ymax=mean+if(errorBarType=="se") {
        se} else if (errorBarType=="sd") {
          sd} else {ci}),
      width=errorBarWidth)
  return(g)
}
