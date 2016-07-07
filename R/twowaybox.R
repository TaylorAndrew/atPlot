#' twowaybox
#'
#' twowaybox provides black and white publication quality box plots for a 1 or 2-way design
#'
#' @param data data.frame containing all data
#' @param group1 name of grouping variable 1
#' @param group2 name of grouping variable 2
#' @param y name of y
#' @param main Main title of plot
#' @param ylim y limits
#' @param labflip If TRUE, group 2 labels will be flipped 90 degrees
#'
#' @return base graph figure
#' @export
#'
#' @examples
#' #test.data<-function(){                                                       #
#' #   factor2<-rbinom(100,1,.5)                                                  #
#' #   factor21<-rbinom(100,1,.5)                                                 #
#' #   factor3<-sample(c('Male','Female','Other'),100,replace=TRUE)               #
#' #   factor31<-sample(c('Catholic','Christian','Atheist'),100,replace=TRUE)     #
#' #   factor4<-rep(1:4,25)                                                       #
#' #   factor41<-sample(c('U.S.','Australia','UK','Canada'),100,replace=TRUE)     #
#' #   factor5<-rep(1:5,20)                                                       #
#' #   factor51<-sample(c('Caucasian','Af.Amer','Asian Am.','Latin Am.',          #
#' #                      'Canadian'),100,replace=TRUE)                           #
#' #   y<-rnorm(100,53,19)                                                        #
#' #   tester<-data.frame(factor2,factor21,factor3,factor31,factor4,factor41,     #
#' #                      factor5,factor51,y)                                     #
#' #   return(tester)                                                             #
#' #}                                                                            #
#' #okaydata <- test.data()                                                        #
#' #twowaybox(labflip=FALSE,data=okaydata,group1=factor5,group2=factor2,y=y)
#' #twowaybox(labflip=FALSE,data=okaydata,group1=factor5,y=y)
#' #twowaybox(labflip=FALSE,data=okaydata,group1=factor3,y=y)
#' #twowaybox(labflip=FALSE,data=okaydata,group1=factor2,y=y)
twowaybox <- function(data,
                      group1,
                      group2 = NULL,
                      y,
                      ylim = NULL,
                      main = NULL,
                      labflip = TRUE) {
  attach(data)
  g1levels <- levels(as.factor(group1))

  g2levels <- if(!is.null(group2)) {
    levels(as.factor(group2)) } else { 1 }
  numboxes <-
    1:(length(g1levels) * length(g2levels))
  firstsplit <- (length(g2levels) * length(g1levels)) /
                length(g2levels)
  secondsplit <- (firstsplit / length(g1levels))* 1
  margins <- (numboxes * secondsplit)
  margfunc <- function(l) {
    if ((l - 1) %% (length(g1levels)) == 0) {
      margins <<- c(margins[1:l - 1], margins[l:length(margins)] + 1)
    }
  }
  fulllist <- 2:length(margins)
  bettermargins <- as.vector(if(is.null(group2)) {
    margins} else {do.call(rbind, lapply(fulllist, margfunc))})
  finalmargins <- bettermargins
  otherlist <- 1:length(margins)
  find.middle <- function(i) {
    ((finalmargins[i] + finalmargins[i + length(levels(as.factor(group1))) -
                                       1]) / 2)
  }
  get.labplace <- function(l) {
    if (l == 1)
      l
    else if (l %% length(g1levels) - 1 == 0)
      l
  }
  firstlabs <- do.call(rbind, lapply(otherlist, get.labplace))
  oddlist <-
    subset((1:(2 * length(g1levels))),(1:(2 * length(g1levels))) %% 2 == 1)
  labelplacement <<- do.call(rbind,lapply(firstlabs,find.middle))
  #png('triple box plots with patterns.png')
  boxplots.triple = boxplot(if(!is.null(group2)) {
    y ~ as.factor(group1) + as.factor(group2)} else {
    y ~ as.factor(group1)
    },
    data = data,
    at = finalmargins,
    xaxt = if(!is.null(group2)) {'n'} else {NULL},
    ylim = if(is.null(ylim)) {
        c(min(y, na.rm=T),
          max(y, na.rm = T)) } else {
        ylim
          },
    col = c('white', if (length(g1levels) == 2) {
      'gray'
    }
    else if (length(g1levels) == 3) {
      c('white','gray')
    }
    else if (length(g1levels) == 4) {
      c('white','gray', 'white')
    }
    else if (length(g1levels) == 5) {
      c('white','gray', 'white','white')
    })
  )
  par(las = 1)
  if(!is.null(group2)){
    axis(
    side = 1, at = labelplacement,
    labels = c(g2levels),
    line = 3, lwd = 0
  )
  if (labflip == TRUE)
    par(las = 2)
  axis(
    side = 1, at = finalmargins,
    labels = rep(g1levels,length(g2levels)),
    line = -.5, lwd = 0
  )
  }

  title(if(is.null(main)) {
    paste(
    'Comparing',
    deparse(substitute(y)),
    'across levels of',
    deparse(substitute(group1)),
    'and',
    deparse(substitute(group2))
  )} else {main})
  if (length(g1levels) == 4 & length(g2levels) == 2) {
    rect(
      finalmargins[c(2,6)] - .4, boxplots.triple$stats[2, c(2, 6)], finalmargins[c(2,6)] +
        .4, boxplots.triple$stats[4, c(2, 6)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,8)] - .4, boxplots.triple$stats[2, c(4, 8)], finalmargins[c(4,8)] +
        .4, boxplots.triple$stats[4, c(4, 8)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 4 & length(g2levels) == 3) {
    rect(
      finalmargins[c(2,6,10)] - .4, boxplots.triple$stats[2, c(2, 6,10)], finalmargins[c(2,6,10)] +
        .4, boxplots.triple$stats[4, c(2, 6,10)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,8,12)] - .4, boxplots.triple$stats[2, c(4, 8,12)], finalmargins[c(4,8,12)] +
        .4, boxplots.triple$stats[4, c(4, 8,12)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 4 & length(g2levels) == 4) {
    rect(
      finalmargins[c(2,6,10,14)] - .4, boxplots.triple$stats[2, c(2, 6,10,14)], finalmargins[c(2,6,10,14)] +
        .4, boxplots.triple$stats[4, c(2, 6,10,14)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,8,12,16)] - .4, boxplots.triple$stats[2, c(4, 8,12,16)], finalmargins[c(4,8,12,16)] +
        .4, boxplots.triple$stats[4, c(4, 8,12,16)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 4 & length(g2levels) == 5) {
    rect(
      finalmargins[c(2,6,10,14,18)] - .4, boxplots.triple$stats[2, c(2, 6,10,14,18)], finalmargins[c(2,6,10,14,18)] +
        .4, boxplots.triple$stats[4, c(2, 6,10,14,18)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,8,12,16,20)] - .4, boxplots.triple$stats[2, c(4, 8,12,16,20)], finalmargins[c(4,8,12,16,20)] +
        .4, boxplots.triple$stats[4, c(4, 8,12,16,20)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 3 & length(g2levels) == 2) {
    rect(
      finalmargins[c(2,5)] - .4, boxplots.triple$stats[2, c(2, 5)], finalmargins[c(2,5)] +
        .4, boxplots.triple$stats[4, c(2, 5)], density = 12, angle = 45
    )
  }
  if (length(g1levels) == 3 & length(g2levels) == 3) {
    rect(
      finalmargins[c(2,5,8)] - .4, boxplots.triple$stats[2, c(2, 5,8)], finalmargins[c(2,5,8)] +
        .4, boxplots.triple$stats[4, c(2, 5,8)], density = 12, angle = 45
    )
  }
  if (length(g1levels) == 3 & length(g2levels) == 4) {
    rect(
      finalmargins[c(2,5,8,11)] - .4, boxplots.triple$stats[2, c(2, 5,8,11)], finalmargins[c(2,5,8,11)] +
        .4, boxplots.triple$stats[4, c(2, 5,8,11)], density = 12, angle = 45
    )
  }
  if (length(g1levels) == 3 & length(g2levels) == 5) {
    rect(
      finalmargins[c(2,5)] - .4, boxplots.triple$stats[2, c(2, 5,8,11,14)],finalmargins[c(2,5,8,11,14)] +
        .4, boxplots.triple$stats[4, c(2, 5,8,11,14)], density = 12, angle = 45
    )
  }
  if (length(g1levels) == 5 & length(g2levels) == 2) {
    rect(
      finalmargins[c(2,7)] - .4, boxplots.triple$stats[2, c(2, 7)], finalmargins[c(2,7)] +
        .4, boxplots.triple$stats[4, c(2, 7)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9)] - .4, boxplots.triple$stats[2, c(4, 9)], finalmargins[c(4,9)] +
        .4, boxplots.triple$stats[4, c(4, 9)], density = 12, angle = 135
    )
    rect(
      finalmargins[c(5,10)] - .4, boxplots.triple$stats[2, c(5, 10)], finalmargins[c(5,10)] +
        .4, boxplots.triple$stats[4, c(5, 10)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(5,10)] - .4, boxplots.triple$stats[2, c(5, 10)], finalmargins[c(5,10)] +
        .4, boxplots.triple$stats[4, c(5, 10)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 5 & length(g2levels) == 3) {
    rect(
      finalmargins[c(2,7,11)] - .4, boxplots.triple$stats[2, c(2, 7,11)], finalmargins[c(2,7,11)] +
        .4, boxplots.triple$stats[4, c(2, 7,11)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9,14)] - .4, boxplots.triple$stats[2, c(4, 9,14)], finalmargins[c(4,9,14)] +
        .4, boxplots.triple$stats[4, c(4, 9,14)], density = 12, angle = 135
    )
    rect(
      finalmargins[c(5,10,15)] - .4, boxplots.triple$stats[2, c(5, 10,15)], finalmargins[c(5,10,15)] +
        .4, boxplots.triple$stats[4, c(5, 10,15)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(5,10,15)] - .4, boxplots.triple$stats[2, c(5, 10,15)], finalmargins[c(5,10,15)] +
        .4, boxplots.triple$stats[4, c(5, 10,15)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 5 & length(g2levels) == 4) {
    rect(
      finalmargins[c(2,7,11,16)] - .4, boxplots.triple$stats[2, c(2, 7,11,16)], finalmargins[c(2,7,11,16)] +
        .4, boxplots.triple$stats[4, c(2, 7,11,16)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9,14,19)] - .4, boxplots.triple$stats[2, c(4, 9,14,19)], finalmargins[c(4,9,14,19)] +
        .4, boxplots.triple$stats[4, c(4, 9,14,19)], density = 12, angle = 135
    )
    rect(
      finalmargins[c(5,10,15,20)] - .4, boxplots.triple$stats[2, c(5, 10,15,20)], finalmargins[c(5,10,15,20)] +
        .4, boxplots.triple$stats[4, c(5, 10,15,20)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(5,10,15,20)] - .4, boxplots.triple$stats[2, c(5, 10,15,20)], finalmargins[c(5,10,15,20)] +
        .4, boxplots.triple$stats[4, c(5, 10,15,20)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 5 & length(g2levels) == 5) {
    rect(
      finalmargins[c(2,7,11,16,21)] - .4, boxplots.triple$stats[2, c(2, 7,11,16,21)], finalmargins[c(2,7,11,16,21)] +
        .4, boxplots.triple$stats[4, c(2, 7,11,16,21)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9,14,19,24)] - .4, boxplots.triple$stats[2, c(4, 9,14,19,24)], finalmargins[c(4,9,14,19,24)] +
        .4, boxplots.triple$stats[4, c(4, 9,14,19,24)], density = 12, angle = 135
    )
    rect(
      finalmargins[c(5,10,15,20,25)] - .4, boxplots.triple$stats[2, c(5, 10,15,20,25)], finalmargins[c(5,10,15,20,25)] +
        .4, boxplots.triple$stats[4, c(5, 10,15,20,25)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(5,10,15,20,25)] - .4, boxplots.triple$stats[2, c(5, 10,15,20,25)], finalmargins[c(5,10,15,20,25)] +
        .4, boxplots.triple$stats[4, c(5, 10,15,20,25)], density = 12, angle = 135
    )
  }
  #no Grouping variable 2

  if (length(g1levels) == 5 & length(g2levels) == 1) {
    rect(
      finalmargins[c(2,7)] - .4, boxplots.triple$stats[2, c(2)], finalmargins[c(2)] +
        .4, boxplots.triple$stats[4, c(2)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9)] - .4, boxplots.triple$stats[2, c(4)], finalmargins[c(4)] +
        .4, boxplots.triple$stats[4, c(4)], density = 12, angle = 135
    )
    rect(
      finalmargins[c(5,10)] - .4, boxplots.triple$stats[2, c(5)], finalmargins[c(5)] +
        .4, boxplots.triple$stats[4, c(5)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(5,10)] - .4, boxplots.triple$stats[2, c(5)], finalmargins[c(5)] +
        .4, boxplots.triple$stats[4, c(5)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 4 & length(g2levels) == 1) {
    rect(
      finalmargins[c(2,7,11)] - .4, boxplots.triple$stats[2, c(2)], finalmargins[c(2)] +
        .4, boxplots.triple$stats[4, c(2)], density = 12, angle = 45
    )
    rect(
      finalmargins[c(4,9,14)] - .4, boxplots.triple$stats[2, c(4)], finalmargins[c(4)] +
        .4, boxplots.triple$stats[4, c(4)], density = 12, angle = 135
    )
  }
  if (length(g1levels) == 3 & length(g2levels) == 1) {
    rect(
      finalmargins[c(2,7,11,16)] - .4, boxplots.triple$stats[2, c(2)], finalmargins[c(2)] +
        .4, boxplots.triple$stats[4, c(2)], density = 12, angle = 45
    )
  }
  detach(data)
}
