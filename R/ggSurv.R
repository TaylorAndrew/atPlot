#' ggSurv
#'
#' @param s suvival object from survdiff(Surv())
#' @param plot.cens TRUE/FALSE: Plot censored observations?
#' @param surv.col Color for survival curves
#' @param cens.col Color for censor ticks
#' @param cens.size Size of censor ticks
#' @param lty.est Line-type for survival curves
#' @param lty.ci Line-type for survival curve confidence intervals
#' @param cens.shape Censor tick shapes
#' @param theme_classic TRUE/FALSE: Should the theme be set to theme_class()
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main main title
#' @param legendName Title for the legend
#' @param legendLabels Labels for the legend
#'
#' @return A ggplot2 Kaplan-Meier survival curve object
#' @export
#'
#' @examples
#' #NULL
ggSurv <- function(s,
                   CI = 'def',
                   plot.cens = T,
                   surv.col = 'gg.def',
                   cens.col = 'red',
                   cens.size = 1,
                   lty.est = 1,
                   lty.ci = 2,
                   cens.shape = 3,
                   theme_classic = T,
                   xlab = 'Time',
                   ylab = 'Survival',
                   main = '',
                   legendName = NULL,
                   legendLabels = NULL) {
  strata <- ifelse(is.null(s$strata) == T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  ggsurv.s <-
    function(s,
             CI = 'def',
             plot.cens = T,
             surv.col = 'gg.def',
             cens.col = 'red',
             lty.est = 1,
             lty.ci = 2,
             cens.shape = 3,
             theme_classic = F,
             xlab = 'Time',
             ylab = 'Survival',
             main = '') {
      dat <- data.frame(
        time = c(0, s$time),
        surv = c(1, s$surv),
        up = c(1, s$upper),
        low = c(1, s$lower),
        cens = c(0, s$n.censor)
      )
      dat.cens <- subset(dat, cens != 0)
      col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
      pl <- ggplot(dat, aes(x = time, y = surv)) +
        xlab(xlab) + ylab(ylab) + ggtitle(main) +
        geom_step(col = col, lty = lty.est)

      pl <- if (CI == T | CI == 'def') {
        pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
          geom_step(aes(y = low), color = col, lty = lty.ci)
      } else
        (pl)

      pl <- if (plot.cens == T & length(dat.cens) > 0) {
        pl + geom_point(data = dat.cens,
                        aes(y = surv),
                        shape = cens.shape,
                        col = cens.col,
                        size = cens.size)
      } else if (plot.cens == T & length(dat.cens) == 0) {
        stop ('There are no censored observations')
      } else
        (pl)
      pl <- if (theme_classic == T) {
        pl + theme_classic()
      } else
        (pl)
      pl
    }
  ggsurv.m <-
    function(s,
             CI = 'def',
             plot.cens = T,
             surv.col = 'gg.def',
             cens.col = 'red',
             lty.est = 1,
             lty.ci = 2,
             cens.shape = 3,
             theme_classic = F,
             xlab = 'Time',
             ylab = 'Survival',
             main = '') {
      n <- s$strata
      groups <- factor(unlist(strsplit(names
                                       (s$strata), '='))[seq(2, 2 * strata, by = 2)])
      gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
      # if(!is.null(legendName)) gr.name<-legendName
      gr.df <- vector('list', strata)
      ind <- vector('list', strata)
      n.ind <- c(0, n)
      n.ind <- cumsum(n.ind)
      for (i in 1:strata)
        ind[[i]] <- (n.ind[i] + 1):n.ind[i + 1]
      for (i in 1:strata) {
        gr.df[[i]] <- data.frame(
          time = c(0, s$time[ind[[i]]]),
          surv = c(1, s$surv[ind[[i]]]),
          up = c(1, s$upper[ind[[i]]]),
          low = c(1, s$lower[ind[[i]]]),
          cens = c(0, s$n.censor[ind[[i]]]),
          group = rep(groups[i], n[i] + 1)
        )
      }
      dat <- do.call(rbind, gr.df)
      dat.cens <- subset(dat, cens != 0)
      pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
        xlab(xlab) + ylab(ylab) + ggtitle(main) +
        geom_step(aes(col = group, lty = group))
      col <- if (length(surv.col == 1)) {
        scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
      } else{
        scale_colour_manual(name = gr.name, values = surv.col)
      }
      pl <- if (surv.col[1] != 'gg.def') {
        pl + col
      } else {
        pl + scale_colour_discrete(name = gr.name)
      }
      line <- if (length(lty.est) == 1) {
        scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
      } else {
        scale_linetype_manual(name = gr.name, values = lty.est)
      }
      pl <- pl + line
      pl <- if (CI == T) {
        if (length(surv.col) > 1 && length(lty.est) > 1) {
          stop(
            'Either surv.col or lty.est should be of length 1 in order
            to plot 95% CI with multiple strata'
          )
        } else if ((length(surv.col) > 1 |
                    surv.col == 'gg.def')[1]) {
          pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
            geom_step(aes(y = low, color = group), lty = lty.ci)
        } else{
          pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
            geom_step(aes(y = low, lty = group), col = surv.col)
        }
        } else {
          pl
        }
      pl <- if (plot.cens == T & length(dat.cens) > 0) {
        pl + geom_point(data = dat.cens,
                        aes(y = surv),
                        shape = cens.shape,
                        col = cens.col)
      } else if (plot.cens == T & length(dat.cens) == 0) {
        stop ('There are no censored observations')
      } else {pl}
      pl <- if (theme_classic == T) {
        pl + theme_classic() +
          theme(
        axis.line.y = element_line(colour = "black", linetype = "solid"),
        axis.line.x = element_line(colour = "black", linetype = "solid"))
      } else {pl}
      pl
}
  pl <- if (strata == 1) {
    ggsurv.s(
      s,
      CI ,
      plot.cens,
      surv.col ,
      cens.col,
      lty.est,
      lty.ci,
      cens.shape,
      theme_classic,
      xlab,
      ylab,
      main
    )
  } else {
    ggsurv.m(
      s,
      CI,
      plot.cens,
      surv.col ,
      cens.col,
      lty.est,
      lty.ci,
      cens.shape,
      theme_classic,
      xlab,
      ylab,
      main
    )
  }
  pl
}
