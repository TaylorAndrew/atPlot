#' blandAltman
#'
#' blandAltman prints a Bland Altman plot
#'
#' @param x Continuous variable
#' @param y Continuous variable
#' @param data data.frame containing x and y
#' @param main Main title
#' @param xlab X-axis Label
#' @param ylab Y-axis Label
#' @param CIBand TRUE/FALSE: Plot a confidence interval
#' @param CIInterval What should the interval be at? Default is 95%CI. User input will be at +/- user input.
#' @param lty Line type
#' @param CIBand2 TRUE/FALSE: Plot a second confidence interval
#' @param CIInterval2 What should the interval be at? Default is 99%CI. User input will be at +/- user input
#' @param lty2 Line Type for CIBand2
#' @param baseline Should there be a 'baseline' line drawn. 'none' = no line, 'zero' = line at 0, 'bias' = line at the level of bias
#' @param standardized TRUE/FALSE: Should x and y be standardized?
#'
#' @return A Bland-Altman plot
#' @export
#'
#' @examples
#' #NULL
blandAltman <- function(x,
                        y,
                        data,
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        CIBand = FALSE,
                        CIInterval = NULL,
                        lty = "dashed",
                        CIBand2 = FALSE,
                        CIInterval2 = NULL,
                        lty2 = "dashed",
                        baseline = "zero",
                        standardized = FALSE) {
  std <- function(x)  sd(x, na.rm = T) / sqrt(length(x[!is.na(x)]))
  xstd = (data[, x] - mean(data[, x], na.rm = TRUE)) / sd(data[, x], na.rm = TRUE)
  ystd = (data[, y] - mean(data[, y], na.rm = TRUE)) / sd(data[, y], na.rm = TRUE)
  if (standardized == TRUE) {
    bamean = (xstd + ystd) / 2
    badiff = ystd - xstd
  }
  if (standardized == FALSE) {
    bamean = (data[, x] + data[, y]) / 2
    badiff = data[, y] - data[, x]
  }
  dat <- data.frame(xstd, ystd, bamean, badiff)
  some.plot <-
    plot(
      badiff ~ bamean,
      pch = 20,
      xlab = if (is.null(xlab)) {
        "mean"
      } else {
        xlab
      },
      ylab = if (is.null(ylab)) {
        "difference"
      }
      else {
        ylab
      }
    )
  # in the following, the deparse(substitute(varname)) is what retrieves the
  # name of the argument as data
  title(main = if (!is.null(main)) {
    main
  } else {
    paste0("Bland-Altman plot of ",
           y, " and ", x)
  },
  adj = ".5")
  #construct the reference lines on the fly: no need to save the values in new
  # variable names
  if (c("zero", "bias", "none") %in% baseline) {
    return(cat(paste0("baseline must be in c('zero', 'bias', 'none')")))
  }
  if ("zero" %in% baseline)
    abline(h = 0, lty = 1, lwd = 2)
  if ("bias" %in% baseline)
    abline(h = mean(badiff, na.rm = TRUE),
           lty = 2,
           lwd = 2)
  if (CIBand == T &
      is.null(CIInterval)) {
    abline(h = c(mean(badiff) + (1.96 * std(badiff)),
                 mean(badiff) - (1.96 * std(badiff))),
           lty = lty)
  }
  else if (CIBand == T &
           !is.null(CIInterval)) {
    abline(h = c(CIInterval, CIInterval * -1), lty = lty)
  }
  if (CIBand2 == T &
      is.null(CIInterval)) {
    abline(h = c(mean(badiff) + (2.576 * std(badiff)),
                 mean(badiff) - (2.576 * std(badiff))),
           lty = lty2)
  }
  else if (CIBand2 == T &
           !is.null(CIInterval)) {
    abline(h = c(CIInterval2, CIInterval2 * -1),
           lty = lty2)
  }
}
