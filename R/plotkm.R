plotkm <- function (formula, data, test = TRUE, xy.pvalue = NULL, conf.int = FALSE,
  times.print = NULL, nrisk.labels = NULL, legend = NULL,
  xlab = NULL, ylab = NULL, ylim = c(0, 1.02), left = 4.5,
  bottom = 5, cex.mtext = par("cex"), lwd = 2, lty = 1, col = NULL, cex=.5, ...)
{
  formula <- deparse(substitute(formula))
  formula <- paste(formula, collapse = " ")
  formula <- gsub("[[:space:]]+", " ", formula)
  varnames <- gsub("\\<Surv\\>|\\(|\\)| \\?\\(.*\\)", "",
    formula)
  varnames <- strsplit(varnames, "[[:space:]]*(\\+|,|~)[[:space:]]*")
  varnames <- gsub("[[:space:]]+", "", unlist(varnames))
  if (any(!I(setdiff(varnames, "1") %in% names(data)))) {
    stop(paste(paste(varnames, collapse = " and/or "), " not in ",
      deparse(substitute(data)), "\n", sep = ""))
  }
  temps = data[, varnames[1]]
  cens = data[, varnames[2]]
  if (varnames[3] == "1") {
    groupe = factor(rep("# at risk", nrow(data)))
    name_at_risk = ""
    test = FALSE
    legend = NULL
  }
  else {
    groupe = factor(data[, varnames[3]])
    if (length(levels(factor(data[, varnames[3]]))) == 1) {
      stop(paste(varnames[3], " has only one level\n",
        sep = ""))
    }
    name_at_risk = "# at risk"
  }
  d = data.frame(temps, cens, groupe)
  if (any(is.na(d$temps) | is.na(d$cens) | is.na(d$groupe))) {
    cat(paste(sum(is.na(d$temps) | is.na(d$cens) | is.na(d$groupe)),
      " rows deleted due to missing values\n", sep = ""))
  }
  d = d[I(!is.na(d$temps) & !is.na(d$cens) & !is.na(d$groupe)),
    ]
  if (is.null(xlab)) {
    xlab = varnames[1]
  }
  if (is.null(ylab)) {
    ylab = "Survival"
  }
  if (is.null(col)) {
    col = 1:nlevels(d$groupe)
  }
  par(mar = c(1 + nlevels(groupe) + bottom, left, 4, 3) +
    0.1, xaxs = "i", yaxs = "i")
  plot(survfit(Surv(temps, cens) ~ groupe, data = d), conf.int = conf.int,
    xlab = xlab, ylab = ylab, lwd = lwd, lty = lty, col = col,
    ylim = ylim, ...)
  if (!is.null(legend)) {
    legend(legend[1], if (length(legend) == 2) {
      legend[2]
    }
    else {
      NULL
    }, legend = levels(d$groupe), col = col, lwd = lwd,
      lty = lty, cex=cex)
  }
  if (is.null(times.print)) {
    times.print = axis(1, labels = FALSE, tick = FALSE)
    times.print = times.print[times.print >= 0]
  }
  n.risk = matrix(NA, nrow = 1 + nlevels(groupe), ncol = length(times.print),
    dimnames = list(c("temps", levels(groupe)), NULL))
  n.risk[1, ] = times.print
  for (lev in levels(groupe)) {
    tmp = d[d$groupe == lev, ]
    tmp2 = summary(survfit(Surv(temps, cens) ~ 1, data = tmp),
      times.print)$n.risk
    if (length(tmp2) < length(times.print)) {
      n.risk[lev, ] = c(tmp2, rep(0, length(times.print) -
        length(tmp2)))
    }
    else {
      n.risk[lev, ] = tmp2
    }
  }
  if (!is.null(nrisk.labels)) {
    rownames(n.risk)[-1] = nrisk.labels
  }
  range = range(axis(1, labels = FALSE, tick = FALSE))
  mtext(side = 1, at = -0.065 * (range[2] - range[1]) + range[1],
    line = 4, name_at_risk, cex = cex.mtext, adj = 1)
  for (i in 2:nrow(n.risk)) {
    mtext(side = 1, at = times.print, line = i + 3, n.risk[i,
      ], cex = cex.mtext)
    mtext(side = 1, at = -0.065 * (range[2] - range[1]) +
      range[1], line = i + 3, rownames(n.risk)[i], cex = cex.mtext,
      adj = 1)
  }
  if (test) {
    diff = survdiff(Surv(temps, cens) ~ groupe, data = d)
    p.value = 1 - pchisq(diff$chisq, df = length(levels(d$groupe)) -
      1)
    p.value = paste("Log-Rank p", ifelse(p.value < 0.001, "<0.001",
      paste("=", round(p.value, 3), sep = "")), sep = "")
    if (!is.null(xy.pvalue)) {
      text(xy.pvalue[1], xy.pvalue[2], p.value, adj = c(0,
        0), cex=cex)
    }
    else {
      text((range[2] - range[1])/20 + range[1], 0.05,
        p.value, adj = c(0, 0), cex=cex)
    }
  }
}
