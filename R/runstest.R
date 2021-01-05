# runstest.R - DESC
# a4adiags/R/runstest.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

globalVariables(c("runstest", "p.value", "qname", "age", "lcl", "ucl", "outlier"))

# sigma3 (FLQuant) {{{

#' Compute the 3-sigma limits and the corresponding p-value
#'
#' @param x An object of class FLQuant.
#' @param mixing Alternative hypothesis to be tested by snpar::runs.test. One of "two.sided", "less" (default) or "greater".
#'
#' @return A list with elements 'lcl', 'ucl' and 'p.value'.
#'
#' @examples
#' data(sol274)
#' sigma3(catch(stock))

sigma3 <- function(x, mixing="less") {

  # COMPUTE average moving rate

  mr <- abs(diff(x - 0))
  amr <- mean(mr, na.rm=TRUE)
  
  # COMPUTE upper limit for moving ranges

  ulmr <- 3.267 * amr

  # REMOVE moving ranges greater than ulmr and recalculate amr, Nelson 1982

  mr  <- c(mr)[c(mr) < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  # Calculate standard deviation, Montgomery, 6.33

  stdev <- amr / 1.128

  # Calculate control limits
  lcl <- -3 * stdev
  ucl <- 3 * stdev

  if(nlevels(factor(sign(x))) > 1) {

    # Make the runs test non-parametric
    y = ifelse(x < 0, -1, 1)
    runstest = runs.test(c(y), alternative = mixing)
    pvalue = round(runstest$p.value, 3)

  } else {
    pvalue = 0.001
  }
 
return(list(lcl = lcl, ucl = ucl, p.value = pvalue))

} # }}}

# plotRunsTest {{{

#' Plot the runs test result for one or more time series
#'
#' @param fit The result of a model fit.
#' @param obs The observations used in the fit.
#' @param combine Should ages be combined by addition, defaults to TRUE.
#' @param ... Extra arguments.
#'
#' @return An object of class ggplot2::gg
#'
#' @examples
#' data(sol274)

setGeneric("plotRunsTest", function(fit, obs, ...)
		standardGeneric("plotRunsTest"))

#' @rdname plotRunsTest

setMethod("plotRunsTest", signature(fit="FLQuants", obs="missing"),
  function(fit, combine=TRUE) {

  # COMBINE
  if(combine) {
    fit <- lapply(fit, quantSums)
  }

  # RESIDUALS
  res <- fit

  # CREATE data.frame
  dat <- data.table(as.data.frame(res))

  # sigma3, by index
  if(combine) {
    s3s <- lapply(res, sigma3)
  # or index and age
  } else {
    s3s <- lapply(res, function(x) {
      rbindlist(lapply(divide(x, 1), sigma3), idcol="age")
    })
    # CONVERT numeric age to character for merge() below
    dat[, age := as.character(age)]
  }

  s3dat <- rbindlist(lapply(s3s, as.data.frame), idcol="qname")
  
  # p.value >= 0.05 -> TRUE, green
  s3dat[, runstest:=p.value >= 0.05]

  # FIND single limits for all indices
  lims <- c(min=min(unlist(lapply(res, dims, c("minyear")))),
    max=max(unlist(lapply(res, dims, c("maxyear")))))
  
  # MERGE s3dat into dat
  if(combine)
    dat <- merge(dat, s3dat[, list(qname, lcl, ucl)], by=c('qname'))
  else {
    dat <- merge(dat, s3dat[, list(age, qname, lcl, ucl)], by=c('qname', 'age'))
  }

  # ADD limits to colour outliers
  dat[, outlier:=data < lcl | data > ucl]
  
  # PLOT
  p <- ggplot(dat) +
    geom_rect(data=s3dat, aes(xmin=lims[1] - 1, xmax=lims[2] + 1,
      ymin=lcl, ymax=ucl, fill=runstest), alpha=0.8) +
    scale_fill_manual(values=c("TRUE"="#cbe368", "FALSE"="#ef8575")) +
    geom_hline(yintercept=0, linetype=2) +
    geom_segment(aes(x=year, y=0, xend=year, yend=data), na.rm=TRUE) +
    geom_point(aes(x=year, y=data), size=1.5, na.rm=TRUE) +
    geom_point(aes(x=year, y=data, colour=outlier), size=1, na.rm=TRUE) +
    scale_colour_manual(values=c("FALSE"="#ffffff", "TRUE"="#d50102")) +
    xlab("") + ylab("Residuals") +
    theme(legend.position="none")

  if(combine)
    p <- p + facet_grid(qname ~ .)
  else
    p <- p + facet_grid(factor(age, levels=order(unique(age))) ~ qname,
      scale="free_y")

  return(p)
  }
)

setMethod("plotRunsTest", signature(fit="FLQuants", obs="FLQuants"),
  function(fit, obs, combine=TRUE) {
  
  # COMBINE
  if(combine) {
    fit <- lapply(fit, quantSums)
    obs <- lapply(obs, quantSums)
  }

  # RESIDUALS
  res <- FLQuants(mapply(residuals, obs, fit, SIMPLIFY=FALSE))

  return(plotRunsTest(res, combine=combine))
  }
)

#' @rdname plotRunsTest

setMethod("plotRunsTest", signature(fit="a4aFitSA", obs="FLIndices"),
  function(fit, obs, combine=TRUE) {

    # EXTRACT index fit
    fit <- index(fit)

    # EXTRACT index observations
    obs <- lapply(obs[names(fit)], index)

    plotRunsTest(fit, obs, combine=combine)
  }
)

#' @rdname plotRunsTest

setMethod("plotRunsTest", signature(fit="FLQuant", obs="FLQuant"),
  function(fit, obs, combine=TRUE) {

    fit <- FLQuants(A=fit)
    obs <- FLQuants(A=obs)

    plotRunsTest(fit, obs, combine=combine) +
      theme(strip.text = element_blank(), strip.background = element_blank())
  }
)
# }}}
