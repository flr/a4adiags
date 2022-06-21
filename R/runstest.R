# runstest.R - DESC
# a4adiags/R/runstest.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

globalVariables(c("pass", "p.value", "qname", "age", "lcl", "ucl", "outlier"))

# runstest {{{

#' Computes Runs Test p-values
#'
#' @param fit The result of a model fit.
#' @param obs The observations used in the fit.
#' @param combine Should ages be combined by addition, defaults to TRUE.
#' @param ... Extra arguments.
#'
#' @return A list with elements 'p.values' and 'pass'.
#'
#' @examples
#' data(sol274)
#' # Call on a a4aFitSA object
#' runstest(fit, indices)
#' #
#' runstest(index(fit), lapply(indices, index))
    
setMethod("runstest", signature(fit="a4aFitSA", obs="FLIndices"),
  function(fit, obs, combine=TRUE) {
    
    # EXTRACT index fit
    fit <- index(fit)
    
    # EXTRACT index observations
    obs <- lapply(obs[names(fit)], index)
    
    return(runstest(fit, obs, combine=combine))
  }
)
# }}}

# plotRunstest {{{

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
#' plotRunstest(fit, indices)

setMethod("plotRunstest", signature(fit="a4aFitSA", obs="FLIndices"),
  function(fit, obs, combine=TRUE) {

    # EXTRACT index fit
    fit <- index(fit)

    # EXTRACT index observations
    obs <- lapply(obs[names(fit)], index)

    plotRunstest(fit, obs, combine=combine)
  }
)

#' @rdname plotRunstest

setMethod("plotRunstest", signature(fit="FLQuant", obs="FLQuant"),
  function(fit, obs, combine=TRUE) {

    fit <- FLQuants(A=fit)
    obs <- FLQuants(A=obs)

    plotRunstest(fit, obs, combine=combine) +
      theme(strip.text = element_blank(), strip.background = element_blank())
  }
)
# }}}
