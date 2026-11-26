# retro.R - Retrospective runs for a4a models
# a4adiags/R/retro.R

# Author: Ernesto Jardim <ernesto.jardim@ipma.pt>
#
# Distributed under the terms of the EUPL-1.2

# retro {{{ ----

#' Retrospective Analysis for FLa4a Assessments
#'
#' Runs a retrospective analysis on an existing `FLa4a` assessment fit. 
#' It automatically extracts submodels from the base fit, truncates the data sequentially, 
#' and refits the models. Knots (`k`) in `s(year, ...)` splines are dynamically reduced 
#' to avoid overparameterization on shortened time series, for fmodel and srmodel.
#'
#' @param stk An \code{FLStock} object containing the base stock data.
#' @param idxs An \code{FLIndices} object containing tuning indices.
#' @param fit An \code{a4aFitSA} object representing the base assessment fit.
#' @param peels Integer. The number of retrospective years to peel off. Default is 5.
#' @param kfrac Numeric. Fractional penalty applied to reduce knots in \code{s(year, ...)} terms for each year removed. Default is 0.3.
#'
#' @return An \code{FLStocks} object containing the refitted stock for the base run and each peel, named by terminal year.
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' data(ple4.index)
#' idxs <- FLIndices(BTS = ple4.index)
#' fit <- sca(ple4, idxs)
#' retros <- retro(stk = ple4, idxs = idxs, fit = fit, peels = 5)
#' }
 
retro <- function(stk, idxs, fit, peels = 5, kfrac = 0.3) {

  strip_formula <- function(mod) {
    as.formula(paste(deparse(formula(mod)), collapse = " "))
  }
  
  fmod_base  <- strip_formula(fmodel(fit))
  srmod_base <- strip_formula(srmodel(fit))
  n1mod      <- strip_formula(n1model(fit))
  qmod <- lapply(as.list(qmodel(fit)), strip_formula)
  vmod <- lapply(as.list(vmodel(fit)), strip_formula)

  # Helper function to reduce 'k' ONLY for s(year, ...) in fmodel and srmodel
  adjust_knots <- function(frm, x, kf) {
    frm_str <- paste(deparse(frm), collapse = " ")
    # Regex pattern: looks for 's(year' followed by 'k = '
    pattern <- "(s\\(\\s*year\\b[^)]*k\\s*=\\s*)([0-9]+)"
    m <- regexpr(pattern, frm_str, perl = TRUE)
    if (m > 0) {
      matched_str <- regmatches(frm_str, m)
      prefix <- sub(pattern, "\\1", matched_str, perl = TRUE)
      k_val  <- as.numeric(sub(pattern, "\\2", matched_str, perl = TRUE))
      new_k <- max(3, k_val - floor(x * kf)) # avoid reducing k below 3
      regmatches(frm_str, m) <- paste0(prefix, new_k)
    }
    as.formula(frm_str)
  }

  lst0 <- split(0:peels, 0:peels)
  lst0 <- lapply(lst0, function(x) {
    yr <- range(stk)["maxyear"] - x
    stk_sub <- window(stk, end = yr)
    idx_sub <- FLIndices(window(idxs, end = yr))
    
    fmod_sub  <- if(x > 0) adjust_knots(fmod_base, x, kfrac) else fmod_base
    srmod_sub <- if(x > 0) adjust_knots(srmod_base, x, kfrac) else srmod_base
    
    fit_sub <- sca(stk_sub, idx_sub, 
                   fmodel = fmod_sub, 
                   srmodel = srmod_sub, 
                   qmodel = qmod, 
                   n1model = n1mod, 
                   vmodel = vmod)
                   
    stk_sub + fit_sub
  })
  names(lst0) <- ac(seq(range(stk)["maxyear"],range(stk)["maxyear"] - peels))
  FLStocks(lst0)
}
# }}}
