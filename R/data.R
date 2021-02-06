# data.R - Example datasets for the a4adiags package
# a4adiags/R/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>

#' Data from the 2020 ICES stock assessment of North Sea sole (sol.27.4)
#' 
#' fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
#'   s(replace(age, age > 8, 8), k=4) +
#'   s(year, k=22, by=as.numeric(age==1))
#' 
#' qmod <- list(~s(age, k=3), ~s(age, k=3))
#' 
#' vmod <- list(
#'   ~s(age, k=3),
#'   ~s(age, k=3),
#'   ~s(age, k=3))
#' 
#' srmod <- ~factor(year)
#' 
#' @docType data
#' @keywords datasets
#' @aliases stock indices fit
#' @format Objects of class FLStock, FLIndices and a4aFitSA
#' @name sol274
#' @rdname data
NULL
