# data.R - Example datasets for the a4adiags package
# a4adiags/R/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>

#' Data from the 2020 ICES stock assessment of North Sea sole (sol.27.4)
#' 
#' Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
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
