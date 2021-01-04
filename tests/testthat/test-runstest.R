# test-runstest.R - DESC
# a4adiags/test/testthat/test-runstest.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# LOAD data: FLStock + FLIndices

data("sol274")


# --- SET UP model

# fmodel, mimics AAP

fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
  s(replace(age, age > 8, 8), k=4) +
  s(year, k=22, by=as.numeric(age==1))

# qmodel (GAM, SNS)

qmod <- list(~s(age, k=3), ~s(age, k=3))

# vmodel (catch, GAM, SNS)

vmod <- list(
  ~s(age, k=3),
  ~s(age, k=3),
  ~s(age, k=3))

# srmodel

srmod <- ~factor(year)

# fit

fit <- sca(stock, indices[c("BTS", "SNS")],
  srmodel=srmod, fmodel=fmod, qmodel=qmod, vmodel=vmod)

# PLOT for a4aFitSA and FLIndices, by total numbers

plotRunsTest(fit , indices, combine=TRUE) +
  ggtitle("SOL.27.4 - Total numbers")

# PLOT for a4aFitSA and FLIndices, by numbers at age

plotRunsTest(fit , indices, combine=FALSE) +
  ggtitle("SOL.27.4 - Numbers by age")

# PLOT by total biomass

wts <- lapply(indices, catch.wt)

plotRunsTest(index(fit) * wts, wts * lapply(indices, index)) +
  ggtitle("SOL.27.4 - Total biomass")

# CHANGE fill colors

plotRunsTest(fit, indices) +
  scale_fill_manual(values=c("TRUE"="cyan", "FALSE"="pink"))

# CHANGE pont colors

plotRunsTest(fit, indices) +
    scale_colour_manual(values=c("FALSE"="green", "TRUE"="red"))

# CALL on single index

plotRunsTest(index(fit)$SNS, index(indices$SNS), combine=FALSE)
plotRunsTest(index(fit)$SNS, index(indices$SNS), combine=TRUE)
