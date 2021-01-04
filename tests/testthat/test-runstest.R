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

wts <- lapply(indices, catch.wt)

plotRunsTest(index(fit) * wts, wts * lapply(indices, index))


plotRunsTest(index(fit) , lapply(indices, index))

plotRunsTest(fit, indices)

plotRunsTest(fit, indices) +
  scale_fill_manual(values=c("TRUE"="cyan", "FALSE"="pink"))

plotRunsTest(fit, indices) +
    scale_colour_manual(values=c("FALSE"="yellow", "TRUE"="green"))

plotRunsTest(index(fit)['SNS'], lapply(indices['SNS'], index), combine=FALSE)
plotRunsTest(index(fit)[['SNS']], lapply(indices['SNS'], index)[[1]], combine=TRUE)



