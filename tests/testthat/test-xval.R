# test-xval.R - DESC
# /test-xval.R

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

run <- stock + fit


# --- XVAL

library(doParallel)

if(Sys.info()["sysname"] %in% c("Darwin", "Linux")) {
  registerDoParallel(3)
} else {
  cl <- makeCluster(3)
  clusterEvalQ(cl = cl, expr = { library(FLa4a); library(FLasher) })
  registerDoParallel(cl)
}

# CALL xval

system.time(
res <- xval(stock, indices, nyears=5, nsq=3, srmodel=srmod, fmodel=fmod,
  qmodel=qmod, vmodel=vmod)
)

# COMPUTE MASE, BUT dropping 2019 prediction from res$indices.

mase(indices, res$indices[-1])

# PLOT, drop observations index 1.

plotXval(indices, res$indices)
