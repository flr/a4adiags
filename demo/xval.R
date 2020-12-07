# a4a.R - DESC
# /a4a.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#         Ernesto JARDIM (EC JRC) <ernesto.jardim@ec.europa.eu>
#
# Distributed under the terms of the EUPL-1.2


library(a4adiags)

# LOAD data: FLStock + FLIndices

load("sol274_data.RData")

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

plot(run)


# --- XVAL

source("a4axval.R")

# OPTIONAL: USE parallel

library(doParallel)

# LINUX
registerDoParallel(3)

# WINDOWS
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {library(FLa4a); library(FLasher)})
registerDoParallel(cl)

# CALL xval

system.time(
res <- xval(stock, indices, nyears=5, nsq=3, srmodel=srmod, fmodel=fmod,
  qmodel=qmod, vmodel=vmod)
)

# COMPUTE MASE, BUT drop 2019 prediction from res$indices.

mase(indices, res$indices[-1])

# PLOT, drop observations index 1.

plotXval(indices, res$indices)

