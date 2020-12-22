# sol274.R - DESC
# /sol274.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLa4a)

# ORIGINAl objexcts from https://github.com/ices-taf/2020_sol.27.4_assessment

load("sol274.RData")

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

save(fit, stock, indices, file="../data/sol274.RData", compress="xz")
