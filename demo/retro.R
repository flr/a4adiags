# retro.R - DESC
# /retro.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(sol274)

# WINDOW stocks and indices

stks <- lapply(setNames(nm=seq(2019, length=6, by=-1)), function(x)
  window(stock, end=x))

idxs <- lapply(seq(2019, length=6, by=-1), function(x)
  window(indices[c("BTS", "SNS")], end=x))

# SUBMODELS

# fmodel
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

# RUN sca
retrf <- mapply(function(...) do.call(sca, list(...)),
  stock=stks, indices=idxs, srmodel=list(srmod), fmodel=list(fmod),
  qmodel=list(qmod), vmodel=list(vmod), SIMPLIFY=FALSE)

# UPDATE stocks
retrs <- FLStocks(mapply("+", stks, retrf))

# COMPUTE mohnMatrix

mmats <- lapply(setNames(nm=c("fbar", "ssb", "rec")), mohnMatrix, stocks=retrs)

# CALCULATE mohn's rho

mrhos <- lapply(mmats, icesAdvice::mohn)
