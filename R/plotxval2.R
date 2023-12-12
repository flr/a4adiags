
# plotXval2 (FLIndices, list) {{{

#' Plot of FLIndices cross-validation by retrospective hindcast
#'
#' @param x An *FLIndices* object of the original observations.
#' @param y A list contaning *FLIndices* objects returned by *a4ahcxval*.
#' @param order Order in which retrospective runs are stored, defaults to"inverse".
#'
#' @return A ggplot object
#'
#' @examples
#' # SEE vignette

plotXval2 <- function(x, y="missing", order="inverse") {
  
  # SINGLE input and $data in y
  if(missing(y) & is.list(x) & "data" %in% names(x)) {
    y <- x[!names(x) %in% "data"]
    x <- x[["data"]]
  }
  
  # CHECK names of y, drop 'data'
  if("data" %in% names(y))
    y <- y[!names(y) %in% "data"]
  
  # TODO CHECK years of y and x
  
  # SUBSET x, if needed, by names of y
  if(length(y[[1]]) < length(x))
    x <- x[names(y[[1]])]
  
  # SET first year of plot as 1st of retro - 3
  y0 <- dims(x[[1]])$maxyear - length(y) - 2
  py <- do.call(seq, as.list(rev(dims(x[[1]])$maxyear - c(0, length(y)  - 2))))
  
  # CONVERT inputs to data.tables
  
  # Original FLIndices
  dato <- .dto(x, y0)
  
  # Hindcasted list(FLIndices)
  datp <- .dtp(y, y0)
  
  # CALCULATE mase
  if(order == "inverse")
    idr <- 1
  else if (order == "ahead")
    idr <- length(y)
  else {
    idx <- an(names(y)) == dims(x[[1]])$maxyear
    if(sum(idx) == 1)
      idr <- which(idr)
    else
      stop("Could not identify reference run (to last year).")
  }
  
  # CALCULATE mase, exclude ref run
  #><> Fix
  imase <- mase(x, y[-1], order=order)
  
  # GENERATE facet labels
  lbs <- unlist(lapply(seq(length(imase)), function(x)
    paste(names(imase)[x], "\nMASE:", format(imase[x], digits=3))))
  names(lbs) <- names(imase)
  llb <- names(y)
  llb[idr] <- paste(llb[idr], "(ref)")
  
  # LINES colors
  colors <- c(c("#0072B2", "#D55E00", "#009E73", "#56B4E9", "#E69F00", 
                "#D55E00", "#009E73", "#56B4E9", "#E69F00")[seq(length(llb)) - 1],
              "#000000")
  
  # PLOT
  p <- ggplot(datp, aes(x=year, y=data, colour=final)) +
    
    # data lines and points
    geom_line(data=dato, linetype=2, colour="gray") +
    geom_point(data=dato, colour="black", size=3) +
    geom_point(data=dato, colour="white", size=2.6) +
    geom_point(data=dato[year %in% py,], aes(colour=ac(year-1)), size=2.6) +
    
    # retro lines and hindcast point
    geom_line() + 
    geom_point(data=datp[year==pred, ]) +
    
    # format
    facet_wrap(~index, scales="free_y", ncol=2, labeller=as_labeller(lbs)) +
    xlab("") + ylab("") +
    scale_color_manual("", labels=rev(llb), values=colors) +
    theme(legend.position="bottom")
  
  return(p)
} 

.dtp <- function(flis, y0) {
  
  rbindlist(lapply(names(flis), function(i) {
    pred <- pmin(an(i) + 1, an(names(flis)[1]))
    data.table(cbind(as.data.frame(lapply(flis[[ac(i)]], function(x) {
      # COMPUTE Total abundance in biomass
      window(quantSums(index(x) * catch.wt(x)), start=y0, end=pred)
    }), drop=TRUE, qname="index"), final=i, pred=pred))}))
}

.dto <- function(flis, y0) {
  
  data.table(as.data.frame(lapply(flis, function(x) {
    dmns <- dimnames(x)
    if(all(is.na(catch.wt(x))))
      stop("catch.wt in FLIndex is NA, cannot compute biomass.")
    window(quantSums(index (x) * catch.wt(x)), start=y0)
  }), drop=TRUE, qname="index"))
  
}
# }}}
