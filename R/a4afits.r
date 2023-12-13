
#' a4aBioidx 
#' Computes observed and expected FLIndexBiomass from a4a fits   
#'
#' @param stock Input FLStock object.
#' @param fit object a4a fit .
#' @param indices Input FLIndices object.
#' @param nyears Number if years for retrospective, defaults to 5.
#' @param nsq Number of years for average biology and selectivity, defaults to 3.
#' @param fixed.ks Is the number of knots is splines with 'year' constant?
#' @return FLIndexBiomass

#' @examples
#' data(sol274)
#'  # models
#' fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
#'   s(replace(age, age > 8, 8), k=4) +
#'   s(year, k=22, by=as.numeric(age==1))
#' qmod <- list(~s(age, k=3), ~s(age, k=3))
#' vmod <- list(~s(age, k=3), ~s(age, k=3), ~s(age, k=3))
#' srmod <- ~factor(year)
#' # RUN a4a
#' fit <- sca(stock, indices, fmodel=fmod, qmodel=qmod, vmodel=vmod,sr=srmod)
#' idxs = a4aBioidx(stock,fit,indices)
#' idxs$"BTS"@index # observed
#' idxs$"BTS"@index.q # fitted stored here

a4aBioidx <- function(stock,fit,indices){
    x <- stock+fit
    qs <- predict(fit)$qmodel

    bioidx <- FLIndices(Map(function(a, b) {
     
      # GET dims
      dmns <- dimnames(a)
      dis <- dims(a)
      timf <- mean(range(b)[c("startf", "endf")])
      #fy = dimnames(b)$year

      # EXTEND cachabilities if empty
      iobs= index(b)[, dimnames(b)$year]
      # COMPUTE predicted index
      index(b)[, dimnames(b)$year] <- (a * stock.n(x)[dmns$age, dimnames(b)$year] *
        exp(-z(x)[dmns$age, dimnames(b)$year] * timf))[, dimnames(b)$year]

      # STORE catchabilities
      index.q(b) <- a[, dimnames(b)$year]
      
      hat = quantSums(index(b)*x@stock.wt[dmns$age, dimnames(b)$year])
      
      obs = quantSums(iobs*x@stock.wt[dmns$age, dimnames(b)$year])
      units(hat) = "tonnes"
      
      
      bio =FLIndexBiomass(index=obs)
      bio@index.q[] = hat
      return(bio)
      
      }, a=qs, b=indices))
    
    return(bioidx)
} # end function   


#' plota4aBioidx {{{
#' Computes observed and expected FLIndexBiomass from a4a fits   
#'
#' @param stock Input FLStock object.
#' @param fit object a4a fit .
#' @param indices Input FLIndices object.
#' @param nyears Number if years for retrospective, defaults to 5.
#' @param nsq Number of years for average biology and selectivity, defaults to 3.
#' @param fixed.ks Is the number of knots is splines with 'year' constant?
#' @return ggplot
#' @examples
#' data(sol274)
#'  # models
#' fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
#'   s(replace(age, age > 8, 8), k=4) +
#'   s(year, k=22, by=as.numeric(age==1))
#' qmod <- list(~s(age, k=3), ~s(age, k=3))
#' vmod <- list(~s(age, k=3), ~s(age, k=3), ~s(age, k=3))
#' srmod <- ~factor(year)
#' # RUN a4a
#' fit <- sca(stock, indices, fmodel=fmod, qmodel=qmod, vmodel=vmod,sr=srmod)
#' idxs = a4aBioidx(stock,fit,indices)
#' plota4aBioidx(stock,fit,indices)

plota4aBioidx <- function(stock,fit,indices){
  idxs = a4aBioidx(stock,fit,indices)
  
  df=as.data.frame(test)
  df1 = df[df$slot%in%c("index"),]
  df2 = df[df$slot%in%c("index.q"),]
  
  p = ggplot(df2)+geom_line(aes(year,data),col=4)+
    geom_point(data=df1,aes(year,data),pch=21,fill="white",size=2)+
    geom_line(data=df1,aes(year,data),size=0.2,linetype=2)+
    facet_wrap(~cname,scales="free_y",ncol=1)+theme_bw()+
    ylab("Biomass Index")+xlab("Year")
  
  return(p)
}
