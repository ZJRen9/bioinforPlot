#' cdfplot for myself
#' @title self_cdfplot
#' @description made by ZJRen 2021,7,23
#' @details input:adata output:cdf
#'
#' @param adata adata is list
#' @param xlab ylab is str
#' @param main main is str
#' @param collist collist is atrray
#'
#' @import graphics
#'
#' @return a figure
#' @export
#' @examples a <- rnorm(n=100,mean = 0,sd=3);b <- rnorm(n=100,mean = 3,sd=3)
#' @examples adata <- list(a=a,b=b)
#' @examples self_cdfplot(adata = adata,collist = c("red","green"),main="",xlab="")
#'


self_cdfplot <- function(adata,collist,main,xlab){
  namelist <- names(adata)
  plot(x=NULL,y=NULL,xlim = c(min(unlist(adata)),max(unlist(adata))),ylim=c(0,1),ylab = "accumulation",main=main,xlab=xlab)
  i = 1
  for (valuelist in adata){
    cdf <- selfcdf(valuelist = valuelist)
    points(cdf$value,cdf$cdf,col=collist[i],pch=19,cex=0.5)
    i = i+1
  }
  print(namelist)
  legend("topleft",namelist,col = collist,pch=19,bty = "n")
}

selfcdf <- function(valuelist){
  valuelist <- valuelist[order(valuelist)]
  cdflist <- c()
  for (i in c(1:length(valuelist))){
    cdfvalue <- i/length(valuelist)
    cdflist <- c(cdflist,cdfvalue)
  }
  return(list(value=valuelist,cdfvalue=cdflist))
}












