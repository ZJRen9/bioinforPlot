#' boxplot for myself
#' @title selfboxplot_with_jitter
#' @description made by ZJRen 2021,7,23
#' @details input:adata output:box
#'
#' @param adata adata is list
#' @param ylab ylab is str
#' @param main main is str
#' @param collist collist is atrray
#' @param width width is point width
#'
#' @import graphics
#'
#' @return a figure
#' @export
#' @examples adata=list(a=rnorm(n=10,mean=0,sd=1),b=rnorm(n=10,mean=2,sd=1),c=rnorm(n=10,mean=5,sd=1));
#' @examples selfboxplot_with_jitter(adata=adata,ylab="value",main="main",collist=rainbow(3),width=0.2)
#'
#'



selfboxplot_with_jitter <- function(adata,ylab,main,collist,width){
  boxnumber <- length(adata)
  namelist <- names(adata)
  boxplot(adata,names = FALSE,ylab=ylab,main=main,col=collist,outline = FALSE)
  borderlist <- par("usr")
  for (i in c(1:boxnumber)){
    text(x = i,y = borderlist[3]-(borderlist[4]-borderlist[3])*0.05,xpd=TRUE,srt=30,labels = namelist[i])
  }
  for (i in c(1:boxnumber)){
    value_list <- adata[[i]]
    jitter_list <- jitter(rep(i,length(value_list)),amount = width)
    points(x = jitter_list,y = value_list,pch=19,col="black",cex=0.5)
  }
}








