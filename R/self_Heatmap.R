#' Heatmap for myself
#' @title self_Heatmap
#' @description made by ZJRen 2021,7,23
#' @details input:adata output:box
#'
#' @param matrix_input Heatmap matrix
#' @param coltype coltype=c("YB","RB","BGR","GrayBlue")
#'
#' @import graphics
#' @import pheatmap
#' @import RColorBrewer
#'
#' @return a figure
#' @export
#' @examples matrix_test <- matrix(c(rnorm(100,mean = 1,sd = 2),rnorm(100,mean = 5,sd=3)),ncol = 5)
#' @examples colnames(matrix_test) <- paste("rep",c(1:5),sep = "")
#' @examples rownames(matrix_test) <- paste("gene",c(1:40),sep = "")
#' @examples self_Heatmap(matrix_input=matrix_test,coltype="YB")
#'


self_Heatmap <- function(matrix_input,coltype){
  library(pheatmap)
  library(RColorBrewer)
  normalized_matrix <- matrix_normalize(matrix_input)
  collist <- collist_make(coltype = coltype)
  pheatmap(normalized_matrix,color = collist,treeheight_row = 20,treeheight_col = 20)
}


collist_make <- function(coltype){
  if (coltype=="YB"){
    palette <- colorRampPalette(c("yellow3","white","darkblue"))
    collist <- palette(100)
  }
  if (coltype=="RB"){
    palette <- colorRampPalette((brewer.pal(n = 11,	name="RdBu")))
    collist <- palette(100)
  }
  if (coltype=="BGR"){
    palette <- colorRampPalette(c("DarkBlue","Blue","DeepSkyBlue","YellowGreen","Gold1","Red1","Red4"))
    collist <- palette(100)
  }
  if (coltype=="GrayBlue"){
    palette <- colorRampPalette(c("Gray91","DarkBlue"))
    collist <- palette(100)
  }
  return(collist)
}


matrix_normalize <- function(matrix_input){
  rowNumber = dim(matrix_input)[1];colNumber = dim(matrix_input)[2]
  colname_input <- colnames(matrix_input)
  rowname_input <- rownames(matrix_input)

  outarray <- c()
  for (i in c(1:rowNumber)){
    outarray <- c(outarray,scale(matrix_input[i,])[,1])
  }
  outmatrix <- matrix(outarray,ncol = colNumber,nrow = rowNumber,byrow = TRUE)
  colnames(outmatrix) <- colname_input
  rownames(outmatrix) <- rowname_input
  return(outmatrix)
}

nameselct <- function(orgnamelist,targetnamelist){
  outlist <- c()
  for (name in orgnamelist){
    name <- strsplit(name,"_")[[1]][2]
    if (name %in% targetnamelist){
      outlist <- c(outlist,name)
    }
    else{
      outlist <- c(outlist,"")
    }
  }
  return(outlist)
}












