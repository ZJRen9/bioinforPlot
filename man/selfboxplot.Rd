% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/self_boxplot.R
\name{selfboxplot}
\alias{selfboxplot}
\title{selfboxplot}
\usage{
selfboxplot(adata, ylab, main, collist)
}
\arguments{
\item{adata}{adata is list}

\item{ylab}{ylab is str}

\item{main}{main is str}

\item{collist}{collist is atrray}
}
\value{
a figure
}
\description{
made by ZJRen 2021,7,23
}
\details{
boxplot for myself

input:adata output:box
}
\examples{
adata=list(a=rnorm(n=10,mean=0,sd=1),b=rnorm(n=10,mean=2,sd=1),c=rnorm(n=10,mean=5,sd=1));
selfboxplot(adata=adata,ylab="value",main="main",collist=rainbow(3))

}
