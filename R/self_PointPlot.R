pdf(file = "Volcano_Total_42_PointPlot.pdf",height = 5,width = 4.25)
par(bty="l")
plot(x = subdata1$log2FoldChange,
     y = -log10(subdata1$pvalue),
     pch=19,
     cex=0.5,
     col=collist[1],
     xlim = c(-2,2),
     ylim = c(0,10),
     main = expression(paste("Total RNA (42",degree,"C)")),
     xlab = expression(paste("LFC of sg","m"^"6","A/sgCtrl")),
     ylab = substitute(paste("-Log"["10"],"(",italic('P'),"-value)")))
############################################################
abline(h=2,lty=2,lwd=2,col="gray")
points(x = updata$log2FoldChange,
       y = -log10(updata$pvalue),
       pch=19,cex=0.5,
       col=collist[2])

points(x = dodata$log2FoldChange,
       y = -log10(dodata$pvalue),
       pch=19,cex=0.5,
       col=collist[3])

legend("topleft",c("Unchange genes","Up-regulated genes","Down-regulated genes"),pch=19,col = collist,bty="n")

text(x=-1.5,y=2,labels = length(dodata$Row.names),col="gray41",cex=1.2,pos=3)
text(x= 1.5,y=2,labels = length(updata$Row.names),col="gray41",cex=1.2,pos=3)
