source("functions.R")

#### Figure 1 ####

plotit <- function(m=c(.3, .4, .525, .80), density=dBetam, shape, ylab="", xlab="", main="", ylim=c(0,1), xlim=c(0,1), legpos="top") {
  x=seq(.05,1,.01)
  col=rainbow(length(m))
  plot(x, density(x, m=m[length(m)], shape=shape), type="n", xlab=xlab, ylab=ylab,  main=main, xaxt="n")
  axis(1, at = c(.05, .10, .30, .50, .80, 1))
  for (i in 1:length(m)) {
    lines(x, density(1-x, m=1-m[i], shape=shape), col=col[i])
  }
  
  l=c()
  for (i in 1:length(m)) {
    l = c(l, paste0(m[i], " (", signif(dvar(density=density, m=m[i], shape=shape), digits=2), ")"))
  }
  if (!is.na(legpos)) {
    legend(legpos,
           c("Mean (var)",l),
           col=c(NA, col),
           lwd=c(NA,rep(1,length(m))),
           bty="n",
           y.intersp=.8)
  }
  
}

op <- par(no.readonly = TRUE)
par(mfcol=c(3,2), mar=c(5, 5.5, 2, 1), cex=.8)

plotit(shape=1, density=dBetam, xlab="Statistical power", ylab="density", main="S=1")
plotit(shape=1/2, density=dBetam, xlab="Statistical power", ylab="density", main="S=1/2")
plotit(shape=1/3, density=dBetam, xlab="Statistical power", ylab="density", main="S=1/3")
plotit(shape=1, density=dbimodal1090, xlab="Statistical power", ylab="density", main="S=1 (10/90)")
plotit(shape=2, density=dbimodal1090, xlab="Statistical power", ylab="density", main="S=2 (10/90)")
plotit(shape=1, density=dbimodal0595, xlab="Statistical power", ylab="density", main="S=1 (05/95)")

dev.copy(device = jpeg, filename = 'jpeg/figure1.jpg', quality=100, width = 700, height = 700)
dev.off()
