load("publication_bias_data.RDta")

# Plot function #
plotit <- function(d, est, ylim=c(0,1), log="x", legpos="topleft", xlab="", ylab="", main="", inset=c(0.05, 0.05)) {
  theta=d$theta

  d1=d$outer[[est]]$max
  d2=d$outer[[est]]$min
  d3=d$alt[[est]]$max
  d4=d$alt[[est]]$min
  d5=d$likely[[est]]$max
  d6=d$likely[[est]]$min

  zeroOnTop = d$zero[[est]]$c3 > d$extreme[[est]]$c3
  d7 = d$zero[[est]]$max * zeroOnTop + d$zero[[est]]$min * !zeroOnTop
  d8 = d$extreme[[est]]$min * zeroOnTop + d$extreme[[est]]$max * !zeroOnTop
  
  plot(theta, d1, type="n", ylim=ylim, xlab=xlab, ylab=ylab, log=log, main=main, xaxt="n")
  axis(1, at = c(.025, .05, .10, .20, .50, 1))
  
  polygon(c(theta, rev(theta)), c(d1, rev(d2)), col="grey90", border=NA)
  polygon(c(theta, rev(theta)), c(d3, rev(d4)), col="grey70", border=NA)
  polygon(c(theta, rev(theta)), c(d5, rev(d6)), col="black", border=NA)
  
  lines(theta, d7, type="l", lwd=1, lty=3, col="red")
  lines(theta, d8, type="l", lwd=1, lty=2, col="red")
  
  if (!is.na(legpos)) {
    legend(legpos,
           c("Likely range", "Alternative variances", "Zero variance", "Extreme variance", "Outer boundary"),
           col=c("black", "grey70", "red", "red", "grey90"),
           lwd=c(4, 4, 1, 1, 4),
           lty=c(1, 1, 3, 2, 1),
           bty="n",
           y.intersp=1.1,
           inset=inset,
           cex=.8
    )
  }
}

d=data$range
op <- par(no.readonly = TRUE)
par(mfrow=c(2,2), mar=c(5, 5.5, 2, 1), cex=.6)

plotit(d, "power",  legpos="topright", inset=c(0.1, 0.05),
       xlab="Assumed prior probability",
       ylab="Expected statistical power\nof the underlying research",
       main="Statistical power")

plotit(d, "posterior",  legpos="topleft",
       xlab="Assumed prior probability",
       ylab="Expected posterior probability\nof the original findings",
       main="Posterior probability")

plotit(d, "positive",  legpos="topleft", ylim=c(0,0.4), 
       xlab="Assumed prior probability",
       ylab="Expected probability of observing\npositive evidence in the underlying research",
       main="Positive evidence")

plotit(d, "bias",  legpos="bottomleft", ylim=c(1,250), log="xy",
       xlab="Assumed prior probability",
       ylab="Odds of observed negative evidence\nto be supressed from publication",
       main="Publication bias")

par(op)

dev.copy(device = jpeg, filename = 'jpeg/figure2.jpg', quality=100, res=300, unit="mm", width = 150, height = 150)
dev.off()