load("publication_bias_data.RDta")

# Plot function #
plotit <- function(data, est, CIs, Rs, ylim=c(0,1), log="x", slabel="s2i", 
                   legpos="topleft", xlab="", ylab="", main="", inset=c(0.05, 0.05)) {
  
  theta = data$theta

  CIrev = c(CIs, NA, rev(CIs))
  
  col = c(rainbow(length(CIs)), "black", rev(rainbow(length(CIs))))
  lty = c(rep(2, length(CIs)), 1, rep(3, length(CIs)))
  lwd = c(rep(1, length(CIs)), 2, rep(1, length(CIs)))
 
  d=list()
  l=c()
  plot(theta, theta, type="n", ylim=ylim, xlab=xlab, ylab=ylab, log=log, main=main, xaxt="n")
  axis(1, at = c(.025, .05, .10, .20, .50, 1))
  
  for (i in 1:length(Rs)) {
    d[[i]] = data[[paste0("R", round(Rs[[i]], 2))]][[slabel]][[est]]
    missing = is.na(d[[i]])
    d[[i]] = d[[i]][!missing]
    t=theta[!missing]
    
    if (i == length(CIs)+1) {
      l=c(l, paste0(signif(Rs[[i]], 2)))
      
    }
    else {
      l=c(l, paste0(format(Rs[[i]], digits=2, nsmall=2), " (", CIrev[i]*100, ")" ))
    }
    lines(t, d[[i]], type="l", lwd=lwd[i], lty=lty[i], col=col[i])
    
  }

  if (!is.na(legpos)) {
    legend(legpos,
           c("R (CI %)", l),
           col=c(NA, col),
           lwd=c(NA, lwd),
           lty=c(NA, lty),
           bty="n",
           inset=inset,
           y.intersp=1.3,
           cex=.9
    )
  }
}

f<-function(c) c(binom.test(35, 97, 0, conf.level=c)$conf.int[1], binom.test(35, 97, 0, conf.level=c)$conf.int[2])
fv = Vectorize(f)
CIs = c(.95, .75, .50)
Rs=sort(c(35/97, fv(CIs)))
d=data$rep

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2), mar=c(5, 5.5, 2, 1), cex=.6)

plotit(d, "power", CIs, Rs, legpos="bottomleft",
       xlab="Assumed prior probability",
       ylab="Expected statistical power\nof the underlying research",
       main="Statistical power")

plotit(d, "posterior", CIs, Rs, legpos="bottomright", inset=c(.1, 0.05),
       xlab="Assumed prior probability",
       ylab="Expected posterior probability\nof the original findings",
       main="Posterior probability")

plotit(d, "positive", CIs, Rs, legpos="topleft", ylim=c(0,0.4), 
       xlab="Assumed prior probability",
       ylab="Expected probability of observing\npositive evidence in the underlying research",
       main="Positive evidence")

plotit(d, "bias", CIs, Rs, legpos="bottomleft", ylim=c(1,300), log="xy",
       xlab="Assumed prior probability",
       ylab="Odds of observed negative evidence\nto be supressed from publication",
       main="Publication bias")

par(op)

dev.copy(device = jpeg, filename = 'jpeg/figure3.jpg', quality=100, res=300, unit="mm", width = 150, height = 150)
dev.off()