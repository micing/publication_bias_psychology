load("publication_bias_data.RDta")

plotit <- function(d, condition, shapes, est, log="x", ylab="", xlab="", main="", ylim=c(0,1), xlim=c(0,1), legpos="topright") {
  plot(d[["c1"]][["zero"]]$theta, d[["c1"]][["zero"]][[est]], type="n", ylim=ylim, xlab=xlab, ylab=ylab, log=log, main=main, xaxt="n")
  axis(1, at = c(.025, .05, .10, .20, .50, 1))
  col=rainbow(length(shapes))
  
  for (i in 1:length(shapes)) {
    lines(d[[condition]][[shapes[i]]]$theta, d[[condition]][[shapes[i]]][[est]], col=col[i])
  }
    
  if (!is.na(legpos)) {
    legend(legpos,
           shapes,
           col=col,
           lwd=c(1),
           bty="n",
           cex=.5
    )
  }
  
}

d=data$main
shapes = c("zero", "s50", "s1", "s2i", "s3i", "s1b1090", "s2b1090", "s1b0595")
est=c("power", "posterior", "positive", "bias")
legpos=c("topright", "bottomright", "topright", "topleft")
ylim=list(c(0,1), c(0,1), c(0,1), c(0,300) )
ylab=c("Expected statistical power","Expected posterior probability", "Expected positive evidence", "Publication bias" )
main=c("Replication = same power", "Replication = 100% power", "Lower likely (+6%)", "Upper likely (+10%)")

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2), mar=c(5, 5.5, 2, 1), cex=1)

for (i in 1:length(est)) {
  for (j in 1:length(main)){
  plotit(d=d, condition=paste0("c", j), est=est[i], shapes=shapes, xlab="Assumed prior", ylab=ylab[i], main=main[j], legpos=legpos[i], ylim=ylim[[i]])
  }
  dev.copy(device = jpeg, filename = paste0("jpeg/figureS", i, ".jpg"), quality=100, width = 700, height = 700)
  dev.off()
}

op <- par(no.readonly = TRUE)
