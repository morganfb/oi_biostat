coal<-read.csv("~/OI_Biostat/data/coal_workers/coal_workers.csv")

dust.convert <-function(age, exposure){
  factor <- (age-18) * 1740/1000
  dust <- exposure/factor
  return(dust)
}
dust <- dust.convert(coal$AGE, coal$exp)
#get rid of infinites
dust[is.infinite(dust)]<-NA
dust<-dust[!is.na(dust)] #remove all the NAs
#############################################################################

library(openintro)
data(COL)
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/histOfDustExposure')

myPDF('histOfDustExposure.pdf', 8, 3.15,
      mfrow = 1:2,
      mar = c(4, 4, 0.5, 1.5),
      mgp = c(3, 0.55, 0))

histPlot(dust,
         xlab = 'Dust Exposure',
         ylab = '',
         col = COL[1],
         axes = FALSE)
axis(1, at = seq(0, 20, 5))
axis(2, at = seq(0, 3000, 500))
mtext('Freqency', side = 2, line = 2.3, las = 0)
par(mar = c(3.5, 4.5, 0.5, 0.5))
boxPlot(dust,
        main = "",
        ylab = "Dust Exposure",
        col = COL[1])

dev.off()
