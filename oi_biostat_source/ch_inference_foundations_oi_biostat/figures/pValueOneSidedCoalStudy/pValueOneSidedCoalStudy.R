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

######################################################
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/pValueOneSidedCoalStudy')
library(openintro)
data(COL)

myPDF('pValueOneSidedCoalStudy.pdf', 6.75, 2.64,
      mar = c(2, 0, 0.5, 0),
      mgp = c(3, 0.65, 0))
normTail(U = 2.18,
         col = COL[1],
         xlim = c(-3, 4),
         axes  =  FALSE,
         lwd  =  2)
at <- c(-5, 0, 0.37 / 0.17, 5)
labels <- expression(0, H[0]*': '*mu*' = 2  ',
                     bar(dust)*' = 2.20', 0)
axis(1, at, labels, cex.axis = 1)
yMax <- 0.4

arrows(2.5, yMax / 2,
       2.5, yMax / 10,
       length = 0.1,
       col = COL[1],
       lwd = 1.5)
text(2.5, yMax / 2, 'p-value',
     pos = 3,
     cex = 1.2,
     col = COL[1])
dev.off()
