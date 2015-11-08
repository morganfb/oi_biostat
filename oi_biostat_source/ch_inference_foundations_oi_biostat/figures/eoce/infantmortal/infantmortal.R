setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/infantmortal')

library(openintro)


pdf("infantmortal.pdf", height = 2.5, width = 4)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(infMortRate$infMortRate, col = COL[1], xlab = "Infant Mortality Rate", ylab = "")


dev.off()

