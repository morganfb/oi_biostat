setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/frogs')
load("~/Dropbox/teaching/open_intro/oi_biostat/data_working_dir/data/ch1_intro_oi_biostat/frogs/frog_altitude.Rda")

library(openintro)

pdf("frogs.pdf", height = 3, width = 6)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(frog_altitude_data$bodysize, col = COL[1], xlab = "Body size of mother (cm)", ylab = "")

dev.off()

bodysize <- frog_altitude_data$bodysize
stats <- data.frame(summary(bodysize)[1:6])
names(stats)<-""
xtable(stats)

sd(bodysize)
bodysize.nona <- bodysize[!is.na(bodysize)]
se <- sd(bodysize.nona)/sqrt(length(bodysize.nona))
