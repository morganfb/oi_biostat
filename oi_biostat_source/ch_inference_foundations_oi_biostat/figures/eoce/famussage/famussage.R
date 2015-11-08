setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/famussage')
load("~/Dropbox/teaching/open_intro/oi_biostat/data_working_dir/data/ch1_intro_oi_biostat/famuss/famuss_09aug2015.Rdata")

library(openintro)

names(famuss)

pdf("famussage.pdf", height = 2.5, width = 4)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(famuss$age, col = COL[1], xlab = "Age", ylab = "")


dev.off()

mean(famuss$age)
sd(famuss$age)
length(famuss$age)
se <- sd(famuss$age)/sqrt(length(famuss$age))
