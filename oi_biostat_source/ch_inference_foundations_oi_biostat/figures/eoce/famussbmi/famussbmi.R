setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/famussbmi')
load("~/Dropbox/teaching/open_intro/oi_biostat/data_working_dir/data/ch1_intro_oi_biostat/famuss/famuss_09aug2015.Rdata")

library(openintro)


pdf("famussbmi.pdf", height = 3, width = 6)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(famuss$bmi, col = COL[1], xlab = "BMI", ylab = "")


dev.off()

bmi <- famuss$bmi
mean(bmi)

ci95 <- c(mean(bmi)-1.96*sd(bmi)/sqrt(length(bmi)),mean(bmi)+1.96*sd(bmi)/sqrt(length(bmi)))

ci90 <- c(mean(bmi)-1.65*sd(bmi)/sqrt(length(bmi)),mean(bmi)+1.65*sd(bmi)/sqrt(length(bmi)))
