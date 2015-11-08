setwd("~/Desktop/Open Intro/EOCE - Second Edition/04/figures/eoce/gifted")
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/gifted/')
library(openintro)
data(gifted)

oiB = "#3E9BC0"

###

pdf("gifted_count_hist.pdf", height = 3, width = 6)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25, cex.axis = 1.25)

histPlot(gifted$count, col = oiB, xlab = "Age when the child first counted to 10 successfully (in months)", ylab = "")

dev.off()

###

pdf("gifted_count_pval.pdf", height = 3, width = 6)

par(mar=c(2,0,0,0), las=1, mgp=c(3,1,0), mfrow = c(1,1))

m = 32
s = 4.31/sqrt(36)
l = 30.69
u = 32 + (32-30.69)

normTail(m = m, s = s, L = l, U = u, axes = FALSE, col = oiB)
axis(1, at = c(m - 3*s,l,m,u,m + 3*s), label = c(NA,l,m,u,NA), cex.axis = 1.5)

dev.off()

###

pdf("gifted_momIQ_hist.pdf", height = 3, width = 6)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25, cex.axis = 1.25)

histPlot(gifted$motheriq, col = oiB, xlab = "Mother's IQ", ylab = "", axes = FALSE)
axis(1)
axis(2, at = c(0,4,8,12))

dev.off()

###

pdf("gifted_momIQ_pval.pdf", height = 3, width = 6)

par(mar=c(2,0,0,0), las=1, mgp=c(3,1,0), mfrow = c(1,1))

m = 100
s = 6.5/sqrt(36)
u = 118.2
l = 81.8

normTail(m = m, s = s, U = u, L = 81.8, axes = FALSE, col = oiB, xlim = c(80,120))
axis(1, at = c(m - 3*s,l,m,u,m + 3*s), label = c(NA,l,m,u,NA), cex.axis = 1.55)

dev.off()


########################################

pdf("parentIQ.pdf", height = 3, width = 6)
par(mar=c(4, 3, 1.4, 1), las=1, mgp=c(3,1,0), mfrow = c(1,2))

histPlot(gifted$motheriq, col = COL[1], xlab = "Mother's IQ", ylab = "", ylim=c(0,14))

par(las = 1, mar = c(4, 3, 1.4, 1))
histPlot(gifted$fatheriq, col = COL[1], xlab = "Father's IQ", ylab = "", ylim=c(0,14))

dev.off()

#difference CI
mean(gifted$motheriq)
mean(gifted$fatheriq)

parent.diff <- gifted$motheriq - gifted$fatheriq
sd(parent.diff)/sqrt(length(parent.diff))
