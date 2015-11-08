setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/eoce/counties')

library(openintro)

asian <- countyComplete$asian
asian <- asian[!is.na(asian)]
pdf("countyasian.pdf", height = 2.5, width = 4)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(asian, col = COL[1], xlab = "Percent asian", ylab = "")


dev.off()


# plot population

pdf("countyasian.pdf", height = 3, width = 5.8)

par(mar=c(3.7,2,1,0.5), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25, cex.axis = 1.25)

histPlot(asian, col = COL[1], xlab = "Percent asian", ylab = "", axes = FALSE)
axis(1)

dev.off()

# sample n = 5

set.seed(123)
xbar = c()
for(i in 1:5000){
  sub = sample(asian, size = 5, replace = TRUE)
  xbar = c(xbar, mean(asian[sub]))
}
xbar5 = xbar

# plot xbar, n = 5

myPDF("countyasian_n5.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar5, col = COL[1], xlab = expression(bar(x)[" n = 5"]), ylab = "", axes = FALSE)

axis(1)

dev.off()

# sample n = 30

xbar = c()
for(i in 1:5000){
  sub = sample(asian, size = 30, replace = TRUE)
  xbar = c(xbar, mean(asian[sub]))
}
xbar30 = xbar

# plot xbar, n = 30

myPDF("countyasian_n30.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar30, col = oiB, xlab = expression(bar(x)[" n = 30"]), ylab = "", axes = FALSE)

axis(1)

dev.off()

# sample n = 100

xbar = c()
for(i in 1:5000){
  sub = sample(asian, size = 500, replace = TRUE)
  xbar = c(xbar, mean(asian[sub]))
}
xbar100 = xbar

# plot xbar, n = 100

myPDF("countyasian_n100.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar100, col = oiB, xlab = expression(bar(x)[" n = 100"]), ylab = "", axes = FALSE)

axis(1)

dev.off()


####################
#black
####################
black <- countyComplete$black
black <- black[!is.na(black)]
pdf("countyblack.pdf", height = 2.5, width = 4)

par(mar=c(3.7,2,1,1), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25)

histPlot(black, col = COL[1], xlab = "Percent black", ylab = "")


dev.off()

# plot population

pdf("countyblack.pdf", height = 3, width = 5.8)

par(mar=c(3.7,2,1,0.5), las=1, mgp=c(2.5,0.7,0), mfrow = c(1,1), cex.lab = 1.25, cex.axis = 1.25)

histPlot(black, col = COL[1], xlab = "Percent black", ylab = "", axes = FALSE)
axis(1)

dev.off()

# sample n = 5

set.seed(123)
xbar = c()
for(i in 1:5000){
  sub = sample(black, size = 5, replace = TRUE)
  xbar = c(xbar, mean(black[sub]))
}
xbar5 = xbar

# plot xbar, n = 5

myPDF("countyblack_n5.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar5, col = COL[1], xlab = expression(bar(x)[" n = 5"]), ylab = "", axes = FALSE)

axis(1)

dev.off()

# sample n = 30

xbar = c()
for(i in 1:5000){
  sub = sample(black, size = 30, replace = TRUE)
  xbar = c(xbar, mean(black[sub]))
}
xbar30 = xbar

# plot xbar, n = 30

myPDF("countyblack_n30.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar30, col = oiB, xlab = expression(bar(x)[" n = 30"]), ylab = "", axes = FALSE)

axis(1)

dev.off()

# sample n = 100

xbar = c()
for(i in 1:5000){
  sub = sample(black, size = 500, replace = TRUE)
  xbar = c(xbar, mean(black[sub]))
}
xbar100 = xbar

# plot xbar, n = 100

myPDF("countyblack_n100.pdf", 3, 2.4, mar=c(3.5,1,1,1), las=1, mgp=c(2.1,0.4,0))

histPlot(xbar100, col = oiB, xlab = expression(bar(x)[" n = 100"]), ylab = "", axes = FALSE)

axis(1)

dev.off()


####################
#white
####################
white <- countyComplete$white
white <- white[!is.na(white)]

####################
#white
####################
two_plus_races <- countyComplete$two_plus_races
two_plus_races <- two_plus_races[!is.na(two_plus_races)]



