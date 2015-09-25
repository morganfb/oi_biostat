library(openintro)
data(COL)
data(poker)

myPDF('hospitalCanApplyNormalToSampMean.pdf', 5, 3,
      mar = c(3.7, 3.7, 0.7, 0.7),
      mgp = c(2.3, 0.6, 0))
histPlot(poker$winnings,
         xlab = 'Num. of Patients in Hospital (benchmarked to 5,000)',
         ylab = 'Frequency',
         col = COL[1])
dev.off()
