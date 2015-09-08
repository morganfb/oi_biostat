library(Hmisc)
brfss <- sasxport.get("~/Dropbox/THESIS/R scripts/brfss/CDBRFS00.XPT")
data(COL)

#create df of only columns we like
brfss.data<-cbind(brfss$x.state,brfss$age,brfss$weight,brfss$wtdesire,brfss$height,brfss$htf, brfss$hti,brfss$sex)
colnames(brfss.data)<-c("x.state","age","weight", "wtdesire", "height", "htf", "hti", "sex")
brfss.data<-as.data.frame(brfss.data)

#eliminate missing data 
brfss.df<-brfss.data[brfss.data$weight != 777 & brfss.data$weight != 999 & brfss.data$wtdesire != 777
                     & brfss.data$wtdesire != 999 & brfss.data$height!= 777 & brfss.data$height != 999 
                     & brfss.data$htf != 7 & brfss.data$htf != 9 & brfss.data$hti != 77 & brfss.data$hti != 99,]
brfss.df<-brfss.df[complete.cases(brfss.df),]

#calculate bmi
height.total<- brfss.df$htf * 12 + brfss.df$hti
weight.dif<-brfss.df$weight-brfss.df$wtdesire
bmi<-(brfss.df$weight * 703)/ (height.total^2)
brfss.df<-cbind(brfss.df,height.total,weight.dif,bmi)

# sample 40k from brfss data to use as our sample
set.seed(102)
sample.vec<-sample(1:nrow(brfss.df),size = 40000,replace = FALSE)
brfss.sample<-brfss.df[sample.vec,]

######################################################
library(openintro)
data(COL)
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/sampDistNormal')

N     <- 100000
means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), 100)
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)
}

myPDF('sampDistNormal.pdf', 8, 3.15,
      mar = c(3.3, 4, 1.4, 1),
      mgp = c(2.7,0.55,0))
layout(matrix(1:2, 1),
       c(4.3, 3))
plot(0, 0,
     type = 'n',
     xlim = c(24, 29),
     ylim = c(0, 1350 * N / 17000),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean", 1, 2)
mtext("Frequency", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10
rect(m - s / 100, 0,
     m + s / 100, 1350,
     col = '#00000044',
     border = '#00000000')
rect(m - s, 0,
     m + s, 135000,
     col = '#00000011',
     border = '#00000000')
rect(m - 2 * s, 0,
     m + 2 * s, 135000,
     col = '#00000011',
     border = '#00000000')
rect(m - 3 * s, 0,
     m + 3 * s, 135000,
     col = '#00000011',
     border = '#00000000')
histPlot(means, col = COL[1], breaks = 50, add = TRUE)
abline(h = 0)
axis(1)
axis(2, at = seq(0, 8000, 1000))

par(las = 1, mar = c(4, 4, 1.4, 1))
q <- c(seq(0.00001, 0.0009, 0.00001),
       seq(0.001, 0.999, 0.0001),
       seq(0.9991, 0.99999, 0.00001))
ms <- quantile(means, q)
nq <- qnorm(q)

plot(nq, ms,
     xlab = "",
     ylab = 'Sample means',
     main = '',
     col = COL[1],
     axes = FALSE)
mtext("Theoretical quantiles", 1, 2)
axis(1)
axis(2)
box()
dev.off()


