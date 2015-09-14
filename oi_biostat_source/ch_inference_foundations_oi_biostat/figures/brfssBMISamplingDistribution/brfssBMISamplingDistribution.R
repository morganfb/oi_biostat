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
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/brfssBMISamplingDistribution')

N <- 100000
means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), 40000)
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)
}

myPDF('brfssBMISamplingDistribution.pdf', 8, 3.15,
      mar = c(3.3, 4, 1.4, 1),
      mgp = c(2.7,0.55,0))

plot(0, 0,
     type = 'n',
     xlim = c(26.25, 26.45),
     ylim = c(0, 1350 * N / 15000),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean for n = 40,000", 1, 2)
mtext("Frequency", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10
histPlot(means, col = COL[1], breaks = 50, add = TRUE)
abline(h = 0)
axis(1, at = seq(26.25, 26.45, 0.05))
axis(2, at = seq(0, 10000, 1000))

dev.off()
