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
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/clt30')

myPDF('clt30.pdf', 8, 6,
      mfrow = c(2,2),
      mar = c(3.3, 4, 1.4, 1),
      mgp = c(2.7,0.55,0))
 
ns = c(5, 10, 20, 30)
N     <- 100000

#for n = 5
means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), ns[1])
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)}

plot(0, 0,
     type = 'n',
     xlim = c(18, 36),
     ylim = c(0, 0.18),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean for n=5", 1, 2)
mtext("Density", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10
histPlot(means, col = COL[1], breaks = 50, probability = TRUE, add = TRUE)
curve(dnorm(x,mean=mean(means), sd=sqrt(var(means))),col="darkblue", lwd=2, add = TRUE)
abline(h = 0)
axis(1)
axis(2, at = seq(0, 0.18, 0.05))
#new graph
#n=10
par(las = 1, mar = c(4, 4, 1.4, 1))

means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), ns[2])
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)}

plot(0, 0,
     type = 'n',
     xlim = c(20, 33),
     ylim = c(0, 0.25),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean for n=10", 1, 2)
mtext("Density", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10
histPlot(means, col = COL[1], breaks = 50, probability = TRUE, add = TRUE)
curve(dnorm(x,mean=mean(means), sd=sqrt(var(means))),col="darkblue", lwd=2, add = TRUE)
abline(h = 0)
axis(1)
axis(2, at = seq(0, 0.25, 0.05))

par(las = 1, mar = c(4, 4, 1.4, 1))

#new graph
#n=20
means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), ns[3])
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)}

plot(0, 0,
     type = 'n',
     xlim = c(22, 31),
     ylim = c(0, 0.35),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean for n=20", 1, 2)
mtext("Density", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10
histPlot(means, col = COL[1], breaks = 50,probability = TRUE, add = TRUE)
curve(dnorm(x,mean=mean(means), sd=sd(means)),col="darkblue", lwd=2, add = TRUE)
abline(h = 0)
axis(1)
axis(2, at = seq(0, 0.35, 0.05))

par(las = 1, mar = c(4, 4, 1.4, 1))
#new graph
#n=30
means <- rep(0, N)
pb <- txtProgressBar(0, N, style = 3)
for (i in 1:N) {
  temp <- sample(nrow(brfss.df), ns[4])
  means[i] <- mean(brfss.df$bmi[temp], na.rm = TRUE)
  setTxtProgressBar(pb, i)}

plot(0, 0,
     type = 'n',
     xlim = c(22, 30),
     ylim = c(0, 0.42),
     xlab = '',
     ylab = '',
     axes = FALSE)
mtext("Sample mean for n=30", 1, 2)
mtext("Density", 2, line = 3, las = 0)
m <- mean(brfss.df$bmi, na.rm = TRUE)
s <- sd(brfss.df$bmi, na.rm = TRUE) / 10

histPlot(means, col = COL[1], breaks = 50,probability = TRUE, add = TRUE)
curve(dnorm(x,mean=mean(means), sd=sd(means)),col="darkblue", lwd=2, add = TRUE)
abline(h = 0)
axis(1)
axis(2, at = seq(0, 0.42, 0.05))

dev.off()


