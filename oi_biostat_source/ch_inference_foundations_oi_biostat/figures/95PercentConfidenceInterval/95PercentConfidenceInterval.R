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
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/95PercentConfidenceInterval')

myPDF('95PercentConfidenceInterval.pdf', 6, 4,
      mar = c(2, 1, 1, 1),
      mgp = c(2.7, 0.9, 0))
m <- mean(brfss.df$bmi)
s <- sd(brfss.df$bmi)
n <- 100
k <- 25
SE <- s/sqrt(n)

set.seed(105)
means <- c()
SE    <- c()
for(i in 1:k){
  temp <- sample(nrow(brfss.df), n)
  d    <- brfss.df$bmi[temp]
  means[i] <- mean(d, na.rm = TRUE)
  SE[i]    <- sd(d)/sqrt(n)
}
xR <- m + 4 * c(-1, 1) * s / sqrt(n)
yR <- c(0, 41 * k / 40)
plot(xR, yR,
     type = 'n',
     xlab = 'BMI',
     ylab = '',
     axes = FALSE)
abline(v = m, lty = 2, col = COL[6])
axis(1, at = m, expression(mu*''),
     cex.axis = 1.15)
for(i in 1:k){
  ci <- means[i] + 2 * c(-1, 1) * SE[i]
  if(abs(means[i] - m) > 1.96 * SE[i]){
    col <- COL[4]
    points(means[i], i, cex = 1.4, col = col)
    lines(ci, rep(i, 2), col = col, lwd = 4)
  } else {
    col <- COL[1]
  }
  points(means[i], i, pch = 20, cex = 1.2, col = col)
  lines(ci, rep(i, 2), col = col)
}
dev.off()
