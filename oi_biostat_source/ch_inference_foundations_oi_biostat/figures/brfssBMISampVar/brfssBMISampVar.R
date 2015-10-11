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
sample.vec<-sample(1:nrow(brfss.df),size = 40,replace = FALSE)
brfss.sample<-brfss.df[sample.vec,]

set.seed(100)
sample.vec1<-sample(1:nrow(brfss.df),size = 40,replace = FALSE)
brfss.sample1<-brfss.df[sample.vec1,]

######################################################
library(openintro)
data(COL)
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/brfssBMISampVar')

xBars <- cumsum(brfss.sample$bmi)/ (1:length(brfss.sample$bmi))
xBars1 <-cumsum(brfss.sample1$bmi)/ (1:length(brfss.sample1$bmi))

myPDF('brfssBMISampVar.pdf', 7,2.5,
      mar=c(3.3, 4, 1, 1))

plot(xBars1[1:40],
     type = "l",
     axes = FALSE,
     xlab = "",
     ylab = "Running mean ",
     col = COL[1],
     ylim = c(24,32),
     lwd = 2)
lines(xBars[1:40],
     type = "l",
     axes = FALSE,
     xlab = "",
     ylab = "Running mean",
     col = COL[13],
     lwd = 2)

axis(1, at = seq(0, 40, 2))
axis(2, at = seq(0, 50, 1))
mtext("Sample size", 1, 2)

dev.off()


myPDF('brfssBMISampVar20.pdf', 7,3.5,
      mar=c(3.3, 4, 1, 1))
colors <- colorRampPalette(c(COL[7], COL[1]))(20)
set.seed(1)
sample.vec20<-sample(1:nrow(brfss.df),size = 40,replace = FALSE)
brfss.sample20<-brfss.df[sample.vec1,]
xBars20 = cumsum(brfss.sample20$bmi)/ (1:length(brfss.sample20$bmi))
plot(xBars20[1:40],
     type = "l",
     axes = FALSE,
     xlab = "",
     ylab = "Running means",
     ylim = c(15,45),
     col = COL[1],
     lwd = 2)
for (i in 2:20){ 
  set.seed(i)
  sample.vec20<-sample(1:nrow(brfss.df),size = 40,replace = FALSE)
  brfss.sample20<-brfss.df[sample.vec20,]
  xBars20 = cumsum(brfss.sample20$bmi)/ (1:length(brfss.sample20$bmi))
  lines(xBars20[1:40],
     type = "l",
     xlab = "",
     col = colors[i],
     lwd = 2)
}

axis(1, at = seq(0, 40, 2))
axis(2, at = seq(15, 45, 5))
mtext("Sample size", 1, 2)
dev.off()



