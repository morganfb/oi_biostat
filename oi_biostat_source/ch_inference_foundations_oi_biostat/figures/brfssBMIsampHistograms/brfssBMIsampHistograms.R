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

######################################################
library(openintro)
data(COL)
source("~/figures/brfssBMIsampHistograms.R", chdir = TRUE)
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/brfssBMIsampHistograms')

myPDF("brfssBMIsampHistograms.pdf", 7, 3,
      mfrow = 1:2,
      mar = c(4, 4, 0.5, 1.5),
      mgp = c(3, 0.55, 0))
histPlot(brfss.sample$bmi,
         main = "",
         xlab = "BMI",
         ylab = "Frequency",
         col = COL[1])
par(mar = c(3.5, 4.5, 0.5, 0.5))
boxPlot(brfss.sample$bmi,
         main = "",
         ylab = "BMI",
         col = COL[1])
dev.off()