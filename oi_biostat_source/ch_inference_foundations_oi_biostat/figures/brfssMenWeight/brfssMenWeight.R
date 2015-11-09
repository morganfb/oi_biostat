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
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/brfssMenWeight')

set.seed(102)
men <- brfss.df[which(brfss.df$sex == 1),]

men.sample.vec<-sample(1:nrow(men),size = 40,replace = FALSE)
men.brfss.sample<-brfss.df[men.sample.vec,]

myPDF('brfssMenWeight.pdf', 5, 2.5,
      mar = c(4, 4, 1, 1),
      mgp = c(2.7,0.55,0))

hist(men.brfss.sample$weight, main = "", xlab = "Male Weight", col = COL[1])

dev.off()

ci <- c(mean(men.brfss.sample$weight) - 2.58* sd(men.brfss.sample$weight)/sqrt(length(men.brfss.sample$weight)),
        mean(men.brfss.sample$weight) + 2.58* sd(men.brfss.sample$weight)/sqrt(length(men.brfss.sample$weight)))
