###  R code for open intro biostat

#####  Chapter 1

## create subset of famuss data, hopefully matching clarkson paper

## modify later to eliminate use of attach()

## clean up the approach to this; too roundabout
## Eliminate Am Indian as possible value

setwd("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data")
load("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data/famuss.Rdata")


## still have to eliminate Am Indian as a value of race



## eliminate cases with any missing data
## change var names consistent with our convention

id = fms$id
ndrm.ch = fms$NDRM.CH
drm.ch = fms$DRM.CH
sex = fms$Gender
age = fms$Age
race = fms$Race
height = fms$Pre.height
weight = fms$Pre.weight
actn3.r577x = fms$actn3_r577x


famuss.oi.biostat.tmp = na.omit(data.frame(ndrm.ch, drm.ch, sex, age, race, height, weight, actn3.r577x))
famuss.oi.biostat = droplevels(famuss.oi.biostat.tmp)

table(famuss.oi.biostat$race)

save(famuss.oi.biostat, file="famuss.oi.biostat.Rdata")





library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
summary(famuss.oi.biostat$age~ famuss.oi.biostat$sex)
summary(famuss.oi.biostat$age~ famuss.oi.biostat$actn3.r577x)



library(xtable)

famuss.oi.biostat[c(1,2,3,595),c("sex", "age", "race", "height", "weight", "actn3.r577x", "ndrm.ch")]

xtable(famuss.oi.biostat[c(1,2,3,595),c( "sex", "age", "race", "height", "weight", "actn3.r577x", "ndrm.ch")],digits = 1)



