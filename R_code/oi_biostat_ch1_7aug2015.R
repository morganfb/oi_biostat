###  R code for open intro biostat

#####  Chapter 1

## create subset of famuss data, hopefully matching clarkson paper

## modify later to eliminate use of attach()

## clean up the approach to this; too roundabout
## Eliminate Am Indian as possible value

setwd("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data")
# load("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data/famuss.Rdata")

load("~/Dropbox/working_files/teaching/lecture_datasets/stat102_data/famuss.Rdata")

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
bmi = fms$pre.BMI
actn3.r577x = fms$actn3_r577x


famuss.oi.biostat.tmp = na.omit(data.frame(ndrm.ch, drm.ch, sex, age, race, height, weight, actn3.r577x, bmi))
famuss.oi.biostat = droplevels(famuss.oi.biostat.tmp)

table(famuss.oi.biostat$race)

#  save file using old famuss name for later consistency.  Original file available in stat 102 files

famuss = famuss.oi.biostat

setwd("~/oi_biostat/data/famuss")

save(famuss, file="famuss_09aug2015.Rdata")





library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
summary(famuss.oi.biostat$age~ famuss.oi.biostat$sex)
summary(famuss.oi.biostat$age~ famuss.oi.biostat$actn3.r577x)



library(xtable)

famuss[c(1,2,3,595),c("sex", "age", "race", "height", "weight", "actn3.r577x", "ndrm.ch")]

xtable(famuss[c(1,2,3,595),c( "sex", "age", "race", "height", "weight", "actn3.r577x", "ndrm.ch")],
       digits = 1, caption = "FAMuSSDF")

# scatterplots for chapter 1


plot(famuss$height, famuss$weight)
dev.copy(pdf,"~/oi_biostat/oi_biostat_source/ch_intro_to_data_oi_biostat/figures/famussHeightVsWeight/famussHeightVsWeight.pdf")
dev.off()

plot(famuss$age, famuss$ndrm.ch)

plot(famuss$height, famuss$bmi)
dev.copy(pdf,"~/oi_biostat/oi_biostat_source/ch_intro_to_data_oi_biostat/figures/famussHeightVsBmi/famussHeightVsBmi.pdf")
dev.off()

plot(famuss$height, famuss$bmi)
abline(lm(famuss$bmi ~ famuss$height))

#  Load diabetes dataset

setwd("~/oi_biostat/data/cdc_diabetes/excel")

diabetes.cdc.2012 = read.csv("diabetes_cdc_2012.csv")
setwd("~/oi_biostat/data/cdc_diabetes")
save(diabetes.cdc.2012, file="diabetes.cdc.2012.Rdata")


hist(diabetes.cdc.2012$percent.men.diabetes)
plot(diabetes.cdc.2012$percent.men.obese, diabetes.cdc.2012$percent.men.diabetes)
plot(diabetes.cdc.2012$percent.women.diabetes, diabetes.cdc.2012$percent.men.diabetes)

  
#  Load LEAP data

setwd("~/oi_biostat/data/leap/excel")

LEAP = read.csv("leap_demo_outcome.csv")
setwd("~/oi_biostat/data/leap")
LEAP[LEAP==""] = NA

LEAP = droplevels(LEAP)

# find children in the ITT analysis and with pos skin prick test

a = LEAP$intent.to.treat == "Yes" 


b = (LEAP$Baseline.Skin.Prick.Test == 0)

c = a & b

LEAP = LEAP[c,]

save(LEAP, file="LEAP.Rdata")

addmargins(table(LEAP$treatment.group, LEAP$outcome, useNA = "ifany"))


library(xtable)
xtable(LEAP[c(1,2,3,529, 530),c( "participant.ID", "treatment.group", "overall.V60.outcome")])

outcome.table = addmargins(table(LEAP$treatment.group, LEAP$overall.V60.outcome))

xtable(outcome.table, digits = 0, caption = "Leap Study Results", label = "peanutStudyResults")

#####

## 7 Aug revisions to R code make changes to leap data.  
##  This piece should be added to the R code in the repo
##   Revised LEAP data should be added to repo as well

#########


setwd("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data_working_dir/leap/excel")
load("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data_working_dir/leap/leap.Rdata")

names(LEAP)

table(LEAP$primary.ethnicity)
levels(LEAP$primary.ethnicity)[levels(LEAP$primary.ethnicity) == 
                                 "Chinese, Middle Eastern, or Other Ethnic Group"] <-
                                 "Other"


table(LEAP$primary.ethnicity)

setwd("~/Dropbox/working_files/teaching/open_intro/oi_biostat/data_working_dir/leap/")
save(LEAP, file="LEAP_7aug2015.Rdata")

table(LEAP$overall.V60.outcome, LEAP$primary.ethnicity)

table(LEAP$treatment.group)

a = LEAP$treatment.group == "Peanut Consumption"

table(LEAP$overall.V60.outcome[a], LEAP$primary.ethnicity[a])

b = LEAP$treatment.group != "Peanut Consumption"
table(LEAP$overall.V60.outcome[b], LEAP$primary.ethnicity[b])

## if using this for mosaic plot, better to look at avoidance group

#  Frog altitude study

####working with frog_altitude

## jv: please eliminate use of `attach'; I started that bad practice
## last year and it got students into trouble on the project
## also, we should insert the read.csv commands here for completeness, and
## so that we simply run the program to read and reproduce.

##  data files should be named .Rdata for consistency

setwd("~/oi_biostat/data/frogs/excel")
frog.altitude = read.csv("frog_altitude_data.csv")

setwd("~/oi_biostat/data/frogs")
save(frog.altitude, file="frog.altitude.Rdata")


###convert transformed variables into raw values

frog.altitude$clutch.size = 10^(frog.altitude$log.10.clutch.size)
frog.altitude$body.size = 10^(frog.altitude$log.10.body.size)
frog.altitude$clutch.volume = 10^(frog.altitude$log.10.clutch.volume)
frog.altitude$egg.size = 10^(frog.altitude$log.10.egg.size)

save(frog.altitude, file="frog.altitude.Rdata")

##subset altitude and latitude
## altitude_latitude = frog_altitude_data[,1:2]

#new dataset w/raw values
# frog_altitude_data_raw = cbind(altitude_latitude, raw.egg.size, raw.clutch.size, raw.clutch.volume, raw.body.size)

# detach(frog_altitude_data)

#  attach(frog_altitude_data_raw)

#double-check values w/summary stats in paper
altitude.3462 = frog.altitude[frog.altitude$altitude == "3,462.00", ]
mean(altitude.3462$clutch.size)
mean(altitude.3462$egg.size)
mean(altitude.3462$clutch.volume)
mean(altitude.3462$body.size, na.rm=TRUE)

boxplot(altitude.3462$egg.size)
boxplot(altitude.3462$clutch.size)

#data matrix for 1.2.1
frog.altitude[c(1:3, 150),] 

#  NA not showing up in the table

library(xtable)
xtable(frog.altitude[c(1:3,150),c( "altitude", "latitude", "egg.size", "clutch.size", 
                                          "clutch.volume", "body.size")], 
       caption = "Frog Study Data Matrix", label = "FrogAltitudeDF", digits = 2 )

setwd("~/oi_biostat/oi_biostat_source/ch_intro_to_data_oi_biostat/figures/clutchVolVsBodySize")

plot(frog.altitude$clutch.volume ~ frog.altitude$body.size)
dev.copy(pdf,"clutchVolVsBodySize.pdf")
dev.off()

library(openintro)
plot(frog.altitude$clutch.volume ~ frog.altitude$altitude)
plot(frog.altitude$body.size ~ frog.altitude$altitude)

with(frog.altitude, dotPlot(clutch.volume))
with(frog.altitude, clutch.volume[clutch.volume > 2000])

with(frog.altitude, median(clutch.volume))
with(frog.altitude, IQR(clutch.volume))
with(frog.altitude, mean(clutch.volume))
with(frog.altitude, sd(clutch.volume))

a = frog.altitude$clutch.volume <= 2000

with(frog.altitude, median(clutch.volume[a]))
with(frog.altitude, IQR(clutch.volume[a]))
with(frog.altitude, mean(clutch.volume[a]))
with(frog.altitude, sd(clutch.volume[a]))

# exploring famuss and frogs for robust statistic examples

library(openintro)
with(famuss.oi.biostat, hist(weight))

#  histogram shows several outliers

with(famuss.oi.biostat, dotPlot(weight))

with(famuss.oi.biostat, max(weight))

with(famuss.oi.biostat, weight[weight > 300])
with(famuss.oi.biostat, weight[weight > 270])
with(famuss.oi.biostat, median(weight))
with(famuss.oi.biostat, IQR(weight))
with(famuss.oi.biostat, mean(weight))
with(famuss.oi.biostat, sd(weight)) 

    
a = famuss.oi.biostat$weight <=270

with(famuss.oi.biostat, median(weight[a]))
with(famuss.oi.biostat, IQR(weight[a]))
with(famuss.oi.biostat, mean(weight[a]))
with(famuss.oi.biostat, sd(weight[a])) 


with(famuss.oi.biostat, dotPlot(weight))
plot.storage = file.path("~", "oi_biostat", "oi_biostat_source", "ch_intro_to_data_oi_biostat",
                         "figures", "famussWeightDotPlotRobustEx")
plot.storage
setwd(plot.storage)
pdf("famussWeightDotPlotRobustEx.pdf")
with(famuss.oi.biostat, dotPlot(weight))
dev.off()
