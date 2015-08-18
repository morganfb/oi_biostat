##### General organizational notes #####
# -- organized by dataset, in order of appearance (LEAP > frog > famuss)
# for each dataset, there are two main sections: setting up & tables/figures
# tables are commented with their label in the text
# figures are commented with section name, because "figures" here actually refers to calculations, etc.
# code to generate figures is stored in the figures directory


# JV:  File contains 07Aug revisions to LEAP data, but I did not execute it or update LEAP data, 
#      preferred to LEAP manipulations to you.


##### LEAP ######

### setting up LEAP data

# load LEAP data
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


### tables/figures

## leapStudyresultsDF
library(xtable)
xtable(LEAP[c(1,2,3,529, 530),c( "participant.ID", "treatment.group", "overall.V60.outcome")])

## leapStudyresultsDF
library(xtable)
outcome.table = addmargins(table(LEAP$treatment.group, LEAP$overall.V60.outcome))
xtable(outcome.table, digits = 0, caption = "Leap Study Results", label = "leapStudyResults")





##### frog #####

### setting up frog data

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
# attach(frog_altitude_data_raw)

#double-check values w/summary stats in paper
altitude.3462 = frog.altitude[frog.altitude$altitude == "3,462.00", ]
mean(altitude.3462$clutch.size)
mean(altitude.3462$egg.size)
mean(altitude.3462$clutch.volume)
mean(altitude.3462$body.size, na.rm=TRUE)

boxplot(altitude.3462$egg.size)
boxplot(altitude.3462$clutch.size)


### tables/figures

#frogAltitudeDF
frog.altitude[c(1:3, 150),] 

library(xtable)
xtable(frog.altitude[c(1:3,150),c( "altitude", "latitude", "egg.size", "clutch.size", 
                                   "clutch.volume", "body.size")], 
       caption = "Frog Study Data Matrix", label = "frogAltitudeDF", digits = 2 )

setwd("~/oi_biostat/oi_biostat_source/ch_intro_to_data_oi_biostat/figures/clutchVolVsBodySize")

#measures of spread
d <- frog.altitude$clutch.volume; round(mean(d),1); 
d[c(1,2,3,431)]; d[c(1,2,3,431)] - round(mean(d),1); 
(d[c(1,2,3,431)] - round(mean(d)))^2; sum((d - round(mean(d)))^2)/49; 
sqrt(sum((d - round(mean(d)))^2)/49); 
var(d); sd(d)

#robust statistics
library(openintro); data(frog.altitude); 
d <- frog.altitude$clutch.volume; median(d); 
IQR(d); mean(d); sd(d)

library(openintro); data(frog.altitude); d <- frog.altitude$clutch.volume; 
a <- d<= 2000; median(d[a]); 
IQR(d[a]); mean(d[a]); sd(d[a])

#histograms
hist(frog.altitude$clutch.volume, breaks = 14, plot = FALSE)






##### famuss #####

###setting up famuss data

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



### tables/figures

#famussDF
library(xtable)
xtable(famuss[c(1,2,3,595),c( "sex", "age", "race", "height", "weight", "actn3.r577x", "ndrm.ch")],
       digits = 1, caption = "FAMuSSDF")

#famussFrequencyTable
library(openintro); library(xtable); data(famuss); a = addmargins(table(famuss$actn3.r577x)); 
genotype.table = matrix(a, ncol=4, byrow=T); colnames(genotype.table) = c("CC", "CT", "TT", "Sum"); 
rownames(genotype.table) = "Counts"; xtable(genotype.table, digits = 0, caption = "A frequency table for the actn3.r577x variable.", label = "famussFrequencyTable")

#famussContingencyTable
library(xtable); data(famuss); genotype.by.race.table = addmargins(table(famuss$race, famuss$actn3.r577x)); 
xtable(genotype.by.race.table, digits = 0, caption = "A contingency table for race and actn3.r577x.", label = "famussContingencyTable")

#famussRowPropTable
library(xtable); data(famuss); row.prop.table = table(famuss$race, famuss$actn3.r577x)[1:5,]; row.prop.table / rep(rowSums(row.prop.table), 3); 
rowSums(row.prop.table); xtable(genotype.by.race.table, digits = 0, caption = "A contingency table with row proportions for the race and actn3.r577x variables.", label = "famussRowPropTable"); 
173/595; 261/595; 161/595

#famussColPropTable
library(xtable); data(famuss); col.prop.table = table(famuss$race, famuss$actn3.r577x)[1:5,]; col.prop.table / rep(colSums(col.prop.table), 3); 
colSums(col.prop.table); xtable(genotype.by.race.table, digits = 0, caption = "A contingency table with column proportions for the race and actn3.r577x variables.", label = "famussColPropTable"); 
27/595; 55/595; 467/595; 23/595; 23/595
