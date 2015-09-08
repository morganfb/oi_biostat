#  get marginal distribution of gestational age

setwd("~/Dropbox/working_files/teaching/open_intro/oi_biostat/prob_chapter_draft")
load("~/Dropbox/working_files/teaching/open_intro/oi_biostat/prob_chapter_draft/birthwt_gestage/brthwt.gestage.overall.counts.Rdata")
View(brthwt.gestage.overall.counts)


gestage.marginal.counts = colSums(brthwt.gestage.overall.counts[,-1], na.rm = T)
View(gestage.marginal.counts)

gestage.marginal.dist.table = as.table(gestage.marginal.counts/sum(gestage.marginal.counts))

barplot(gestage.marginal.dist.table)

library(xtable)
xtable(gestage.marginal.dist.table)


