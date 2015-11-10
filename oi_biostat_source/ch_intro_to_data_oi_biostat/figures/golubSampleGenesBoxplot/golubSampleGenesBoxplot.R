library(hddplot)

data(Golub)
data(golubInfo)


#  running test files from the hddplot package

golub.trans = t(Golub)
golub.exprs.pheno = cbind(golubInfo, golub.trans)
golub.exprs.pheno.subtable = golub.exprs.pheno[1:5,]

boxplot(golub.exprs.pheno[,7:9])

