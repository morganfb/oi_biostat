library(openintro)
#data(famuss)
data(COL)

###OI method
tab <- table(famuss$race, famuss$actn3.r577x)
tab <- t(tab)

rp <- prop.table(tab, 1)
cp <- prop.table(tab, 2)

myPDF("famussSegBar.pdf",
      4.5,
      3.5,
      mar = c(2, 3, 0.5, 0.5),
      mgp = c(2.2, 0.6, 0))
barplot(apply(tab, 1, sum),
        col = COL[3])
tabA <- tab[,3]    ##this defines what column to plot in blue (3 = African Am)
names(tabA) <- NULL
barplot(tabA,
        col = COL[1],
        add = TRUE,
        axes = FALSE)
abline(h = 0)
#legend("topright",
       #fill = COL[c(1, 3)],
       #legend = c("Caucasian", "Non-Caucasian"))
dev.off()

myPDF("famussSegBarSta.pdf",
      4.5,
      3.5,
      mar = c(2, 2.5, 0.5, 0.5),
      mgp = c(2.2, 0.6, 0))
barplot(apply(tab, 1, sum) / apply(tab, 1, sum), col = COL[3])
tabA <- rp[, 3]
names(tabA) <- NULL
barplot(tabA,
        col = COL[1],
        add = TRUE,
        axes = FALSE)
abline(h = 0)
dev.off()

###New method

table(famuss$race, famuss$actn3.r577x)
race.genotype = matrix(table(famuss$race, famuss$actn3.r577x), ncol=5, byrow=T)
colnames(race.genotype)=c("African Am", "Asian", "Caucasian", "Hispanic", "Other")
rownames(race.genotype)=c("CC", "CT", "TT")

prop.race.genotype <- prop.table(race.genotype)

myPDF("famussSegBarTest.pdf",
      6,
      3.5,
      mar = c(2, 5, 0.5, 0.5),
      mgp = c(2.2, 0.6, 0))
#par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(race.genotype, col=COL[c(1, 2, 3)], ylim=c(0,500), width=2)
legend("topright", fill=COL[c(1, 2, 3)], legend=rownames(race.genotype))
dev.off()

#barplot with proportions, not actually "standardized"
myPDF("famussSegBarStaTest.pdf",
      6,
      3.5,
      mar = c(2, 5, 0.5, 0.5),
      mgp = c(2.2, 0.6, 0))
barplot(prop.race.genotype, col=COL[c(1,2,3)], ylim=c(0,0.8), width=2)
legend("topright",fill=COL[c(1,2,3)], legend=rownames(race.genotype))
dev.off()

table(famuss$actn3.r577x, famuss$race)
genotype.race = matrix(table(famuss$actn3.r577x, famuss$race), ncol=3, byrow=T)
colnames(genotype.race)=c("CC", "CT", "TT")
rownames(genotype.race)=c("African Am", "Asian", "Caucasian", "Hispanic", "Other")

#alternative barplot
myPDF("famussSegBarTestB.pdf",
      6,
      3.5,
      mar = c(2, 5, 0.5, 0.5),
      mgp = c(2.2, 0.6, 0))
#par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(genotype.race, col=COL[c(7, 4, 1, 2, 3)], ylim=c(0,300), width=2)
legend("topright", fill=COL[c(7, 4, 1, 2, 3)], legend=rownames(genotype.race))
dev.off()