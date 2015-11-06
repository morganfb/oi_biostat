library(openintro)
data(COL)
set.seed(2)
setwd('~/OI_Biostat/oi_biostat_source/ch_inference_foundations_oi_biostat/figures/pertussisHist')
plotdata <- function(regions = c("Lazio", "Lombardia","Sardegna",
                                 "Sicilia", "Toscana","Umbria"),
                     save_plot = T) {
  
  table_values <- mat.or.vec(length(regions),3)
  data <- read.csv("ItalyFormatted_Pertussis.csv")
  vac_data <- read.csv("ItalyFormatted_DTP3.csv")
  pop_data <- read.csv("ItalyFormatted_Population.csv")
  birth_data <- read.csv("ItalyFormatted_LiveBirths.csv")
  rownames(table_values) <- regions
  colnames(table_values) <- c("Mean_population", "Change_population",
                              "Mean_birth_rate")
  pop_data <- subset(pop_data,subset=Year>=1996&Year<=2009)
  birth_data <- subset(birth_data,subset=Year>=1996&Year<=2009)
  vac_data <- subset(vac_data,subset=Year>=1996&Year<=2009)
  
  for (region in regions) {
    
    
    table_values[region,"Mean_population"] <- mean(pop_data[,region])
    table_values[region,"Change_population"] <- max(pop_data[,region]) - min(pop_data[,region])
    table_values[region,"Change_population"] <- table_values[region,"Change_population"] / table_values[region,"Mean_population"]
    table_values[region,"Mean_birth_rate"] <- mean(birth_data[,region]/pop_data[,region])
    
    col_array <- c("black","red")
    
    if (save_plot) {
      #setwd(file.path(RDir,saveDir))
      pdf("pertussisHist.pdf",width=8,height=4)
    }
    
    par(mar = c(4,4,1,4),mgp=c(3,1,0),bty="l")
    par(cex=0.8,font.main=1)

    hist(data[,region],xlab="Monthly Vaccine Reports",col=COL[1], main = "", breaks = 20)
    
    par(new=TRUE)
    
    if (save_plot) {
      dev.off()
    }
    
  }
  return(table_values)
  setwd(file.path(RDir))
  
}

require(xtable)
require(pracma)
require(locfit)
require(plotrix)
require(pomp)


RDir <- getwd()

setwd(file.path(RDir))

#regions <- c("Lazio", "Lombardia", "Sicilia", 
#             "Toscana","Sardegna","Umbria")

regions <- c("Lombardia")

# # Generate TIFF versions
# source("DefinePlotFunctions_TIFF.R")

# Generate PDF files
#source("DefinePlotFunctions.R")


# Plot notification and vaccine coverage data -----------------------------

table_values <- plotdata(regions=regions)


