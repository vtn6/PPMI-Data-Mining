#apply PCA Clustering to Biospecimen, Imaging 
#Think in terms of a basis function for parkinson's disease
#This code shows that parkinson's patients can be mostly described by the asymmetry of their striatal region and the values of their abeta 42 
#and alpha synuclein. In particular the basis function looks like this:
#(asymmetry, Abeta 42/Alphasynuclein), (asymmetry,-Abeta42/alphasynuclein)

#in other words, based on PC2 and PC2, there are 4 kinds of patient biospecimen profiles

#From PC2
##Those with high asymmetry, low  Total.tau and CSF.Alpha.synuclein (abeta 42 is not that important)
#Those with low asymmetry, high  Total.tau and CSF.Alpha.synuclein (abeta 42 is not that important)

#From PC3
#Those with low asymmetry, low  Total.tau and CSF.Alpha.synuclein (abeta 42 is not that important)
#Those with high asymmetry, high  Total.tau and CSF.Alpha.synuclein (abeta 42 is not that important)

#another interpretation is that any vector that isn't in the first few PCS can be ignored, as they represent a small populaiton of the data

rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
        
setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Biospecimen_Imaging_Clustering/")
ppmi.raw.data = read.csv("NMIB_AverageValues.csv")
ppmi.raw.data.csv = ppmi.raw.data
ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD',]
unique.patient.list = unique(ppmi.raw.data$PATNO) #vector of unique patient IDs

ppmi.raw.data = ppmi.raw.data[, 
                              c(which(names(ppmi.raw.data) %in% "CAUDATE_R"):ncol(ppmi.raw.data))] #take only columns to the right of np1cog

#adding ratio data
t.tau.Abeta.42.ratio = ppmi.raw.data$Total.tau/ppmi.raw.data$Abeta.42
p.tau.Abeta.42.ratio = ppmi.raw.data$p.Tau181P/ppmi.raw.data$Abeta.42
p.tau.t.tau.ratio = ppmi.raw.data$p.Tau181P/ppmi.raw.data$Total.tau



ppmi.raw.data = data.frame(ppmi.raw.data,
                  t.tau.Abeta.42.ratio,
                  p.tau.Abeta.42.ratio,
                  p.tau.t.tau.ratio)


ppmi.biospec.imaging.pca = prcomp(ppmi.raw.data,scale=TRUE)
pairs(ppmi.biospec.imaging.pca$x[,c(1:10)],cex = 0.1)

print("largest dimensions of Loading 1")
print(ppmi.biospec.imaging.pca$rotation[,1][order(abs(ppmi.biospec.imaging.pca$rotation[,1]),decreasing=TRUE)])

print("largest dimensions of Loading 2")
print(ppmi.biospec.imaging.pca$rotation[,2][order(abs(ppmi.biospec.imaging.pca$rotation[,2]),decreasing=TRUE)])

print("largest dimensions of Loading 3")
print(ppmi.biospec.imaging.pca$rotation[,3][order(abs(ppmi.biospec.imaging.pca$rotation[,3]),decreasing=TRUE)])


print("largest dimensions of Loading 4")
print(ppmi.biospec.imaging.pca$rotation[,4][order(abs(ppmi.biospec.imaging.pca$rotation[,4]),decreasing=TRUE)])

print("largest dimensions of Loading 5")
print(ppmi.biospec.imaging.pca$rotation[,5][order(abs(ppmi.biospec.imaging.pca$rotation[,5]),decreasing=TRUE)])

print("largest dimensions of Loading 6")
print(ppmi.biospec.imaging.pca$rotation[,6][order(abs(ppmi.biospec.imaging.pca$rotation[,6]),decreasing=TRUE)])

print("largest dimensions of Loading 7")
print(ppmi.biospec.imaging.pca$rotation[,7][order(abs(ppmi.biospec.imaging.pca$rotation[,7]),decreasing=TRUE)])
print("largest dimensions of Loading 8")
print(ppmi.biospec.imaging.pca$rotation[,8][order(abs(ppmi.biospec.imaging.pca$rotation[,8]),decreasing=TRUE)])
print("largest dimensions of Loading 9")
print(ppmi.biospec.imaging.pca$rotation[,9][order(abs(ppmi.biospec.imaging.pca$rotation[,9]),decreasing=TRUE)])



print("largest dimensions of Loading 6")
print(ppmi.biospec.imaging.pca$rotation[,6][order(abs(ppmi.biospec.imaging.pca$rotation[,6]),decreasing=TRUE)])

# figuring out which components of loading 2 are the most important compared to the others -------------
foo = ppmi.biospec.imaging.pca$rotation[,c(1:10)] #looking between the first 10

index.dimensions.of.PC6.that.with.largest.magnitude = c()
for(i in 1:nrow(foo))
{
  if(which.max(foo[i,]) == 6)
  {
    index.dimensions.of.PC6.that.with.largest.magnitude = c(index.dimensions.of.PC6.that.with.largest.magnitude,i)
  }
  
  if(which.min(foo[i,]) == 6)
  {
    index.dimensions.of.PC6.that.with.largest.magnitude = c(index.dimensions.of.PC6.that.with.largest.magnitude,i)
  } 
}
print("The Highest Magnitude dimensions (the most important features) of loading 6 compared to the other loadings")
print(ppmi.biospec.imaging.pca$rotation[,2][index.dimensions.of.PC6.that.with.largest.magnitude])
