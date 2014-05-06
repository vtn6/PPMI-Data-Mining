#apply PCA Clustering to Biospecimen, Imaging, and Motor Data for 
rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Motor_Clustering/")

ppmi.raw.data = read.csv("NMI_FilteredColumns_By_PATNO.csv")

#only take a subset because too many patients
#ppmi.raw.data = ppmi.raw.data[c(1:500),]

#remove the SCAU26AT and SCAßU26ATSCAU26CT
#ppmi.raw.data = subset.data.frame(ppmi.raw.data,select= -c(SCAU26CT,SCAU26AT,SCAU26DT,TMDISMED,CNTRLDSM))

ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD',]
unique.patient.list = unique(ppmi.raw.data$PATNO) #vector of unique patient IDs

ppmi.raw.data = subset.data.frame(ppmi.raw.data, select= -c(PATNO,
                                                            RECRUITMENT_CAT,
                                                            GENDER,
                                                            AGE_ASSESS_LNS,
                                                            AGE_ASSESS_SFTANIM,
                                                            AGE_ASSESS_JLO,
                                                            AGE_ASSESS_SFTANIM,
                                                            AGE_ASSESS_HVLT) )#remove age

ppmi.raw.data = ppmi.raw.data[, 
                              c(which(names(ppmi.raw.data) %in% "NP1COG"):ncol(ppmi.raw.data))] #take only columns to the right of np1cog

ppmi.motor.pca = prcomp(ppmi.raw.data,scale.=TRUE)
#pairs(ppmi.motor.pca$x[,c(1:15)],cex = 0.1)
#ppmi.motor.pca.scale.test = prcomp(scale(ppmi.raw.data))

#finding the highest magnitude characteristics in each principal component
print("largest dimensions of Loading 1")
print(ppmi.motor.pca$rotation[,1][order(abs(ppmi.motor.pca$rotation[,1]),decreasing=TRUE)])
print("largest dimensions of Loading 2")
print(ppmi.motor.pca$rotation[,2][order(abs(ppmi.motor.pca$rotation[,2]),decreasing=TRUE)])
print("largest dimensions of Loading 3")
print(ppmi.motor.pca$rotation[,3][order(abs(ppmi.motor.pca$rotation[,3]),decreasing=TRUE)])

for(i in 1:10)
{
  densityplot(ppmi.motor.pca$x[,i]) 
}
densityplot(ppmi.motor.pca$x[,2])
densityplot(ppmi.raw.data$CAUDATE_ASYMMETRY)
densityplot(ppmi.raw.data$PUTAMEN_ASYMMETRY)
densityplot(ppmi.raw.data$NP3RTALU)
densityplot(ppmi.raw.data$NP3PRSPL)
densityplot(ppmi.raw.data$NP3RIGLU)
densityplot(ppmi.raw.data$NP3TTAPL)
densityplot(ppmi.raw.data$NP3FTAPL)
densityplot(ppmi.raw.data$NP3HMOVL)
densityplot(ppmi.raw.data$NP3PRSPR)



# figuring out which components of loading 2 are the most important -------------
foo = ppmi.motor.pca$rotation[,c(1:10)] #looking between the first 10

index.dimensions.of.PC2.that.with.largest.magnitude = c()
for(i in 1:nrow(foo))
{
  if(which.max(foo[i,]) == 2)
  {
    index.dimensions.of.PC2.that.with.largest.magnitude = c(index.dimensions.of.PC2.that.with.largest.magnitude,i)
  }
  
  if(which.min(foo[i,]) == 2)
  {
    index.dimensions.of.PC2.that.with.largest.magnitude = c(index.dimensions.of.PC2.that.with.largest.magnitude,i)
  } 
}

print("The Highest Magnitude dimensions (the most important features) of loading 2 compared to the other loadings")
print(ppmi.motor.pca$rotation[,2][index.dimensions.of.PC2.that.with.largest.magnitude])

# thank you!
#http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
#--------------------------------------------------------------
  
# 
# plot3d(ppmi.motor.pca$x[,c(1:3)])
# text3d(ppmi.motor.pca$rotation[,1:3], texts=rownames(ppmi.motor.pca$loadings), col="red")
# coords <- NULL
# for (i in 1:3)
#   {
#   coords <- rbind(coords, rbind(c(0,0,0),ppmi.motor.pca$rotation[i,1:3]))
# }
# lines3d(coords, col="red", lwd=4)

       
#text3d(ppmi.motor.pca,[,1:3])
# for(i in 1:length(unique.patient.list))
# {
#   foo.patient = ppmi.raw.data[ppmi.raw.data$PATNO == unique.patient.list[i],]
#   
# }