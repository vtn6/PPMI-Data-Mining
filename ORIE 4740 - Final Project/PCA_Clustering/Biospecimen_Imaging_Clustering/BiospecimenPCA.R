#apply PCA Clustering to Biospecimen, Imaging 

# Developping Notes for May 7th -------------------------------------------
#Added Individual PCA analysis and clustering for both the DatSpect data and the CSF data

# Developping Notes For May 4th 2014 --------------------------------------
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

#for biospecimen data, we choose 5 because after 6 each PC holds less than the average amount of information (the ei
#genvalue is less than 1)

rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
library(sjPlot)
library(fpc)
library(mclust)
library(cluster)


setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Biospecimen_Imaging_Clustering/")
ppmi.raw.data.csv = read.csv("NMIB_AverageValues.csv") #use average values
ppmi.raw.data = ppmi.raw.data.csv

#adding ratio data
t.tau.Abeta.42.ratio = ppmi.raw.data$Total.tau/ppmi.raw.data$Abeta.42
p.tau.Abeta.42.ratio = ppmi.raw.data$p.Tau181P/ppmi.raw.data$Abeta.42
p.tau.t.tau.ratio = ppmi.raw.data$p.Tau181P/ppmi.raw.data$Total.tau

ppmi.raw.data = data.frame(ppmi.raw.data,
                           t.tau.Abeta.42.ratio,
                           p.tau.Abeta.42.ratio,
                           p.tau.t.tau.ratio)

#selecting PD patients
ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD',]

ppmi.biospecimen.imaging.data = subset.data.frame(ppmi.raw.data, select = c(CAUDATE_R,
                                                             CAUDATE_L,
                                                             PUTAMEN_R,
                                                             PUTAMEN_L,
                                                             CAUDATE_ASYMMETRY,
                                                             PUTAMEN_ASYMMETRY,
                                                             Abeta.42,
                                                             p.Tau181P,
                                                             Total.tau,
                                                             CSF.Alpha.synuclein,
                                                             t.tau.Abeta.42.ratio,
                                                             p.tau.Abeta.42.ratio,
                                                             p.tau.t.tau.ratio))

ppmi.CSF.data = subset.data.frame(ppmi.raw.data, select = c(Abeta.42,
                                                              p.Tau181P,
                                                              Total.tau,
                                                              CSF.Alpha.synuclein,
                                                              t.tau.Abeta.42.ratio,
                                                              p.tau.Abeta.42.ratio,
                                                              p.tau.t.tau.ratio))

ppmi.imaging.data = subset.data.frame(ppmi.raw.data, select = c(CAUDATE_R,
                                                                CAUDATE_L,
                                                                PUTAMEN_R,
                                                                PUTAMEN_L,
                                                                CAUDATE_ASYMMETRY,
                                                                PUTAMEN_ASYMMETRY))

#perform PCA on the whole biological dataset
ppmi.biospec.imaging.PCA = prcomp(ppmi.biospecimen.imaging.data,scale=TRUE)

plot(ppmi.biospec.imaging.PCA,
     type = "line",
     main = "Variances of each PCA loading")

sjp.pca(ppmi.biospec.imaging.PCA,
        plotEigenvalues = TRUE,
        type = "tile")

foo = sjp.pca(ppmi.biospecimen.imaging.data,
        plotEigenvalues = TRUE,
        hideLegend = FALSE,
        type = "tile")

# Model Based Clustering based on the first 5 Principal Components --------------------------------------------------

mydata = ppmi.biospec.imaging.PCA$x[,c(1:5)]
model_fit = Mclust(mydata)
plot(model_fit)
summary(model_fit)

# K-Means Clustering on the first 5 Principal Components ------------------------------------------------------
mydata = ppmi.biospec.imaging.PCA$x[,c(1:5)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



# Perform PCA on the CSF Data ---------------------------------------------

ppmi.CSF.pca = prcomp(ppmi.CSF.data,scale = TRUE)

sjp.pca(ppmi.CSF.pca,
        plotEigenvalues = TRUE,
        type = "circle")

# Model Based Clustering Based on the first 3 PCs -------------------------

mydata = ppmi.CSF.pca$x[,c(1:3)]
model_fit = Mclust(mydata)
plot(model_fit)
summary(model_fit)


# Perform PCA on DatSPECT data --------------------------------------------
ppmi.imaging.pca = prcomp(ppmi.imaging.data,scale = TRUE)

sjp.pca(ppmi.imaging.pca,
        plotEigenvalues = TRUE,
        type = "circle")


# Model Based Clustering Based on the first 2 or 3 PCs --------------------

mydata = ppmi.imaging.pca$x[,c(1:2)]
model_fit = Mclust(mydata)
plot(model_fit)
summary(model_fit)

# plotting in 3dd ---------------------------------------------------------
plot3d(ppmi.biospec.imaging.PCA$x[,c(1:3)],
       xlim = c(-10,10),
       ylim = c(-10,10),
       zlim = c(-10,10))
# plot3d(ppmi.biospec.imaging.PCA$x[,c(1:3)],
#        xlim = c(-10,10),
#        ylim = c(-10,10),
#        xlim = c(-10,10))