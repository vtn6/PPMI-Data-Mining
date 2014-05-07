#Apply PCA to UPDRS Part 3 Motor Data

rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
library(sjPlot)
library(fpc)
library(mclust)
library(cluster)

setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Motor_Clustering/")
ppmi.raw.data.csv = read.csv("NMIB_AverageValues.csv") #use average values
ppmi.raw.data = ppmi.raw.data.csv

#selecting PD patients
ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD',]

# getting updrspart 3 frame -----------------------------------------------


NP3FieldNames = names(ppmi.raw.data)[substr(names(ppmi.raw.data),1,3) == 'NP3']
# ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
#                                         c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NP3RTCON"))]

ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
                                        NP3FieldNames]


# performing PCA ----------------------------------------------------------

ppmi.motor.NUPDRS3.pca = prcomp(ppmi.motor.NUPDRS3.data,scale = TRUE)

sjp.pca(ppmi.motor.NUPDRS3.pca,
        plotEigenvalues = TRUE,
        type = "circle")

# PCA shows that only the first 7 PCs satisfy kaiser criterion -----------
ppmi.motor.NUPDRS3.pca = prcomp(ppmi.motor.NUPDRS3.data,scale = TRUE)
mydata = ppmi.motor.NUPDRS3.pca$x[,c(1:7)]

# model based clustering on the first 7 components -----------------------

model_fit = Mclust(mydata)
plot(model_fit)
summary(model_fit)

# k-means clustering with the first 7 components -------------------------
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

plot3d(ppmi.motor.NUPDRS3.pca$x[,c(1:3)],
       xlim = c(-10,10),
       ylim = c(-10,10),
       zlim = c(-10,10))