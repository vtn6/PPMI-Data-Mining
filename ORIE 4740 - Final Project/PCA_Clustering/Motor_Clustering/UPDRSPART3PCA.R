#Apply PCA to UPDRS Part 3 Motor Data

rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
library(sjPlot)

setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Motor_Clustering/")
ppmi.raw.data.csv = read.csv("NMIB_AverageValues.csv") #use average values
ppmi.raw.data = ppmi.raw.data.csv

#selecting PD patients
ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD',]

#assuming that all UPDRSPart3 data is in the columns between NP3SPCH and NUPDRS_TOT
# ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
#                                 c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NUPDRS_TOT"))]



ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
                                        c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NP3RTALL"))]

ppmi.motor.NUPDRS3.pca = prcomp(ppmi.motor.NUPDRS3.data,scale = TRUE)

sjp.pca(ppmi.motor.NUPDRS3.pca,
        plotEigenvalues = TRUE,
        type = "circle")

plot3d(ppmi.motor.NUPDRS3.pca$x[,c(1:3)],
       xlim = c(-10,10),
       ylim = c(-10,10),
       zlim = c(-10,10))