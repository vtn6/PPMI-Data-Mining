#Apply PCA to Motor Data

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

#assuming that all motor data is in the columns between NP1COG and NUPDRS_TOT
ppmi.motor.data = ppmi.raw.data[,
                                c(which(names(ppmi.raw.data) %in% "NP1COG"):which(names(ppmi.raw.data) %in% "NUPDRS_TOT"))]

ppmi.motor.pca = prcomp(ppmi.motor.data,scale = TRUE)

sjp.pca(ppmi.motor.pca,
        plotEigenvalues = TRUE,
        type = "circle")
                        
