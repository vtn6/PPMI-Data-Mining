#Apply PCA to UPDRS Part 3 Motor Data AND Imaging Data

rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
library(sjPlot)
#setwd("C://Users/cit-labs/Dropbox/ORIE 4740 - Final Project/PCA_Clustering/Motor_Biospecimen_Imaging_Clustering/")
setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/Motor_Biospecimen_Imaging_Clustering/")
ppmi.raw.data.csv = read.csv("NMIB_ByVisits.csv") #use average values
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
ppmi.raw.data = ppmi.raw.data[ppmi.raw.data$RECRUITMENT_CAT == 'PD' & ppmi.raw.data$EVENT_ID == 'SC' ,]


# #assuming that all UPDRSPart3 data is in the columns between NP3SPCH and NUPDRS_TOT
# ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
#                                         c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NUPDRS_TOT"))]


#assuming that all UPDRSPart3 data is in the columns between NP3SPCH and NUPDRS_TOT
NP3FieldNames = names(ppmi.raw.data)[substr(names(ppmi.raw.data),1,3) == 'NP3']
# ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
#                                         c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NP3RTCON"))]

ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
                                        NP3FieldNames]
# Adding bio data ---------------------------------------------------------


ppmi.biospecimen.imaging.data = subset.data.frame(ppmi.raw.data, select = c(CAUDATE_R,
                                                                            CAUDATE_L,
                                                                            PUTAMEN_R,
                                                                            PUTAMEN_L,
                                                                            CAUDATE_ASYMMETRY,
                                                                            PUTAMEN_ASYMMETRY))


# ppmi.biospecimen.imaging.data = subset.data.frame(ppmi.raw.data, select = c(CAUDATE_R,
#                                                                             CAUDATE_L,
#                                                                             PUTAMEN_R,
#                                                                             PUTAMEN_L,
#                                                                             CAUDATE_ASYMMETRY,
#                                                                             PUTAMEN_ASYMMETRY,
#                                                                             Abeta.42,
#                                                                             p.Tau181P,
#                                                                             Total.tau,
#                                                                             CSF.Alpha.synuclein,
#                                                                             t.tau.Abeta.42.ratio,
#                                                                             p.tau.Abeta.42.ratio,
#                                                                             p.tau.t.tau.ratio))

ppmi.NUPDRS3.Biospecimen.Imaging.data = data.frame(ppmi.motor.NUPDRS3.data,
                                                   ppmi.biospecimen.imaging.data)

# Performing PCA ----------------------------------------------------------
ppmi.NUPDRS3.Biospecimen.Imaging.pca = prcomp(ppmi.NUPDRS3.Biospecimen.Imaging.data,scale = TRUE,retx=TRUE,center = TRUE)

heatmap(ppmi.NUPDRS3.Biospecimen.Imaging.pca$rotation[,c(1:5)],
        Rowv=NA,
        Colv=NA,
        col = cm.colors(256),
        scale="column",
        margins=c(5,10))

sjp.pca(ppmi.NUPDRS3.Biospecimen.Imaging.pca,
        plotEigenvalues = TRUE,
        type = "circle")


