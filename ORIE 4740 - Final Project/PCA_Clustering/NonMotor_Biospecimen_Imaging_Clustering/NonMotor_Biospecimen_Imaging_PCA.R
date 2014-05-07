#Apply PCA to non-motor, biospecimen, and imaging data
#other than the UPSITTOTAL, there doesn't seem to be much significance between bio data and non motor data
rm(list = ls())
library(ggplot2)
library(gplots)
library(rgl)
library(lattice)
library(sjPlot)
library(fpc)
library(mclust)
library(cluster)

setwd("~/Dropbox//ORIE 4740 - Final Project/PCA_Clustering/NonMotor_Biospecimen_Imaging_Clustering//")
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

# #obtaining non-motor data
# ppmi.nonmotor.data = ppmi.raw.data[,
#                                    c(which(names(ppmi.raw.data) %in% "BJLOT1"):which(names(ppmi.raw.data) %in% "NP1FATG"),
#                                      
#                                      which(names(ppmi.raw.data) %in% "NUPDRS1_TOT"))]

ppmi.nonmotor.data = subset.data.frame(ppmi.raw.data,select = c(JLO_TOTRAW,
                                                                ESSTOTAL,
                                                                GDTOTAL,
                                                                DVT_TOTAL_RECALL,
                                                                DVT_DELAYED_RECALL,
                                                                DVT_RETENTION,
                                                                DVT_RECOG_DISC_INDEX,
                                                                LNS_TOTRAW,
                                                                MCATOT,
                                                                REMSLEEPTOTAL,
                                                                DVT_SFTANIM,
                                                                DVT_SDM,
                                                                SCOPATOTAL,
                                                                UPSITTOTAL                                                                
                                                                ))

#assuming that all UPDRSPart3 data is in the columns between NP3SPCH and NUPDRS_TOT
ppmi.motor.NUPDRS3.data = ppmi.raw.data[,
                                        c(which(names(ppmi.raw.data) %in% "NP3SPCH"):which(names(ppmi.raw.data) %in% "NUPDRS_TOT"))]

ppmi.motor.NUPDRS3.data = ppmi.motor.NUPDRS3.data[,-which(names(ppmi.raw.data) %in% "NUPDRS1_TOT")]

#selecting all imaging and biospecimen data
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

ppmi.nonmotor.biospecimen.imaging.data = data.frame(ppmi.nonmotor.data,
                                                   ppmi.biospecimen.imaging.data)

# Performing PCA ----------------------------------------------------------
ppmi.nonmotor.biospecimen.imaging.pca = prcomp(ppmi.nonmotor.biospecimen.imaging.data,scale = TRUE)
sjp.pca(ppmi.nonmotor.biospecimen.imaging.pca,
        plotEigenvalues = TRUE,
        type = "circle")

# PCA shows that only the first 9 PCs satisfy kaiser criterion -----------
mydata = ppmi.nonmotor.biospecimen.imaging.pca$x[,c(1:9)]

# model based clustering on the first 9 components -----------------------

model_fit = Mclust(mydata)
plot(model_fit)
summary(model_fit)

# k-means clustering with the first 9 components -------------------------
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

plot3d(ppmi.motor.NUPDRS3.pca$x[,c(1:3)],
       xlim = c(-10,10),
       ylim = c(-10,10),
       zlim = c(-10,10))