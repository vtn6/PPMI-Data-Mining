setwd('C:\\Users\\cit-labs\\Dropbox\\ORIE 4740 - Final Project')
library(lattice)
data=read.csv("NMIB_AverageValues.csv", header=TRUE, na.string="NA")
dim(data)
head(data$RECRUITMENT_CAT)
pd_data = data[data$RECRUITMENT_CAT!="HC",]
dim(pd_data)

# pca on motor, non-motor, imaging, biospecimen
pca_data=pd_data[4:325]
pca_data=scale(pca_data)
output=prcomp(pca_data)
summary(output)

# pca on motor, non-motor (excluding imaging)
motor_nonmotor=pd_data[4:321]
motor_nonmotor=scale(motor_nonmotor)
output2=prcomp(motor_nonmotor)
summary(output2)

# pca on motor
motor=pd_data[250:315]
motor=scale(motor)
motor_output=prcomp(motor)
summary(motor_output)
motor_output$rotation
pairs(motor_output$x[,1:5])

# pca on nonmotor
nonmotor=pd_data[4:249]
nonmotor=scale(nonmotor)
nonmotor_output=prcomp(nonmotor)
summary(nonmotor_output)
nonmotor_output$rotation[,1]
pairs(nonmotor_output$x[,1:5])

# pca on imaging
imaging = pd_data[316:321]
imaging = scale(imaging)
imaging_output = prcomp(imaging)
summary(imaging_output)

# pca on bio
bio = pd_data[322:325]
bio = scale(bio)
bio_output = prcomp(bio)
summary(bio_output)

# pca on nonmotor and imaging
nonm_img_output=prcomp(cbind(nonmotor,imaging))
summary(nonm_img_output)
nonm_img_output$x
pairs(nonm_img_output$x[,1:5])

# pca on nonmotor and bio
nonm_bio_output=prcomp(cbind(nonmotor,bio))
summary(nonm_bio_output)
nonm_bio_output$x
pairs(nonm_bio_output$x[,1:15],cex=0.2)

# pca on nonmotor, imaging and bio
nonm_img_bio_output=prcomp(cbind(nonmotor,imaging,bio))
summary(nonm_img_bio_output)
nonm_img_bio_output$x
pairs(nonm_img_bio_output$x[,1:5])

#to obtain graph for W(C) of PC1 and PC2
within_clusters12<-c();

for (i in 1:10) {
  within_clusters12[i-1] = kmeans(nonm_img_bio_output$x[,1:2], i)$tot.withinss
}

plot(within_clusters12, type="b", main="W(C) for PC1 and PC2", xlab="number of clusters", ylab="Total Sum of Squares")


#to obtain graph for W(C) of PC3 and PC2
within_clusters32<-c();

for (i in 1:10) {
  within_clusters32[i-1] = kmeans(nonm_img_bio_output$x[,c(3,2)], i)$tot.withinss
}

plot(within_clusters32, type="b", main="W(C) for PC3 and PC2", xlab="number of clusters", ylab="Total Sum of Squares")








#finding the highest magnitude characteristics in each principal component
print("largest dimensions of Loading 1")
print(nonm_img_bio_output$rotation[,1][order(abs(nonm_img_bio_output$rotation[,1]),decreasing=TRUE)])
print(nonm_img_bio_output$rotation[,2][order(abs(nonm_img_bio_output$rotation[,2]),decreasing=TRUE)])
print(nonm_img_bio_output$rotation[,3][order(abs(nonm_img_bio_output$rotation[,3]),decreasing=TRUE)])
print(nonm_img_bio_output$rotation[,4][order(abs(nonm_img_bio_output$rotation[,4]),decreasing=TRUE)])
print(nonm_img_bio_output$rotation[,5][order(abs(nonm_img_bio_output$rotation[,5]),decreasing=TRUE)])











#finding the highest magnitude characteristics in each principal component
print("largest dimensions of Loading 1")
print(nonm_bio_output$rotation[,1][order(abs(nonm_bio_output$rotation[,1]),decreasing=TRUE)])
#PC1 represents low STAID, high geriatic depresison

print("largest dimensions of Loading 2")
print(nonm_bio_output$rotation[,2][order(abs(nonm_bio_output$rotation[,2]),decreasing=TRUE)])
print("largest dimensions of Loading 3")
print(nonm_bio_output$rotation[,3][order(abs(nonm_bio_output$rotation[,3]),decreasing=TRUE)])
print("largest dimensions of Loading 4")
print(nonm_bio_output$rotation[,4][order(abs(nonm_bio_output$rotation[,4]),decreasing=TRUE)])
print("largest dimensions of Loading 5")
print(nonm_bio_output$rotation[,5][order(abs(nonm_bio_output$rotation[,5]),decreasing=TRUE)])
print("largest dimensions of Loading 6")
print(nonm_bio_output$rotation[,6][order(abs(nonm_bio_output$rotation[,6]),decreasing=TRUE)])
print("largest dimensions of Loading 7")
print(nonm_bio_output$rotation[,7][order(abs(nonm_bio_output$rotation[,7]),decreasing=TRUE)])

print("largest dimensions of Loading 8")
print(nonm_bio_output$rotation[,8][order(abs(nonm_bio_output$rotation[,8]),decreasing=TRUE)])
#PC8 represents patients with high SCOPA scores, low REM scores, high tau and CSF Alpha Synuclein 

print("largest dimensions of Loading 9")
print(nonm_bio_output$rotation[,9][order(abs(nonm_bio_output$rotation[,9]),decreasing=TRUE)])
#PC9 represents patients with low ESS scores, high UPSIT scores, high REM score

print("largest dimensions of Loading 10")
print(nonm_bio_output$rotation[,10][order(abs(nonm_bio_output$rotation[,10]),decreasing=TRUE)])
#PC10 represents high Hopkins score, low ESS, high geriatic depression, high Benton LO, low impulsive QUIP, low A-beta, low p tau


densityplot(output4$x[,1])

#finding the highest magnitude characteristics in each principal component
print("largest dimensions of Loading 1")
print(output4$rotation[,1][order(abs(output4$rotation[,1]),decreasing=TRUE)])
print("largest dimensions of Loading 2")
print(output4$rotation[,2][order(abs(output4$rotation[,2]),decreasing=TRUE)])
print("largest dimensions of Loading 3")
print(output4$rotation[,3][order(abs(output4$rotation[,3]),decreasing=TRUE)])

nonmotor1=output4$x[,1]
nonmotor2=output4$x[,2]
nonmotor3=output4$x[,3]
nonmotor4=output4$x[,4]
nonmotor5=output4$x[,5]

pairs(output4$x[,1:5])

#PC1 & PC2 clustering
pc12=kmeans(c(nonmotor2,nonmotor1), 3)
plot(nonmotor1, nonmotor2, col=pc12$cluster)
points(pc12$centers, col = 1:2, pch = 8, cex = 2)

#PC3 & PC2 clustering
pc32=kmeans(c(nonmotor2,nonmotor3), 3)
plot(nonmotor3, nonmotor2, col=pc32$cluster)
points(pc32$centers, col = 1:2, pch = 8, cex = 2)

#PC4 & PC2 clustering
pc42=kmeans(c(nonmotor2,nonmotor4), 3)
plot(nonmotor4, nonmotor2, col=pc42$cluster)
points(pc42$centers, col = 1:2, pch = 8, cex = 2)

#PC5 & PC2 clustering
pc52=kmeans(c(nonmotor2,nonmotor5), 3)
plot(nonmotor5, nonmotor2, col=pc52$cluster)
points(pc12$centers, col = 1:2, pch = 8, cex = 2)

#PC1,2,3
pc123=kmeans(c(nonmotor2,nonmotor1, nonmotor3), 3)
scatterplot3d(x=nonmotor1, y=nonmotor2, z=nonmotor3,color=pc123$cluster)
points(pc123$centers, col = 1:3, pch = 8, cex = 2)
length(nonmotor1)
length(pc123$cluster)

#