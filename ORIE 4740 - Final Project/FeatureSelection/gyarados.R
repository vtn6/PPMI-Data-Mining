
#Correlation Matrix Filter, PCA and Backwards Selection 
#http://www.r-bloggers.com/introduction-to-feature-selection-for-bioinformaticians-using-r-correlation-matrix-filters-pca-backward-selection/
library(ggplot2)
library(gplots)

rm(list = ls())
setwd("~/Dropbox//ORIE 4740 - Final Project/FeatureSelection//")
ppmi.raw.data = read.csv("pichu.csv",header=TRUE,
                         stringsAsFactors = TRUE
)


# removing factors --------------------------------------------------------

factor.index = c()
for (i in 1:dim(ppmi.raw.data)[2])
{
  if(is.factor(ppmi.raw.data[[i]])==TRUE)
  {
    print("fuck")
    factor.index = c(factor.index,i)
  }
}

ppmi.data = ppmi.raw.data[,-factor.index]

cor.ppmi = cor(ppmi.data,use = "pairwise.complete.obs")
