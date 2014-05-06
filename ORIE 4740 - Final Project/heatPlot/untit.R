library(gplots)
data(mtcars)
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)

##
## demonstrate the effect of row and column dendogram options
##
heatmap.2(x)

