a <- matrix(rnorm(4000*2000), 4000, 2000) 
# Generate some NAs in the matrix 
nr <- sample(50, 1:4000) 
nc <- sample(50, 1:2000) 
a[nr, nc] <- NA 

# convert to data frame: 
b <- data.frame(row = rep(1:4000, 2000), col = rep(1:2000, each = 4000), 
                x = as.vector(a)) 
# relatively time consuming...about 13.5 s on my machine 
bb <- b[rev(order(b$x, na.last = FALSE)), ] 

bb[1:10, ] 
