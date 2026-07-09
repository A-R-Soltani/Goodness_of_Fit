sol.test.stat <- function(n, data, dist, par1=c(0,1)){
j <- 1:(n-1)
data <- sort(data)
cc <- (n-j+1)/n
dd <- (dist(data[j+1])-dist(data[j]))/(1-dist(data[j]))
T <- cc*dd
sqrt(n)*sum(T-(1/n))
}
# histogram:

ECDF.test <- replicate(1000, sol.test.stat(n=50, data=rnorm(50), 
                                         dist=pnorm, par1=c(0,1)))
hist(ECDF.test, main="Null distribution of our TS") 
ECDF.test[200]
