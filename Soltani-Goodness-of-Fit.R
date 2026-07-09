R <- 999 
reps <- numeric(R)
for (i in 1:R) {
data1 <- rnorm(100)
n <- length(data1)
data1_ordered <- sort(data1)
data1_ordered
c1 = data1_ordered
c2 <- data1_ordered[2:n]
m <- length(c2)
F_0 <- function(x){pnorm(x)}
F1 <- function(x){F_0(c1[x])}
F2 <- function(x){F_0(c2[x])}
s1 <- sapply(1:m, F2)
#s1
s_1 <- c(s1,1)
#s_1
s2 <- sapply(1:n, F1)
#s2
numerator <- s_1-s2
#numerator
denominator <- rep(1,n)- sapply(1:n, F1)
#denominator
ratio <- numerator/denominator
#ratio
G <- function(x){(n - x + 1)/n}
factor <- sapply(1:n, G)
#factor
T <- factor*ratio
reps[i] <- sqrt(n)*sum(T-rep(1/n, n))
}
hist(reps, main="Null distribution of our TS") 
#z_F <- sqrt(n)*sum(T-rep(1/n, n))
#z_F
#p_value <- 2*(1-pnorm(abs(z_F)))
#p_value
#round(p_value, 4)
