a <- b <- c <- d <- seq(0, 1, length.out = 100)
mt <- matrix(c(a, b, c, d), ncol = 4)
for(i in 1:nrow(mt)){
  distances <- sapply(X = 1:nrow(mt), FUN = function(x){
    distance_euclid(mt[i, ], mt[x, ])
  }) 
}

x = distances
beta = .0
weights <- exp(-beta*x)/sum(exp(-beta*x))

n = 100

x1 <- rbeta(n, 15, 1)
x2 <- rexp(n, rate = .1)
x3 <- rbeta(n, 1, 10)
y <- 80*x1 + x2 + 300*(x3) + (rbeta(n, .5, .5) -.5) * 60
data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
shapiro.test(y)
a <- lm("y ~ x1 + x2 + x3", data=data)
a
#plot(y)
#lines(a$fitted.values)
# err <- runif(n, -3, 3)^3
# err2 <- runif(n, -27, 27)
# par(mfrow=c(1,2))
# hist(err)
# hist(err2)
# shapiro.test(err)
par(mfrow = c(1,1))
