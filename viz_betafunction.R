a <- b <- c <- d <- seq(0, 1, length.out = 100)
mt <- matrix(c(a, b, c, d), ncol = 4)
for(i in 1:nrow(mt)){
  distances <- sapply(X = 1:nrow(mt), FUN = function(x){
    distance_euclid(mt[i, ], mt[x, ])
  }) 
}

x = distances


beta <- data.frame(d = x, beta_0 = exp(-0*x)/sum(exp(-0*x)),
beta_1 = exp(-1*x)/sum(exp(-1*x)),
beta_3 = exp(-3*x)/sum(exp(-3*x)),
beta_5 = exp(-5*x)/sum(exp(-5*x)),
beta_7 = exp(-7*x)/sum(exp(-7*x)),
beta_10 = exp(-10*x)/sum(exp(-10*x))) #%>%

beta %>% reshape2::melt(id = c("d"),
                        variable.name = "Beta", 
                        value.name = "Peso") %>%
  ggplot(aes(x = d, y = Peso, color = Beta)) +
  geom_line() +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_bw()
