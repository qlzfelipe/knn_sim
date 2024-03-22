gen_struct1 <- function(n, N, seed = n, split = .8){
  set.seed(n)
  target_indexes = 1
  train_series <- test_series <- list()
  n_train <- round(split * n)
  n_test <- n - n_train
  target_indexes = 1
  for(i in 1:N){
    x1 <- runif(n, -2.5, 2.5)
    x2 <- runif(n, -2.5, 2.5)
    x3 <- runif(n, -2.5, 2.5)
    x4 <- runif(n, -2.5, 2.5)
    y <- -2*sin(x1) + (x2^2 - 25/12) + x3 + (exp(-x4) - (2/5)*sin(5/2)) +
      rnorm(n)
    data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
    train <- head(data, n_train)
    test <- tail(data, n_test)
    train_series[[i]] <- train
    test_series[[i]] <- test
  }
  train_indexes = 2:ncol(data)
  return(list(train = train_series, test = test_series,
              train_indexes = train_indexes,
              target_indexes = target_indexes))
}

gen_struct2 <- function(n, N, seed = n, split = .8){
  set.seed(n)
  train_series <- test_series <- list()
  n_train <- round(split * n)
  n_test <- n - n_train
  
  target_indexes = 1
  for(i in 1:N){
    x1 <- runif(n, 0, 1)
    x2 <- runif(n, 0, 1)
    x3 <- runif(n, 0, 1)
    x4 <- runif(n, 0, 1)
    
    f1 <- (x1)
    f2 <- (2*x2 - 1)^2
    f3 <- (sin(2*pi*x3))/(2 - sin(2*pi*x3))
    f4 <- .1*(sin(2*pi*x4)) + .2*(sin(2*pi*x4)) +.3*(sin(2*pi*x4)^2) +
      .4*(cos(2*pi*x4)^3) + .5*(sin(2*pi*x4)^3)
    
    y <- 5*f1 + 3*f2 + 4*f3 + 6*f4 + rnorm(n , 0 , 1.74)
    data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
    train <- head(data, n_train)
    test <- tail(data, n_test)
    train_series[[i]] <- train
    test_series[[i]] <- test
  }
  train_indexes = 2:ncol(data)
  return(list(train = train_series, test = test_series,
              train_indexes = train_indexes,
              target_indexes = target_indexes))
}