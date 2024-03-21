gen_struct1 <- function(n, N, seed = n, split = .8){
  set.seed(n)
  train_series <- test_series <- list()
  n_train <- round(split * n)
  n_test <- n - n_train
  
  target_indexes = 1
  for(i in 1:N){
    x1 <- rbeta(n, 15, 1)
    x2 <- rexp(n, rate = .1)
    x3 <- rbeta(n, 1, 10)
    y <- 80*x1 + x2 + 300*(x3) + (rbeta(n, .5, .5) -.5) * 60
    data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
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

gen_struct2 <- function(n, N, seed = set.seed(n), split = .8){
  set.seed(n)
  train_series <- test_series <- list()
  n_train <- round(split * n)
  n_test <- n - n_train
  target_indexes = 1
  for(i in 1:N){
    x1 <- rnorm(n, 15, 1)
    x2 <- rnorm(n, 10, 1)
    y <- 3*x1 + 2*x2 + rnorm(n, 5, 1)
    data <- data.frame(y = y, x1 = x1, x2 = x2)
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
