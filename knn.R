#Distance measures
distance_euclid <- function(x, y){
  return(sqrt(sum((x - y)^2)))
}
distance_manhattan <- function(x, y){
  return(sum(abs(x - y)))
}

# Loss functions - Classification
basic_loss <- function(actual, predicted){
  return(sum(actual != predicted)/length(actual))
}
exponential_loss <- function(actual, predicted){
  return(sum(exp(actual != predicted)))
}

## Loss functions - Regression
absolute_loss <- function(actual, predicted, ...){
  return(sum(abs(actual - predicted)))
}

quadratic_loss <- function(actual, predicted, ...){
  return(sum((actual - predicted)^2))  
}

epsilon_loss <- function(actual, predicted, epsilon = .001){
  if(abs(actual - predicted) <= epsilon){
    return(0)
  }else{
    return(sum(abs(actual - predicted) - epsilon))
  }
}

huber_loss <- function(actual, predicted, d = 1){
  if(abs(actual - predicted) <= d){
    return((1/2)*(actual - predicted)^2)
  }else{
    return(d*(abs(actual - predicted) - (1/2)*(d^2)))
  }
}

#Funcao para computar moda
mode_continuous <- function(x, ...){
  dist <- density(x)
  val <- dist$x[which.max(dist$y)]
  return(val)
}

#Funcao para computar pesos
weighted.mean <- function(x, distances, beta, ...){
  weights <- exp(-beta*distances)/sum(exp(-beta*distances))
  return(sum(x*weights))
}

# train: training data (no labels) - matrix or data frame
# validation: validation data (no labels) - matrix or data frame
# target: factor with labels (classification) or numeric vector (regression)
# K: K parameter
# loss_function: loss function to be computed
# distance_metric: distance metric to be used
# task: task to be performed

knn_custom <- function(train, 
                       validation,
                       training_target,
                       validation_target,
                       K,
                       loss_function = quadratic_loss,
                       function_calc = mean,
                       distance_metric = distance_euclid,
                       task = "regression",
                       ...){
  #scaling
  it_train = sapply(X = 1:ncol(train), function(x){
    train[, x] = (train[, x] - min(train[, x]))/
      (max(train[, x]) - min(train[, x]))
  })
  it_validation = sapply(X = 1:ncol(validation), function(x){
    validation[, x] = (validation[, x] - min(validation[, x]))/
      (max(validation[, x]) - min(validation[, x]))
  })
  
  it_target <- training_target
  if(task == "classification"){
    for(i in 1:nrow(it_validation)){
      distances <- sapply(X = 1:nrow(it_train), function(x){
        do.call(distance_metric, list(it_validation[i,], it_train[x, ]))
      })
      distances <- matrix(c(1:nrow(it_train), distances), ncol = 2)
      neighbors <- head(distances[order(distances[, 2]), 1], K)
      it_target <- factor(c(as.character(it_target), 
                            names(which.max(table(it_target[neighbors])))),
                          levels = unique(it_target), ordered = F)
      it_train <- rbind(it_train, it_validation[i, ])
    }
    validation_target_predict <- tail(it_target, nrow(validation))
    it_train <- rbind(train, validation)
    loss_value <- do.call(loss_function, list(validation_target, 
                                              validation_target_predict))
  }else if(task == "regression"){
    for(i in 1:nrow(it_validation)){
      distances <- sapply(X = 1:nrow(it_train), function(x){
        do.call(distance_metric, list(it_validation[i,], it_train[x, ]))
      })
      distances <- matrix(c(1:nrow(it_train), distances), ncol = 2)
      neighbors <- head(distances[order(distances[, 2]), 1], K)
      newval <- do.call(function_calc, list(x = it_target[neighbors],
                                            distances = distances[neighbors, 2],
                                            ...))
      if(is.nan(newval) || is.na(newval)){
        stop("NA/NaN produced. You may want to change your beta parameter.")
      }
      it_target <- c(it_target, newval)
      it_train <- rbind(it_train, it_validation[i, ])
    }
    validation_target_predict <- tail(it_target, nrow(validation))
    it_train <- rbind(train, validation)
    
    norm_validation_target <- (validation_target - min(validation_target))/
      (max(validation_target) - min(validation_target))
    norm_validation_predict <- (validation_target_predict - min(validation_target_predict))/
      (max(validation_target_predict) - min(validation_target_predict))
    loss_value <- do.call(loss_function, list(actual = norm_validation_target, 
                                              predicted = norm_validation_predict))
  }
  if(!hasArg(beta)){
    beta = NULL
  }
  return(list("predictors" = it_train, 
              "target" = it_target, 
              "predictions" = validation_target_predict,
              "loss_value" = loss_value,
              "parameters" = list(K = K, 
                                  beta = beta,
                                  loss_function = loss_function)))
}

#Gerando dados para executar CV
gen_cv_data <- function(data, Kfolds, ...){
  size_Kfolds <- nrow(data)/Kfolds
  indx <- 1
  result = list()
  for(i in 1:Kfolds){
    indx_end <- indx + size_Kfolds - 1
    full_out <- data[indx:indx_end,]
    indx <- indx_end + 1
    result[[i]] <- full_out
  }
  return(cv_data = result)
}

knn_cv <- function(data, error_measure = Metrics::rmse, ...){
  cv_data <- gen_cv_data(data, ...)
  metrics <- sapply(X = 1:length(cv_data), function(x){
    indx_train <- c(1:length(cv_data))[-c(x)]
    test <- cv_data[[x]]
    train <- matrix(nrow = 0, ncol = ncol(cv_data[[x]]))
    for(i in indx_train){
      train <- rbind(train, cv_data[[i]])
    }
    model <- knn_custom(train, test, training_target = train[,1], 
               validation_target = test[,1], ...)
    do.call(error_measure, list(test[,1], model$predictions))
  })
  return(mean(metrics))
}

knn_cv(train, Kfolds = 5, K = 5)
