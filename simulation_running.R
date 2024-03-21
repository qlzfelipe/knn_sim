source("knn.R")

listN <- function(...){
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

#Definindo configurações, funções de perda, K, Beta e grid
configurations <- list(list(function_calc = mean), 
                       list(function_calc = median),
                       list(function_calc = weighted.mean),
                       list(function_calc = mode_continuous))
#Funções de perda
loss_functions <- listN(absolute_loss, quadratic_loss)
#métricas de erro
error_metrics <- listN(rmse, mae)
#range de beta
beta_range <- seq(0, 10, 1)
#range de K
k_range <- seq(2, 14, 2)

grid <- expand.grid(k = k_range, beta = beta_range, 
                    #error = 1:length(error_metrics),
                    loss = 1:length(loss_functions),
                    function_calc = 1:length(configurations))
grid_rmse <- numeric(nrow(grid))

for(i in 1:nrow(grid)){
  grid_rmse[i] <- knn_cv(data=data$train[[1]], Kfolds = 5, K = grid$k[i], 
                      beta = grid$beta[i], 
         loss_function = loss_functions[[grid$loss[i]]], 
         function_calc = configurations[[grid$function_calc[i]]][[1]])
}

for(f in list.files("data")){
  path <- path.expand(paste0("data/", f))
  data <- readRDS(path)
  train_ind <- data$train_indexes
  test_ind <- data$target_indexes
  
  result_config <- list()
  
  for(c in 1:length(configurations)){
    
    predictions <- matrix(nrow = nrow(data$test[[1]]), ncol = length(data$test))
    fit <- matrix(nrow = nrow(data$train[[1]]), ncol = length(data$test))
    measured_errors <- matrix(nrow = length(error_metrics), ncol = ncol(predictions))
    measured_loss <- matrix(nrow = length(loss_functions), ncol = ncol(predictions))
    
    for(i in 1:length(data$train)){
      model.predict.train <- knn_custom(train = data$train[[i]][, train_ind], 
                                        validation = data$train[[i]][, train_ind], 
                                        training_target = data$train[[i]][, test_ind],
                                        validation_target = data$train[[i]][, test_ind], 
                                        K = 10,
                                        function_calc = configurations[[c]][[1]],
                                        task = "regression",
                                        beta = 1)
      
      model.predict.test <- knn_custom(train = data$train[[i]][, train_ind], 
                                       validation = data$test[[i]][, train_ind], 
                                       training_target = data$train[[i]][, test_ind], 
                                       validation_target = data$test[[i]][, test_ind], 
                                       K = 10,
                                       function_calc = configurations[[c]][[1]],
                                       loss_function = quadratic_loss,
                                       task = "regression",
                                       beta = 1)
      
      for(e in 1:length(error_metrics)){
        measured_errors[e, i] <- do.call(error_metrics[[e]], 
                                         list(actual = data$test[[i]][, test_ind],
                                              predicted = model.predict.test$predictions))
      }
      for(l in 1:length(loss_functions)){
        measured_loss[l, i] <- do.call(loss_functions[[l]],
                                       list(actual = data$train[[i]][, test_ind],
                                            predicted = model.predict.train$predictions))
      }
      rownames(measured_errors) <- names(error_metrics)
      rownames(measured_loss) <- names(loss_functions)
      predictions[, i] <- model.predict.test$predictions
      fit[, i] <- model.predict.train$predictions
    }
    result_config[[c]] <- list(errors = measured_errors, fit = fit,
                               loss = measured_loss, predictions = predictions,
                               file = file)
    names(result_config) <- paste0("config", 1:length(result_config))
  }
  saveRDS(result_config, file = paste0("results/", f))
}
