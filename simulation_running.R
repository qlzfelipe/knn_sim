source("knn.R")

listN <- function(...){
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

#Definindo configurações, funções de perda, K, Beta e grid
function_calcs <- list(list(function_calc = mean), 
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

for(f in list.files("data")){
  path <- path.expand(paste0("data/", f))
  data <- readRDS(path)
  train_ind <- data$train_indexes
  test_ind <- data$target_indexes

  grid <- expand.grid(k = k_range, beta = beta_range, 
                      function_calc = 1:length(function_calcs))
  for(e in 1:length(error_metrics)){
    grid <- cbind(grid, matrix(nrow = nrow(grid)))
    names(grid)[ncol(grid)] <- names(error_metrics)[e]
  }
  index_errors <- tail(1:ncol(grid), length(error_metrics))
  for(l in 1:length(loss_functions)){
    grid <- cbind(grid, matrix(nrow = nrow(grid)))
    names(grid)[ncol(grid)] <- names(loss_functions)[l]
  }
  index_losses <- tail(1:ncol(grid), length(loss_functions))
  dataset_results <- list()
  
  for(i in 1:length(data$train)){
    for(c in 1:nrow(grid)){
      predictions <- matrix(nrow = nrow(data$test[[1]]), ncol = length(data$test))
      fit <- matrix(nrow = nrow(data$train[[1]]), ncol = length(data$test))
      result_grid <- list()
      model.predict.train <- knn_custom(train = data$train[[i]][, train_ind], 
                                        validation = data$train[[i]][, train_ind], 
                                        training_target = data$train[[i]][, test_ind],
                                        validation_target = data$train[[i]][, test_ind], 
                                        K = grid$k[c],
                                        function_calc = function_calcs[[grid$function_calc[c]]][[1]],
                                        task = "regression",
                                        beta = grid$beta[c])
      
      model.predict.test <- knn_custom(train = data$train[[i]][, train_ind], 
                                       validation = data$test[[i]][, train_ind], 
                                       training_target = data$train[[i]][, test_ind], 
                                       validation_target = data$test[[i]][, test_ind], 
                                       K = grid$k[c],
                                       function_calc = function_calcs[[grid$function_calc[c]]][[1]],
                                       task = "regression",
                                       beta = grid$beta[c])
      
      for(e in 1:length(error_metrics)){
       grid[, index_errors[e]] <- do.call(error_metrics[[e]], 
                                         list(actual = data$test[[i]][, test_ind],
                                              predicted = model.predict.test$predictions))
      }
      for(l in 1:length(loss_functions)){
        grid[, index_losses[l]] <- do.call(loss_functions[[l]],
                                       list(actual = data$train[[i]][, test_ind],
                                            predicted = model.predict.train$predictions))
      }
      predictions[, i] <- model.predict.test$predictions
      fit[, i] <- model.predict.train$predictions
      result_grid[[c]] <- list(grid,
                               fit = fit,
                               predictions = predictions,
                               file = file)
    }
    names(result_grid) <- paste0("config", 1:length(result_grid))
    dataset_results[[i]] <- list(grid_details = result_grid)
    print(i)
  }
  saveRDS(dataset_results, file = paste0("results/", f))
}
