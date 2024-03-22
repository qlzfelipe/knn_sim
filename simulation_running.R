source("knn.R")
library(doParallel)
library(bigstatsr)

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
beta_range <- c(1, seq(2, 10, 2))
#range de K
k_range <- seq(2, 12, 2)

for(f in list.files("data")){
  path <- path.expand(paste0("data/", f))
  data <- readRDS(path)
  train_ind <- data$train_indexes
  test_ind <- data$target_indexes
  
  dataset_results <- list()
  
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  for(i in 1:length(data$train)){
    tm <- Sys.time()
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
    for(g in 1:nrow(grid)){
      if(grid[g, 3] != 3){
        grid[g, 2] <- 0
      }
      if(grid[g, 1] == 1 && grid[g, 3] == 4){
        grid[g, 3] <- 1
      }
    }
    grid <- unique(grid)
    grid_og <- grid
    
    predictions <- as_FBM(matrix(nrow = nrow(data$test[[1]]), ncol = nrow(grid)))
    fit <- as_FBM(matrix(nrow = nrow(data$train[[1]]), ncol = nrow(grid)))
    grid <- as_FBM(grid)

    out <- foreach(c = 1:nrow(grid)) %dopar% {
      model.predict.train <- knn_custom(train = data$train[[i]][, train_ind], 
                                        validation = data$train[[i]][, train_ind], 
                                        training_target = data$train[[i]][, test_ind],
                                        validation_target = data$train[[i]][, test_ind], 
                                        K = grid[c, 1],
                                        function_calc = function_calcs[[grid[c, 3]]][[1]],
                                        task = "regression",
                                        beta = grid[c, 2])
      
      model.predict.test <- knn_custom(train = data$train[[i]][, train_ind], 
                                       validation = data$test[[i]][, train_ind], 
                                       training_target = data$train[[i]][, test_ind], 
                                       validation_target = data$test[[i]][, test_ind], 
                                       K = grid[c, 1],
                                       function_calc = function_calcs[[grid[c, 3]]][[1]],
                                       task = "regression",
                                       beta = grid[c, 2])
      
      for(e in 1:length(error_metrics)){
       grid[c, index_errors[e]] <- do.call(error_metrics[[e]], 
                                         list(actual = data$test[[i]][, test_ind],
                                              predicted = model.predict.test$predictions))
      }
      for(l in 1:length(loss_functions)){
        grid[c, index_losses[l]] <- do.call(loss_functions[[l]],
                                       list(actual = data$train[[i]][, test_ind],
                                            predicted = model.predict.train$predictions))
      }
      predictions[, c] <- model.predict.test$predictions
      fit[, c] <- model.predict.train$predictions
      NULL
    }
    grid <- grid[]
    colnames(grid) <- colnames(grid_og)
    dataset_results[[i]] <- list(grid = grid, 
                                 fit = fit[], predictions = predictions[])
    print(i)
    print(Sys.time() - tm)
  }
  stopCluster(cl)
  saveRDS(dataset_results, file = paste0("results/", f))
  print(f)
}