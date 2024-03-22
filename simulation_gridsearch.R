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
                       list(function_calc = weighted.mean))
#Funções de perda
loss_functions <- listN(absolute_loss, quadratic_loss)
#métricas de erro
error_metrics <- listN(rmse)
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

    for(g in 1:nrow(grid)){
      if(grid$function_calc[g] != 3){
        grid$beta[g] <- 0
      }
    }
    grid <- unique(grid)
    grid_og <- grid
    
    predictions <- as_FBM(matrix(nrow = nrow(data$test[[1]]), ncol = nrow(grid)))
    fit <- as_FBM(matrix(nrow = nrow(data$train[[1]]), ncol = nrow(grid)))
    grid <- as_FBM(grid)
    
    out <- foreach(c = 1:nrow(grid)) %dopar% {
      grid[c, 4] <- knn_cv(data = data$train[[i]][, train_ind], 
                                        K = grid[c, 1],
                                        function_calc = function_calcs[[grid[c, 3]]][[1]],
                                        task = "regression",
                                        beta = grid[c, 2], Kfolds = 5)
     NULL
    }
    grid <- grid[]
    colnames(grid) <- colnames(grid_og)
    selected <- grid[which.min(grid[, 4]), 1:3]
    
    model.predict.test <- knn_custom(train = data$train[[i]][, train_ind], 
                                     validation = data$test[[i]][, train_ind], 
                                     training_target = data$train[[i]][, test_ind],
                                     K = selected[1],
                                     function_calc = function_calcs[[selected[3]]][[1]],
                                     task = "regression",
                                     beta =  selected[2])
    error_rmse <- Metrics::rmse(data$test[[i]][, test_ind], 
                                model.predict.test$predictions)
    error_mae <- Metrics::mae(data$test[[i]][, test_ind], 
                                model.predict.test$predictions)
    
    dataset_results[[i]] <- list(grid = grid,
                                 fit = fit[], predictions = predictions[],
                                 error_rmse = error_rmse,
                                 error_mae = error_mae)
    print(i)
    print(Sys.time() - tm)
  }
  stopCluster(cl)
  saveRDS(dataset_results, file = paste0("results/cv/", f))
  print(f)
}
