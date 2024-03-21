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