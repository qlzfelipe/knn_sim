library(dplyr)
library(ggplot2)

results <- list()
files <- list.files(path = "results", full.names = T)
files <- files[2:length(files)]
for(i in 1:length(files)) {results <- append(results, list(readRDS(files[i])))}
final_gridinfo <- matrix(nrow = 0, ncol = ncol(results[[1]][[1]]$grid) + 3,
                         dimnames = list(NULL, c(colnames(results[[1]][[1]]$grid), 
                                                 "n", "structure", "nrep")))

length(results[[1]])
for(i in 1:length(files)){
  for(j in 1:length(results[[i]])){
    struct <- stringi::stri_split_fixed(stringi::stri_split_fixed(files[i], "/")
                                        [[1]][[2]], "_")[[1]][[1]]
    final_gridinfo <- rbind(final_gridinfo, 
                            cbind(results[[i]][[j]]$grid, 
                              matrix(c(rep(nrow(results[[i]][[j]]$fit) +
                                           nrow(results[[i]][[j]]$predictions), 
                                           nrow(results[[i]][[j]]$grid)), 
                                rep(struct, nrow(results[[i]][[j]]$grid)),
                                rep(i, nrow(results[[i]][[j]]$grid))), 
                              ncol = 3, dimnames = list(NULL, c("n", "structure", "nrep"))))
                            )
  }
}
final_res <- as.data.frame(final_gridinfo) %>%
  mutate_at(vars(rmse:quadratic_loss), as.numeric) %>%
  filter(function_calc != "4")
final_res$function_calc <- stringi::stri_replace_all_fixed(final_res$function_calc, 1, "Média")
final_res$function_calc <- stringi::stri_replace_all_fixed(final_res$function_calc, 2, "Mediana")
final_res$function_calc <- stringi::stri_replace_all_fixed(final_res$function_calc, 3, "Média Ponderada")
unique(final_res$function_calc)

final_res <- unique(final_res)

colnames(final_res)
str1_mae <- final_res %>%
  filter(structure == "struct1") %>%
  group_by(function_calc, n) %>%
  slice_min(mae, n = 10)

str1_rmse <- final_res %>%
  filter(structure == "struct1") %>%
  group_by(function_calc, n) %>%
  slice_min(rmse, n = 10)

str2_mae <- final_res %>%
  filter(structure == "struct2") %>%
  group_by(function_calc, n) %>%
  slice_min(mae, n = 10)

str2_rmse <- final_res %>%
  filter(structure == "struct2") %>%
  group_by(function_calc, n) %>%
  slice_min(rmse, n = 10)

res_cor <- matrix(c(cor(str1_mae$absolute_loss, str1_mae$mae, method = "spearman"),
cor(str1_rmse$quadratic_loss, str1_rmse$rmse, method = "spearman"),
cor(str2_mae$absolute_loss, str2_mae$mae, method = "spearman"),
cor(str2_rmse$quadratic_loss, str2_rmse$rmse, method = "spearman")), ncol = 2)
rownames(res_cor) <- c("Estrutura 1", "Estrutura 2")
colnames(res_cor) <- c("Correlação - L_a e MAE", "Correlação - L_q e RMSE")
res_cor

tablatex <- xtable::xtable(res_cor, digits = c(0, 3, 3))
print(tablatex, include.rownames = T)

str1_mae_max <- final_res %>%
  filter(structure == "struct1") %>%
  group_by(function_calc, n) %>%
  slice_max(mae, n = 10)

str1_rmse_max <- final_res %>%
  filter(structure == "struct1") %>%
  group_by(function_calc, n) %>%
  slice_max(rmse, n = 10)

str2_mae_max <- final_res %>%
  filter(structure == "struct2") %>%
  group_by(function_calc, n) %>%
  slice_max(mae, n = 10)

str2_rmse_max <- final_res %>%
  filter(structure == "struct2") %>%
  group_by(function_calc, n) %>%
  slice_max(rmse, n = 10)

res_cor <- matrix(c(cor(str1_mae_max$absolute_loss, str1_mae_max$mae, method = "spearman"),
                    cor(str1_rmse_max$quadratic_loss, str1_rmse_max$rmse, method = "spearman"),
                    cor(str2_mae_max$absolute_loss, str2_mae_max$mae, method = "spearman"),
                    cor(str2_rmse_max$quadratic_loss, str2_rmse_max$rmse, method = "spearman")), 
                  ncol = 2)
rownames(res_cor) <- c("Estrutura 1", "Estrutura 2")
colnames(res_cor) <- c("Correlação - L_a e MAE", "Correlação - L_q e RMSE")
res_cor

tablatex <- xtable::xtable(res_cor, digits = c(0, 3, 3))
print(tablatex, include.rownames = T)

mean_by_function <- final_res %>%
  group_by(structure, function_calc) %>%
  summarize_if(is.numeric, mean) %>%
  ungroup

mean_by_file <- final_res %>%
  group_by(structure, n, k, beta, function_calc) %>%
  summarize_if(is.numeric, mean) %>%
  ungroup

mean_by_file %>%
  #group_by(structure, n, function_calc) %>%
  summarize(corre1 = cor(absolute_loss, mae, method = "spearman"),
          corre2 = cor(quadratic_loss, rmse, method = "spearman"))

final_res %>%
  group_by(structure, function_calc, k, beta) %>%
  #slice_min(mae, n = 1000) %>%
  ungroup %>%
  ggplot(aes(x = function_calc, y = mae)) +
  geom_boxplot() +
  labs() + 
  facet_grid(structure ~ ., scales = "free") +
  theme_bw()

final_res %>%
  #slice_min(mae, n = 1000) %>%
  ungroup %>%
  ggplot(aes(x = function_calc, y = rmse)) +
  geom_boxplot() +
  labs(x = "", y = "RMSE") +
  facet_grid(structure ~ ., scales = "free") +
  theme_bw()

final_res %>%
  #slice_min(mae, n = 1000) %>%
  ungroup %>%
  ggplot(aes(x = function_calc, y = mae)) +
  geom_boxplot() +
  labs(x = "", y = "MAE") +
  facet_grid(structure ~ ., scales = "free") +
  theme_bw()


final_res %>%
  group_by(structure, function_calc, k, beta) %>%
  summarize(n()) %>% View
final_res %>% summarize(n())
