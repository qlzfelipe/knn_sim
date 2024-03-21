library(kernlab)

#Classificação: SVM
colnames(iris)
tr_smp <- sample(1:nrow(iris), nrow(iris) - 10)

#Separação entre treinamento e validação
train <- iris[tr_smp, ]
valid <- iris[setdiff(1:nrow(iris), tr_smp), ]
valid2 <- valid

colnames(valid2)[1:4] <- 1:4
colnames(train)
#IMPORTANTE: CONVERTER DADOS CATEGÓRICOS PRA FACTOR
mod.iris <- kernlab::ksvm(x = Species ~ ., 
                          data = train, kernel = "rbfdot", C = 1, epsilon = .1)
mod.iris
fitted(mod.iris)
#IMPORTANTE: KERNLAB USA KERNEL GAUSSIANO POR PADRÃO
#ESPECIFICAR O KERNEL ENTRE ASPAS SIGNIFICA ESTIMAR O PARÂMETRO AUTOMATICAMENTE: 
#KERNEL GAUSSIANO E LAPLACIANO

pr <- kernlab::predict(object = mod.iris, newdata = train)
#IMPORTANTE: VARIÁVEIS TEM O MESMO NOME NOS DADOS DE TREINAMENTO E VALIDAÇÃO

#EXEMPLO: ESCALAR VARIÁVEIS
iris_scaled = sapply(X = 1:(ncol(train) - 1), function(x){
  iris[, x] = (iris[, x] - min(iris[, x]))/
    (max(iris[, x]) - min(iris[, x]))
})
iris_scaled <- cbind(iris_scaled, iris$Species)
colnames(iris_scaled) <- colnames(iris)
