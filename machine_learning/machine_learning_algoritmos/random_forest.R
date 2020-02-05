library(randomForest)

credito <- read.csv(file.choose(), sep = ",", header = TRUE)

# Não precisa gerar dados de treino e teste, pois o modelo de randomforest realiza um bootstrap
# Ação pela qual realiza reposição dos dados, votando e opondo o resultado mais votado
modelo <- randomForest(class ~ ., data = credito, ntree = 500) # ntree = número de árvores geradas

modelo$predicted

modelo$importance

modelo$votes

modelo$forest

modelo$confusion

plot(modelo)

predict(modelo, newdata = credito [154, ])

variavel <- if (modelo$predicted[[154]] == predict(modelo, newdata = credito [154, ])) {
  1
} else {
  2
}