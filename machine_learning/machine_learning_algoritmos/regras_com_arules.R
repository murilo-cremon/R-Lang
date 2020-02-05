# Utilizando algoritmo arules de regra de associação para criar uma classificação

library(arules) # Mineração de regras de associação
library(arulesCBA) # Pacote que cria regras
library(caret) # Modelagem estatística

tempo <- read.csv(file.choose(), sep = ";", header = TRUE)

modelo <- CBA(play ~ ., data = tempo, support = 0.05, confidence = 0.9) # Suporte e confiança mínica para criação das regras
inspect(modelo$rules) # Inspeção do modelo e das regras

previsao <- predict(modelo, newdata = tempo)
head(previsao)

table(previsao, tempo$play)