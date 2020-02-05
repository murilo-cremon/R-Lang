library(rpart)
library(caret)

# Obtendo dataset
credito <- read.csv(file.choose(), sep = ",", header = TRUE)

# Separação de dados de treino e teste
particao <- createDataPartition(1:1000, p = 0.7)
credito_treino <- credito[particao$Resample1, ]
credito_teste <- credito[-particao$Resample1, ]

# Criando modelo
modelo1 <- rpart(class ~ ., data = credito_treino, method = 'class')
plot(modelo1) # Plota a árvore
text(modelo1) # Adiciona texto para a plotagem

previsao <- predict(modelo1, newdata = credito_teste) # Realiza a predição
previsao <- as.data.frame(previsao) # Converte para DF, para não ser vetor atomico e conseguir visualizar as variaveis
previsao$class <- ifelse(previsao$bad >= 0.5, 'bad', 'good') # Transforma a probabilidade da árvore em dados nominais

# Verifica o acerto do modelo
confusionMatrix(table(previsao$class, credito_teste$class)) # 71%

# Modelo 2
modelo2 <- rpart(class ~ ., data = credito_treino, method = 'class', control = rpart.control(minsplit = 20))
plot(modelo2)
text(modelo2)

previsao <- predict(modelo2, newdata = credito_teste)
previsao <- as.data.frame(previsao)
previsao$class <- ifelse(previsao$bad >= 0.5, 'bad', 'good')

confusionMatrix(table(previsao$class, credito_teste$class))

# Modelo 3
modelo3 <- prune(modelo2, cp = 0.05) # Realiza uma poda da árvote
previsao <- predict(modelo3, newdata = credito_teste)
previsao <- as.data.frame(previsao)
previsao$class <- ifelse(previsao$bad >= 0.5, 'bad', 'good')
confusionMatrix(table(previsao$class, credito_teste$class))
