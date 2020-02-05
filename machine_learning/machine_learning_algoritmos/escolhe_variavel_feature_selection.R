library(FSelector)
library(e1071)
library(caret)

anuncios <- read.csv(file.choose(), sep = ",", header = TRUE )
head(anuncios)
dim(anuncios)

particao <- createDataPartition(y = 1:3279, p = 0.7)
anuncios_treino <- anuncios[particao$Resample1, ]
anuncios_teste <- anuncios[-particao$Resample1, ]
dim(anuncios_treino)
dim(anuncios_teste)

modelo <- naiveBayes(ad. ~ ., data = anuncios_treino)
previsao <- predict(modelo, newdata = anuncios_teste)
confusionMatrix(previsao, anuncios_teste$ad.)

atributos <- chi.squared(ad. ~ ., anuncios) # COloca probilibdade maior das seleções
atributos

subgrupo <- cutoff.k(atributos, 7) # Pega os 7 (maiores) primeiros mais importantes para o ML
subgrupo

modelo <- naiveBayes(ad. ~ V3 + V2 + C1 + V1244 + V1400 + V352 + V1355, data = anuncios_treino)
previsao <- predict(modelo, newdata = anuncios_teste)
confusionMatrix(previsao, anuncios_teste$ad.)


