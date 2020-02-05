library(fpc) # Kmeans
library(cluster) # Funções de visualização

cls <- kmeans(iris[, 1:4], centers = 3)
cls$cluster
cls$centers

table(iris$Species, cls$cluster) # Compara a classe com agrupamento criado

# Gera gráfico passando o dataset, junto com o valor do cluster agrupado
plotcluster(x = iris[, 1:4], clvecd = cls$cluster)
clusplot(x = iris[, 1:4], clus = cls$cluster) # Separado pelos cluster

plot(iris[,3:4], col = iris$Species, pch = cls$cluster)