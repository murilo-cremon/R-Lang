library(class)

iris_nova <- iris

iris_teste <- iris_nova[1, ]
nova_iris <- iris_nova[-1, ]

dim(iris_teste)
dim(nova_iris)

previsao <- knn(train = nova_iris[, 1:4], test = iris_teste[, 1:4], cl = nova_iris[, 5], k = 3)