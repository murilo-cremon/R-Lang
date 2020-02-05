library(e1071)

carros <- read.csv(file = "car.data", sep = ",", header = TRUE)

modelo <- naiveBayes(class ~ ., data = carros)

saveRDS(modelo, file = "./RDS/naiveBayesCarros.rds")