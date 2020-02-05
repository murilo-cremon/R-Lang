library(arules) # Pacote do algoritmo CBA e Apriori para regra de associação
library(aruleViz) # Pacote para visualização de regra de associação

transacoes <- read.transactions(file.choose(), format = "basket", sep = ",") # Arquivos de transações
inspect(transacoes) # Verifica as regras geradas
image(transacoes) # Forma gráfica de visualizar as regras

# Suporte de 3%, confiançã de 40% e mínimo de direita e esquerda da regra
regras <- apriori(transacoes, parameter = list(supp = 0.03, conf = 0.4, minlen = 2 ))
summary(regras) # Resumo dos dados gerados pelo algoritmo
inspect(regras)

plot(regras, method = "graph")
plot(regras, method = "matrix")
plot(regras, method = "matrix", engine = "3d", measure = "lift", control = list(reorder = TRUE))
plot(regras, method = "grouped")