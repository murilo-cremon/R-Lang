library(bnlearn)
library(caret)

## REde bayseana trabalha com probabilidade, te retorna a probabildiade de ocorrer
# determiando evento em função de uma evidência
res <- hc(insurance)
plot(res)

modelo <- bn.fit(res, data = insurance)
set.seed(123)
cpquery(modelo,
        event = (Accident == 'Moderate' | Accident == 'Severe'),
        evidence = (Age == 'Senior' & RiskAversion == 'Adventurous' & MakeModel == 'SportsCar'))

dim(insurance[insurance$Age == 'Senior', ])[1]

subset(x = insurance, subset = Age == 'Senior')
