library(dplyr)
library(ggplot2)
library(ggthemes)
library(rpart)
library(rpart.plot)

dataset <- read.csv(file.choose())

# Conhecendo os dados
dim(dataset)
str(dataset)
summary(dataset)

dataset$userid <- NULL # Remove o ID

# Alterando o nome das colunas
colnames(dataset) <- c('Idade', 'Dia', 'Ano', 'Mês', 'Sexo', 
                       'Tenure', 'Qtd_Amigos', 'Amizades_Iniciadas', 
                       'Curtidas', 'Curtidas_Recebidas', 'Celular_Curtida', 
                       'Celular_Curtidas_Recebidas', 'www_Curtidas', 'www_Curtidas_Recebidas')

dataset$Sexo <- ifelse(dataset$Sexo == 'male', 'Masculino',
                       ifelse(dataset$Sexo == 'female', 'Feminino', NA))

retornar_sexo <- function(x) {
    sexo_na <- x %>% 
                filter(is.na(Sexo)) %>% 
                select(Curtidas, Qtd_Amigos, Amizades_Iniciadas,
                       Curtidas_Recebidas, Celular_Curtida, Celular_Curtidas_Recebidas,
                       www_Curtidas, www_Curtidas_Recebidas)
  
    sexo <- rpart(Sexo ~ Curtidas + Qtd_Amigos + Amizades_Iniciadas + 
                         Curtidas_Recebidas + Celular_Curtida + 
                         Celular_Curtidas_Recebidas + www_Curtidas + www_Curtidas_Recebidas,
                  data = x) %>% 
              predict(newdata = sexo_na)
    
    sexo <- ifelse(sexo >= 0.6, 'Masculino', 'Feminino')
    return(sexo)
}

dataset[is.na(dataset$Sexo), 'Sexo'] <- retornar_sexo(dataset)
                  
# Análise exploratória dos dados - Sexo
dataset %>%
  filter(!is.na(Sexo)) %>% 
  group_by(Sexo) %>% 
  summarise(Quantidade = n()) %>% 
  select(Sexo, Quantidade) %>% 
  ggplot(aes(x = Sexo, y = Quantidade, fill = Sexo)) +
    geom_bar(stat = 'identity') +
    labs(title = 'Quantidade de usuários por gênero',
         x = 'Gênero',
         y = 'Quantidade')

# Total de amizades por sexo
dataset %>% 
  group_by(Sexo) %>% 
  summarise(qtde_amigos = sum(Qtd_Amigos)) %>% 
  ggplot(aes(x = Sexo, y = qtde_amigos, fill = Sexo)) +
    geom_bar(stat = 'identity') +
    labs(title = 'Quantidade de amizades por sexo',
         x = 'Gênero',
         y = 'Quantidade')

# Idades que mais utilizam facebook
dataset %>% 
  group_by(Idade) %>% 
  summarise(Qtde = n()) %>% 
  select(Idade, Qtde) %>% 
  arrange(-Qtde) %>% 
  ggplot(aes(x = as.character(Idade), y = Qtde)) +
    geom_bar(stat = 'identity')+ 
    geom_text(aes(label = as.character(Idade)), 
              vjust = -0.6, 
              color = 'black',
              size = 3.0,
              position = 'dodge') +
    labs(title = 'Quantidade de usuários por idade',
         x = 'Idade',
         y = 'Quantidade') +
    theme_economist_white()

# Sexo que mais possuí amizades no Facebook nas idades outlier
dataset %>% 
  filter(Idade %in% c(18, 23, 33, 53, 63)) %>% 
  group_by(Sexo, Idade) %>% 
  summarise(qtde = sum(Qtd_Amigos)) %>%
  arrange(Idade) %>% 
  ggplot(aes(x = as.character(Idade), y = qtde, fill = Sexo)) +
    geom_col() +
    theme(legend.position = 'top' ) +
    labs(title = 'Quantidade de amizades por gênero',
         x = 'Gênero',
         y = 'Quantidade')

# Sexo que recebeu maior número de curtidas
dataset %>% 
  group_by(Sexo) %>% 
  summarise(qtde = sum(Curtidas)) %>% 
  ggplot(aes(x = Sexo, y = qtde, fill = Sexo)) +
    geom_bar(stat = 'identity') +
    labs(title = 'Quantida de curtidas recebidas por gênero',
         x = 'Gênero',
         y = 'Quantidade')
  
# Quantidade de curtidas dadas por sexo
dataset %>% 
  group_by(Sexo) %>% 
  summarise(qtd_web = sum(www_Curtidas),
            qtd_mob = sum(Celular_Curtida),
            qtd_rec_www = sum(www_Curtidas_Recebidas),
            qtd_rec_mob = sum(Curtidas_Recebidas)) %>% 
  select(Sexo, qtd_web, qtd_mob, qtd_rec_www, qtd_rec_mob) %>% 
  ggplot(aes(x = Sexo, y = qtd_web, fill = Sexo, group = 1)) +
    geom_bar(stat = 'identity') +
    geom_line(aes(x = Sexo, y = qtd_rec_www, colour = 'Qtde' )) +
    scale_color_discrete(name = 'Qtd. Curtidas Recebidas')
