library(knitr)
library(scales)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Carregando dataset
AppData <- read.csv(file.choose())

# Transformar as colunas em fatores
AppData$Category <- factor(AppData$Category)
AppData$Installs <- factor(AppData$Installs)
AppData$Type <- factor(AppData$Type)
AppData$Content.Rating <- factor(AppData$Content.Rating)

# Verificando dados faltantes
missing_date <- AppData %>% 
                  map_df(function(x) sum(is.na(x))) %>% 
                  gather(feature, num_nulls) %>% 
                  arrange(-num_nulls) %>% 
                  mutate(percent_missing = num_nulls / nrow(AppData) * 100) %>% 
                  kable(digits = c(0,0,0), col.names = c("Feature", "Nº Nulls", "% Faltante"))
                  
# Distruibuição de Classificação
AppData %>% 
  filter(!is.na(Rating)) %>% 
  ggplot(aes(Rating)) +
  geom_histogram(binwidth = .1) +
  scale_x_continuous(limits = c(0, 5.1)) +
  labs(title = "App Rating Histogram",
       x = "Rating",
       y = "Count") +
  theme_economist_white()

# Numéro de Reviews
num_review <- function(x) {
  if (x > 100000) "1,000,000 +"
  else if (x > 10000) "10,00 - \n999,999"
  else if (x > 100) "100 - \n9,99"
  else if ( x > 0) "1 - 99"
  else "Zero"
}

AppT <- AppData %>% 
          mutate(Review_Bucket =ifelse(Reviews > 100000,"1,000,000 +",
                                       ifelse(Reviews >10000,"10,000 - \n999,999",
                                              ifelse(Reviews >100,"100 - \n9,999",
                                                     ifelse(Reviews >0,"1 - 99","Zero")))))

order <- c("Zero","1 - 99","100 - \n9,999","10,000 - \n999,999","1,000,000 +")

AppTG <- AppT %>% 
           filter(!is.na(Review_Bucket)) %>% 
           group_by(Review_Bucket) %>% 
           summarise(Count = n())


AppData %>% 
  filter(Type %in% c("Free", "Paid")) %>% 
  group_by(Type) %>% 
  summarise(Quantidade = n()) %>% 
  ggplot(aes(x = Type, y = Quantidade)) +
  geom_bar(stat = "identity") +
  labs(title = "Quantidade de Instalação por Tipo de Aplicativo",
       x = "Tipo de Aplicativo",
       y = "Quantidade")
           

AppData %>% 
  filter(is.na(Rating)) %>% 
  count(Installs) %>% 