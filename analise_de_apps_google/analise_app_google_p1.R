library(stringr)
library(lubridate)
library(dplyr)
library(highcharter)
library(xts)
library(tidyr)

dataset <- read.csv(file.choose(), sep = ",", header = TRUE)

# Conhecendo os dados
dim(dataset)
head(dataset, n = 5)
colnames(dataset)
summary(dataset)
str(dataset)

# Data set limpo
data_clean <- dataset %>% 
                mutate(Installs = gsub("\\+", "", as.character(Installs)),
                       Installs = as.numeric(gsub(",", "", Installs)),
                       Size = gsub("M", "", Size),
                       Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
                       Reviews = as.numeric(Reviews),
                       Price = as.numeric(Price),
                       Last.Updated = myd(Last.Updated),
                       Min.Android.Ver = gsub("Varies with device", NA, Android.Ver),
                       Min.Android.Ver = as.numeric(substr(Min.Android.Ver, start = 1, stop = 3)),
                       Android.Ver = NULL) %>% 
                filter(Type %in% c("Free", "Paid"))

str(data_clean)

# Checando número de linhas removendo as linhas duplicadas
nrow(data_clean %>% 
       distinct())

# Realizando análise de valores NA
data_clean %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather() %>% 
  filter(value > 1) %>% 
  arrange(-value) %>% 
  hchart("column", hcaes(x = "key", y = "value", color = "key")) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(text = "Columns With NA values")

# NA na coluna Raiting
data_clean %>% 
  filter(is.na(Rating)) %>% 
  count(Installs) %>% 
  arrange(-n) %>% 
  hchart("column", hcaes(x = "Installs", y = "n")) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = "Installation with no Raiting")


# NA na coluna Size
data_clean %>% 
  filter(is.na(Size)) %>% 
  count()

# NA na columa Minimum Android Version
dataset %>% 
  filter(Android.Ver %in% "Varies with device") %>% 
  count()

# Mostrar o número de categorias mais populares
data_clean %>% 
  count(Category, Installs) %>% 
  group_by(Category) %>% 
  summarize(TotalInstalls = sum(as.numeric(Installs))) %>% 
  arrange(-TotalInstalls) %>% 
  hchart("scatter", hcaes(x = "Category", y = "TotalInstalls", size = "TotalInstalls", color = "Category")) %>% 
  hc_add_theme(hc_theme_538())  %>% 
  hc_title(text = "Most popular categories (# of insalls)")

# Tamanho da aplicação
data_clean %>% 
  count(Size) %>% 
  hchart("area", hcaes(x = "Size", y = "n")) %>% 
  hc_colors("#fb4901") %>% 
  hc_add_theme(hc_theme_ffx()) %>% 
  hc_title(text = "Distribution of application size (in MB)")

hcboxplot(x = data_clean$Size,
          var = data_clean$Type, 
          outliers = TRUE,
          color = "#fb4901",
          fillColor = "lightblue") %>% 
  hc_chart(type = 'column') %>% 
  hc_add_theme(hc_theme_ffx()) %>% 
  hc_title(text = "Tamanho da aplicação em MB")

# Instalações
tmp <- data_clean %>% 
          group_by(Installs.Group = cut(Installs, breaks = seq(0, 1000000000, by = 10000))) %>% 
          summarise(n = n())

highchart() %>% 
  hc_chart(type = "pie") %>% 
  hc_add_series_labels_values(labels = tmp$Installs.Group, values = tmp$n) %>% 
  hc_title(text = "Number of installs (groups per 10k)") %>% 
  hc_add_theme(hc_theme_economist())

# Tipo
tmp <- data_clean %>% 
          count(Type) %>% 
          mutate(perc = round(( n / sum(n)) * 100))

hciconarray(tmp$Type,
            tmp$perc,
            icons = "android",
            size = 5) %>% 
  hc_title(text = "Percentage of paid vs free apps")
          
data_clean %>% 
  group_by(Category, Type) %>% 
  summarise(n = n()) %>% 
  mutate(perc = round((n / sum(n)) * 100)) %>% 
  hchart("bar", hcaes(x = "Category", y = "perc", group = "Type")) %>% 
  hc_plotOptions(series = list(stacking = "normal")) %>% 
  hc_title(text = "Percentage of free vs Paid by Category") %>% 
  hc_add_theme(hc_theme_flat())

# Preço
data_clean %>% 
  filter(Type == "Paid") %>% 
  group_by(Category) %>% 
  summarise(Price = median(Price)) %>% 
  arrange(-Price) %>% 
  select(Price, Category) %>% 
  hchart("treemap", hcaes(x = "Category", value = "Price", color = "Price")) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(text = "Median price per category")

# Total do preço por categória de acordo com quantidade de instalações
data_clean %>% 
  filter(Type == "Paid") %>% 
  group_by(Category) %>% 
  mutate(Total_Paid = Price * Installs) %>% 
  summarise(USD_Paid = sum(Total_Paid)) %>% 
  arrange(-USD_Paid) %>% 
  hchart("treemap", hcaes(x = "Category", value = "USD_Paid", color = "USD_Paid")) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(title = "Total amount speny by category (installs+ price)")