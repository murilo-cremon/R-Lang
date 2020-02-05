library(shiny)
library(e1071)

# Realiza a chamada do modelo
modelo <- readRDS(file = "./RDS/naiveBayesCarros.rds")

# Alimenta os combos
carros <- read.csv(file = "car.data", sep = ",")
buying <- sort(unique(carros$buying))
maint <- sort(unique(carros$maint))
doors <- sort(unique(carros$doors))
persons <- sort(unique(carros$persons))
lug_boot <- sort(unique(carros$lug_boot))
safety <- sort(unique(carros$safety))

classificacao <- function(x) {
  x <- as.character(x)
  if (x == "unacc")
    y <- "Resultado: Não aceitável"
  else if (x == "good")
    y <- "Bom"
  else if (x == "acc")
    y <- "Resultado: Aceitável"
  else
    y <- "Resultado: Muito bom"
  
  return(y)
}

ui <- fluidPage(
   
  titlePanel("Previsão de Qualdiade de Veículos"),
  
  fluidRow(
    column(4, selectInput(inputId = "buying", label = "Preço:", choices = buying)),
    column(4, selectInput(inputId = "maint", label = "Manutenção: ", choices = maint)),
    column(4, selectInput(inputId = "doors", label = "Portas:", choices = doors))
  ),
  fluidRow(
    column(4, selectInput(inputId = "persons", label = "Capacidade de Passageiros", choices = persons)),
    column(4, selectInput(inputId = "lug_boot", label = "Porta Malas", choices = lug_boot)),
    column(4, selectInput(inputId = "safety", label = "Segurança", choices = safety))
  ),
  fluidRow(
    column(12, 
      actionButton(inputId = "Processar", label = "Processar"),
      h1(textOutput(outputId = "Resultado"))
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$Processar, {
    novo_carro <- data.frame("buying" = input$buying,
                             "maint" = input$maint,
                             "doors" = input$doors,
                             "persons" = input$persons,
                             "lug_boot" = input$lug_boot,
                             "safety" = input$safety)
    previsao <- predict(modelo, newdata = novo_carro)
    
    output$Resultado <- renderText({
      classificacao(previsao)
    })
    
  })
}

shinyApp(ui = ui, server = server)