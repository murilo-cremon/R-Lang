library(shiny)
library(forecast)
library(ggplot2)
library(magrittr)

ts_retorno <- function(file, datas) {
  dado <- read.csv(file = file$datapath, header = FALSE)
  ano_inic <- lubridate::year(input$datas[1])
  mes_inic <- lubridate::month(input$datas[1])
  ano_fim <- lubridate::year(input$datas[2])
  mes_fim <- lubridate::month(input$datas[2])
  
  dado <- dado %>% 
    ts(start = c(ano_inic, mes_inic), end = c(ano_fim, mes_fim), frequency = 12)
  
  return(dado)
}

ui <- fluidPage(
  titlePanel("Sistema de Análise e Previsão de Séries Temporais"),
  fluidRow(
    column(4, 
      fileInput(inputId = "arquivo", label = "Escolha o arquivo: ", multiple = FALSE, accept = c(".csv"), buttonLabel = "Escolher", placeholder = "Seleciona um arquivo"),
      helpText("OBS: O arquivo deve conter apenas uma coluna, sem nome no cabeçalho. A frequência deve ser mensal.")
    ),
    column(4,
      dateRangeInput(inputId = "datas", label = "Periodo da Série", format = "mm/yyyy", language = "pt", start = "01/01/2000", end = "12/31/2013", startview = "year", separator = " até "),
      helpText("OBS: Para definir mês e ano, selecione um dia qualquer.")
    ),
    column(4,
      numericInput(inputId = "PeriodoPrevisao", label = "Informe quantos meses quer prever", value = 12, min = 1, max = 48),
      actionButton(inputId = "Processar", label = "Processar")
    )
  ),
  fluidRow(
    column(6, plotOutput(outputId = "GrafSerie")),
    column(6, plotOutput(outputId = "GrafHist"))
  ),
  fluidRow(
    column(6, plotOutput(outputId = "GrafBox")),
    column(6, plotOutput(outputId = "GrafDec"))
  ),
  fluidRow(
    column(6, plotOutput(outputId = "GrafPrev")),
    column(2,
      h1(textOutput(outputId = "llower")),
      tableOutput(outputId = "lower")
    ),
    column(2,
      h1(textOutput(outputId = "lmean")),
      tableOutput(outputId = "mean")
    ),
    column(2,
      h1(textOutput(outputId = "lupper")),
      tableOutput(outputId = "upper")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$Processar, {
    serie_temporal <- ts_retorno(input$arquivo, input$datas)

    output$GrafSerie <- renderPlot({
      autoplot(serie_temporal, main = "Série Original")
    })
    
    output$GrafHist <- renderPlot({
      hist(serie_temporal, main = "Histograma")
    })
    
    output$GrafBox <- renderPlot({
      boxplot(serie_temporal, main = "Boxplot")
    })
    
    dec <- decompose(serie_temporal)
    
    output$GrafDec <- renderPlot({
      autoplot(dec, main = "Decomposição")
    })
    
    modelo <- auto.arima(serie_temporal)
    valr <- input$PeriodoPrevisao
    previsao <- forecast(modelo, h = valr)
    
    output$lower <- renderTable({
      previsao$lower
    })
    
    output$mean <- renderTable({
      previsao$mean
    })
    
    output$upper <- renderTable({
      previsao$upper
    })
    
    output$llower <- renderText({"Lower"})
    output$lmean <- renderText({"Mean"})
    output$lupper <- renderText({"Upper"})
    
    output$GrafPrev <- renderPlot({
      autplot(previsao, main = "Previsão da Série")
    })
  })
}

shinyApp(ui = ui, server = server)