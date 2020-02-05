library(shiny)
library(magrittr)

dados <- read.csv(file = 'slr12.csv', sep = ';')
modelo <- lm(CusInic ~ FrqAnual, data = dados)

ui <- fluidPage(
   
  # Application title
  titlePanel("Previsão de Custo Inicial para Montar uma Franquia"),
   
  fluidRow(
    column(4,
      h2("Dados"),
      tableOutput(outputId = "Dados")
    ),
    column(8,
      plotOutput(outputId = "Graf")
    )
  ),
  fluidRow(
    column(6,
      h3("Valor Anual da Franquia:"),
      numericInput(inputId = "NovoValor",label = "Insira Novo Valor", value = 1500,min = 1, max = 9999999),
      actionButton(inputId = "Processar", label = "Processar")
    ),
    column(6,
      h1(textOutput(outputId = "Resultado"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Graf <- renderPlot({
    dados %>% 
      plot(CusInic ~ FrqAnual, data = ., col = 'blue')
    abline(modelo, col = 'red')
  })
  
  output$Dados <- renderTable({
    dados %>% 
      head(n = 10)
  })
  
  observeEvent(input$Processar, {
    val <- input$NovoValor
    previsao <- predict(modelo, data.frame(FrqAnual = eval(parse(text = val))))
    previsao <- paste0("Previsão de Custo Inicial em R$: ", round(previsao, digits = 2))
    output$Resultado <- renderText({
      previsao
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

