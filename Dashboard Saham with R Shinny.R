library(shiny)
library(readxl)
library(tidyverse)
library(tseries)



# Define UI ----
ui <- fluidPage(
  titlePanel("Dashboard Time Series Multinational Company in Indonesia"),
  h4("Created by Natasya Septina Arianti", style= "color:blue"),
  strong("Departemen Statistika Bisnis"),
  p(strong("Institut Teknologi Sepuluh Nopember")),
  sidebarLayout(
    sidebarPanel(
      imageOutput(outputId = "img"),
      selectInput(
        inputId="model", label="Stock:", choices=c("NIKE", "APPLE"),
                selected ="NIKE"),
      p(),
      p(),
      #NIKE
      radioButtons(  
        inputId="stockmodel", label="Model for company's stock", choices=c("ARIMA(1,2,1)", "ARIMA(0,2,2)"), 
        selected ="ARIMA(1,2,1)"),
      submitButton(text="Submit"),
    ),
    mainPanel(
      h4(strong("Time Series and Forecasting Plot"), align="center"),
      h4(strong("Time Series Plot"), align="center", style="background-color : #A0E4CB",),
      splitLayout(cellwidths= c("50%", "50%"), 
                  plotOutput(outputId="tsplot"),style="border: 5px solid black",),      
      hr(), 
      h4(strong("Forecasting Plot"), align="center",style="background-color : #A0E4CB",),
      splitLayout(plotOutput(outputId="tspfore"),style="border: 5px solid black;",),    
    )
    ),
  hr(),
  h4(strong("Parameter and Goodness Model"), align="center",style="background-color : #A0E4CB",),
  verbatimTextOutput(outputId ="outmodel"),
  h6("Then the selected model is the model with the smallest AIC value", align=",left"),
  
  h4(strong("OUTPUT TABEL HASIL PARAMETER"), align="center",style="background-color : #A0E4CB",),
  hr(),
  fluidRow(dataTableOutput(outputId="coef"), style="margin: 10px; 20px; 10px; 20px; border: 5px solid blue ; padding: 10px"),
  hr(),
  
  h4(strong("DESCRIPTION"), align="center"),
  p(style="text-align: center; font-size: 14px; font-family: sans-serif","Dashboard Time Series aims to find out
     time series analysis related to the characteristics of the time series plot and its subsequent forecasting
     determine how many predictions for the next 10 days from Company Indonesia shares. Apart from that you can find out
     the best ARIMA model from each Indonesian Stock Company."),
  hr(),
)
# Define server logic ----
server <- function(input, output) {
  
  # logo
  output$img <- renderImage({
    list(src="C:/Users/Natas/Downloads/company.png", width = "100%", height =350)
  }, deleteFile = F)
  
  
  # plot 
  output$tsplot <- renderPlot({
    stock <- read_xlsx("C:/Users/IKA ARIANTO/Downloads/sahamevd.xlsx")
    
    
    require(gridExtra)
    library(fpp2)
    p1<- autoplot(ts(stock$`Close nike`))+ ylab("Stock Value") + ggtitle("NIKE Time Series Plot")
    p2<- autoplot(ts(stock$`Close apple`))+ ylab("Stock Value") + ggtitle("APPLE Time Series Plot")
    grid.arrange(p1,p2, ncol=2) # membagi plot menjadi ncol bagian
  })
  
  #peramalan
  output$tspfore <- renderPlot({
    stock <- read_xlsx("C:/Users/IKA ARIANTO/Downloads/sahamevd.xlsx")
    
    #rumusplot
    if (input$model == "NIKE" && input$stockmodel =="ARIMA(1,2,1)"){
      mod<- Arima(stock$`Close nike`, order =c(1,2,1), include.mean= F)
    } else if (input$model == "NIKE" && input$stockmodel =="ARIMA(0,2,2)"){
      mod<- Arima(stock$`Close nike`, order =c(0,2,2), include.mean= F)
    } else if (input$model == "APPLE" && input$stockmodel =="ARIMA(1,2,1)"){
      mod<- Arima(stock$`Close apple`, order =c(1,2,1), include.mean= F)
    } else if (input$model == "Apple" && input$stockmodel =="ARIMA(1,2,1)"){
      mod<- Arima(stock$`Close apple`, order =c(1,2,1), include.mean= F)
    }
    forecasting <- forecast(mod, h=10) 
    p4<- autoplot(forecasting) +ylab("Stock Value")+ggtitle("Forecast Stock Plot")
    p4
  })
  
  #printmodel
  
  output$outmodel <- renderPrint({
    stock <- read_csv ("C:/Users/IKA ARIANTO/Downloads/sahamevd.xlsx")
    
    if (input$stockmodel == "ARIMA(1,2,1)"){
      modd <- 
        Arima(stock$`Close nike`,order=c(1,2,1), include.mean = FALSE)
    } else if (input$modelsaham == "ARIMA(1,2,1)"){
      modd <- Arima(stock$`Close nike`,order=c(0,2,2), include.mean = FALSE)
    } else if (input$modelsaham == "ARIMA(0,2,2)"){
      modd <- Arima(stock$`Close apple`,order=c(1,2,1), include.mean = FALSE)
    } else if (input$modelsaham == "ARIMA(1,2,1)"){
      modd <- Arima(stock$`Close apple`,order=c(1,2,1), include.mean = FALSE)
    } else if (input$modelsaham == "ARIMA(0,2,2)"){
      modd <- Arima(stock$`Close apple`,order=c(0,2,2), include.mean = FALSE)
    } else {
      modd<- c("Output Parameter dan Model Terbaik akan ditampilkan disini!")
    }
    
    modelsaham <- modd
    modelsaham
  })
  
  #menampilkan tabel
  output$coef<- renderDataTable({
    stock <- read_csv ("C:/Users/IKA ARIANTO/Downloads/sahamevd.xlsx")
    if (input$model == "NIKE" && input$modelsaham =="ARIMA(1,2,1)"){
      mod<- Arima(stock$`Close nike`, order =c(1,2,1), include.mean= F)
    } else if (input$model == "NIKE" && input$modelsaham =="ARIMA(0,2,2)"){
      mod<- Arima(stock$`Close nike`, order =c(0,2,2), include.mean= F)
    } else if (input$model == "APPLE" && input$modelsaham =="ARIMA(1,2,1)"){
      mod<- Arima(stock$`Close apple`, order =c(1,2,1), include.mean= F)
    } else if (input$model == "APPLE" && input$modelsaham =="ARIMA(0,2,2)"){
      mod<- Arima(stock$`Close apple`, order =c(0,2,2), include.mean= F)
    }
    hasil <- forecast(mod, h=10) 
    hasil<-as.data.frame(hasil)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)