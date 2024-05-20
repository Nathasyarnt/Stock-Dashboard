library(shiny)
library(readxl)
ui <- fluidPage(
  titlePanel("Dashboard Time Series Multinational Company in Indonesia"),
  h4("Created by : Natasya"),
  h5("Statistika Bisnis ITS"),
  
  sidebarLayout(
    sidebarPanel(
      imageOutput(outputId = "home_img"),
      
      selectInput(inputId = "Model", label = "Company",
                  choices = c("NIKE", "APPLE"), selected = "NIKE" 
      ),
      
      radioButtons(inputId = "Model", label = "ARIMA Model",
                   choices = c("ARIMA 1,2,1", "ARIMA 0,2,2"), choiceValues = "ARIMA 1,2,1"
      ),
    ),
    
    mainPanel(
      h4(strong("Time Series Plot ")),
      plotOutput(outputId = "trendplot"),
      
      h4(strong("Forecasting Plot")),
      plotOutput(outputId = "tsfore"),
    )
  ),
  
  h4(strong("Parameter and Goodness Model")),
  fluidRow(verbatimTextOutput(outputId = "coef")),
  
  h4(strong("Forecasting")),
  fluidRow(dataTableOutput(outputId = "table")),
  
  h4(strong("Description Project")),
  p(style="text-align: justify; font-size = 25px","Dashboard Time Series aims to find out
     time series analysis related to the characteristics of the time series plot and its subsequent forecasting
     determine how many predictions for the next 10 days from Company Indonesia shares. Apart from that you can find out
     the best ARIMA model from each Indonesian Stock Company."),
)
server <- function(input, output) {
  
  output$home_img <- renderImage({
    
    list(src = "C:/Users/Natas/Downloads/company.png",
         width = "100%",
         height = 350)
    
  }, deleteFile = F)
  
  
  output$trendplot <-renderPlot({
    
    library(fpp2)
    require(gridExtra)
    
    stock <- read_xlsx("C:/Users/Natas/Downloads/dataevd.xlsx")
    p1 <- autoplot(ts(stock$`Close nike`)) + ylab("Stock Value")+
      ggtitle("Nike Time Series Plot")
    
    p2 <-autoplot(ts(stock$`Close apple`))+ ylab("Stock Value")+
      ggtitle("Apple Time Series Plot")
    
    grid.arrange(p1,p2, ncol=2)
    
  })
  
  output$tsfore <- renderPlot({
    stock <- read_xlsx("C:/Users/Natas/Downloads/dataevd.xlsx")
    if (input$model =="ARIMA 1,2,1" && input$Model =="NIKE"){
      mod <- arima(stock$`Close nike`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 1,2,1" && input$Model =="APPLE"){
      mod <- arima(stock$`Close apple`, order=c(0,2,2), include.mean = FALSE)
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="APPLE"){
      mod <- arima(stock$`Close apple`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="NIKE"){
      mod <- arima(stock$`Close nike`, order=c(0,2,2), include.mean = FALSE)
    } else {
      mod <- arima(stock$`Close apple`, order=c(0,0,1), include.mean = FALSE)
    }
    p3 <- autoplot(forecast(mod, h = 10)) + ylab("Value")+ 
      ggtitle("Forecast for 10 day later")
    p3
  })
  
  output$coef <- renderPrint({
    stock <- read_xlsx("C:/Users/Natas/Downloads/dataevd.xlsx")
    if (input$model =="ARIMA 1,2,1" && input$Model =="NIKE"){
      mod <- arima(stock$`Close nike`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 1,2,1" && input$Model =="APPLE"){
      mod <- print("Bukan Model Terpilih")
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="APPLE"){
      mod <- arima(stock$`Close apple`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="NIKE"){
      mod <- print("Bukan Model Terpilih")
    } else {
      mod <- arima(stock$`Close apple`, order=c(0,0,1), include.mean = TRUE)
    }
    model <- mod
    model
  })
  
  output$table <- renderDataTable({
    stock <- read_xlsx("C:/Users/Natas/Downloads/dataevd.xlsx")
    if (input$model =="ARIMA 1,2,1" && input$Model =="NIKE"){
      mod <- arima(stock$`Close nike`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 1,2,1" && input$Model =="APPLE"){
      mod <- arima(stock$`Close apple`, order=c(0,2,2), include.mean = FALSE)
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="APPLE"){
      mod <- arima(stock$`Close apple`, order=c(1,2,1), include.mean = FALSE)
    } else if (input$model =="ARIMA 0,2,2" && input$Model =="NIKE"){
      mod <- arima(stock$`Close nike`, order=c(0,2,2), include.mean = FALSE)
    } else {
      mod <- arima(stock$`Close apple`, order=c(0,0,1), include.mean = false)
    }
    data<-forecast(mod, h =10)
    data<-as.data.frame(data)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)