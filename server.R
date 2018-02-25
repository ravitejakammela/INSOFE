library(shiny)
library(shinydashboard)
library(survival)
library(GGally)
library(dplyr)
library(data.table)
library(caret)
library(caTools)
library(randomForest)

shinyServer(function(input, output){
  
  data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {return()}
    
    if (input$fileType_Input == "1") {
      read.csv(inFile$datapath,
               header = TRUE,
               stringsAsFactors = FALSE)
    } else {
      read.xlsx(inFile$datapath,
                header = TRUE,sheetIndex = 1,
                stringsAsFactors = FALSE)
    }
  })

  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  output$plot1 <- renderPlot({
    if(is.null(data())){return()}
    maintenance_graph <- survfit(Surv(lifetime,broken) ~ 1, data = data())
    ggsurv(maintenance_graph)
  })
  
  
  output$plot2 <- renderPlot({
    if(is.null(data())){return()}
    maintenance_graph3 <- survfit(Surv(lifetime,broken) ~ provider, data = data())
    ggsurv(maintenance_graph3)
  })
  
  output$plot3 <- renderPlot({
    if(is.null(data())){return()}
    maintenance_broken <- data() %>% filter(broken == 1)
    par(mfrow=c(1,3))
    boxplot(lifetime~broken,data=maintenance_broken, main="Borken machines", xlab="", ylab="Lifetime",col="green")
    boxplot(lifetime~team,data=maintenance_broken, main="Per team", xlab="", ylab="",col="green")
    boxplot(lifetime~provider,data=maintenance_broken, main="Per provider", xlab="", ylab="",col="green")
    
  })
  
  
  output$file3 <- renderTable({
    if(is.null(data())){return()}
    dependantvars = Surv(data()$lifetime, data()$broken)
    survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data())
    Ebreak=predict(survreg, newdata=data(), type="quantile", p=.5)
    Forecast=data.frame(Ebreak)
    Forecast$lifetime=data()$lifetime
    Forecast$broken=data()$broken
    Forecast$RemainingLT=Forecast$Ebreak-data()$lifetime
    Forecast=Forecast[order(Forecast$RemainingLT),]
    ActionsPriority=Forecast[Forecast$broken==0,]
    ActionsPriorityDT <- head(ActionsPriority, n=35)
    df<-data.frame(data.table(ActionsPriorityDT))
    df
    
  })
  
})