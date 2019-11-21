#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(dplyr)
require(MASS)
library(data.table)
library(lubridate)
library(ggplot2)
library(car)
library(caret)
library(gridExtra)
library(plyr)
library(tidyverse)
library(rpart)

conc <- read.csv("C:/ASIGNATURAS UC3M/FOURTH TERM/ADVANCED REGRESSION/PART2/P2/dataset.csv")
conc=data.frame(conc)
conc$month=factor(conc$month)
colnames(conc)=c("air_quality","wind_direction","wind_speed","ceil_hgt","visibility","air_temperature",
                 "dew_point","atmos_pressure","relative_humidity", "month")

in_train <- createDataPartition(conc[,1], p = 0.8, list = FALSE)  # 80% for training
training <- conc[in_train,]
testing <- conc[-in_train,]

training = training %>% mutate(lag1air_quality=lag(air_quality)) %>% drop_na()
testing = testing %>% mutate(lag1air_quality=lag(air_quality)) %>% drop_na()
training$dewair = training$air_temp*training$dew_point
testing$dewair = testing$air_temp*testing$dew_point

mod2=air_quality ~wind_direction+wind_speed+ceil_hgt+visibility+atmos_pressure+
  relative_humidity+month+air_temperature+air_temperature:dew_point+lag1air_quality
ModelTree = value ~ wd+ws+ceil_hgt+visibility+atmos_pres+RH+month+dewair+lag1value

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, repeats = 2)
test_results <- data.frame(air_quality = testing$air_quality)

# Define UI for application that draws a histogram
ui <- navbarPage("Air quality London Harlington",
                 tabPanel("Purpose of the App",
                          mainPanel(
                            div(style="text-align:justify",h4("The main purpose of this app is to show a descriptive
                                                              analysis of the variables which have an important 
                                                              influence in the air quality. Also, other tab  
                                                              is done for model selection, applying statistical and machine learning
                                                              tools where different parameters are shown, such as R squared, 
                                                              RMSE, MAE, prediction intervals or histogram of the prediction error. 
                                                              "),
                                h4("The model taking into account for model selection is:"),
                                h6("air_quality~wind_direction+wind_speed+ceil_hgt+visibility+atmos_pressure+relative_humidity+month+air_temperature:dew_point+lag1airquality"),
                                
                                h4("Regarding the units of the different variables we can say that 
                                   air quality is expressed in micrograms/cubic metres, wind speed in 
                                   m/s, wind direction in degrees, visibility in metres, air and dew point
                                   temperature in degrees, atmosphere pressure in milibars and relative humidity
                                   in %."))
                                )),
                 tabPanel("Descriptive Analisys",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId="v1",label = "Predictor 1",choices = colnames(conc)[c(-1,-10,-11,-12)],selected="visibility"),
                                selectInput(inputId="v2",label = "Predictor 2",choices = colnames(conc)[c(-1,-10,-11,-12)],selected="month"),
                                selectInput(inputId="v3",label = "Predictor 3",choices = colnames(conc)[c(-1,-10,-11,-12)],selected="atmos_pressure"),
                                selectInput(inputId="v4",label = "Predictor 4",choices = colnames(conc)[c(-1,-10,-11,-12)],selected="relative_humidity")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput(outputId = "plots")
                              ) 
                            ),
                            sidebarLayout(
                              sidebarPanel(h3("Densities"),
                                selectInput(inputId="v5",label = "Variable 1",choices = colnames(conc)[c(-1,-11)],selected="air_quality"),
                                selectInput(inputId="v6",label = "Variable 2",choices = colnames(conc)[c(-1,-11)],selected="atmos_pressure"),
                                selectInput(inputId="v7",label = "Variable 3",choices = colnames(conc)[c(-1,-11)],selected="relative_humidity"),
                                selectInput(inputId="v8",label = "Variable 4",choices = colnames(conc)[c(-1,-11)],selected="wind_speed")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput(outputId = "plots2")
                              ) 
                            )
                    
                            
                          ) #fluidPage
                 ),
                 
                 tabPanel("Model Selection",
                          fluidPage( 
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId="v9",label = "Model selection",choices = c("lm","leapForward","leapBackward",
                                                                                           "leapSeq","ridge","lasso","glmnet",
                                                                                           "pcr","pls","robust","knn","svm","trees","gradboost","rforest"),selected = "lm")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput(outputId = "plots3")
                              ) 
                            )
                          
                 )
                 )
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plots=renderPlot({
    if(input$v1=="month"){
      plot1=ggplot(training,aes(month,air_quality))+
        geom_boxplot()+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab("months")
    }else{
      p1=training[,input$v1]
      plot1=ggplot(training,aes(p1,air_quality))+
        geom_point()+
        ggtitle(paste("Air quality - ",input$v1))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab(input$v1)
    }
    
    if(input$v2=="month"){
      plot2=ggplot(training,aes(month,air_quality))+
        geom_boxplot()+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab("months")
      
    }else{
      p2=training[,input$v2]
      plot2=ggplot(training,aes(p2,air_quality))+
        geom_point()+
        ggtitle(paste("Air quality - ",input$v2))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab(input$v2)
      
    }
    
    if(input$v3=="month"){
      plot3=ggplot(training,aes(month,air_quality))+
        geom_boxplot()+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab("months")
      
    }else{
      p3=training[,input$v3]
      plot3=ggplot(training,aes(p3,air_quality))+
        geom_point()+
        ggtitle(paste("Air quality - ",input$v3))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab(input$v3)
      
    }
    
    if(input$v4=="month"){
      plot4=ggplot(training,aes(month,air_quality))+
        geom_boxplot()+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab("months")
      
    }else{
      p4=training[,input$v4]
      plot4=ggplot(training,aes(p4,air_quality))+
        geom_point()+
        ggtitle(paste("Air quality - ",input$v4))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Air quality")+
        xlab(input$v4)
    }
    grid.arrange(plot1,plot2,plot3,plot4, nrow = 2, ncol=2)
    
  })
  
  output$plots2=renderPlot({
    p5=training[,input$v5]
    plot5=ggplot(training, aes(p5))+
      geom_density(color="darkblue",fill="lightblue")+
      ggtitle(paste("Density ",input$v5))+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab(input$v5)
    
    p6=training[,input$v6]
    plot6=ggplot(training, aes(p6))+
      geom_density(color="darkblue",fill="lightblue")+
      ggtitle(paste("Density ",input$v6))+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab(input$v6)
    
    p7=training[,input$v7]
    plot7=ggplot(training, aes(p7))+
      geom_density(color="darkblue",fill="lightblue")+
      ggtitle(paste("Density ",input$v7))+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab(input$v7)
    
    p8=training[,input$v8]
    plot8=ggplot(training, aes(p8))+
      geom_density(color="darkblue",fill="lightblue")+
      ggtitle(paste("Density ",input$v8))+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab(input$v8)
    
    grid.arrange(plot5,plot6,plot7,plot8, nrow = 2, ncol=2)
  
})
  
  output$plots3=renderPlot({
    
    if(input$v9=="lm"){
      mod2ms <- train(mod2, data = training, 
                      method = input$v9, 
                      preProc=c('scale', 'center'),
                      trControl = ctrl)
      }
    if(input$v9=="leapForward"){
      mod2ms <- train(mod2, data = training, 
                        method = input$v9, #instead of original package 
                        preProc=c('scale', 'center'),
                        tuneGrid = expand.grid(nvmax = 2:21),
                        trControl = ctrl)
      
    }
    if(input$v9 == "leapBackward"){
      mod2ms <- train(mod2, data = training, 
                         method = input$v9, #backward regression in pack. leap
                         preProc=c('scale', 'center'),
                         tuneGrid = expand.grid(nvmax = 2:21),
                         trControl = ctrl)
      
    }
    if(input$v9=="leapSeq"){
      mod2ms <- train(mod2, data = training, 
                         method = input$v9, 
                         preProc=c('scale', 'center'),
                         tuneGrid = expand.grid(nvmax = 2:21),
                         trControl = ctrl)
      
    }
    if(input$v9=='ridge'){
      ridge_grid <- expand.grid(lambda = seq(0, .1, length = 20))
      mod2ms <- train(mod2, data = training,
                          method=input$v9,
                          preProc=c('scale','center'),
                          tuneGrid = ridge_grid,
                          trControl=ctrl)
    }
    
    if(input$v9=='lasso'){
      lasso_grid <- expand.grid(fraction = seq(.01, 1, length = 20))
      mod2ms <- train(mod2, data = training,
                          method=input$v9,
                          preProc=c('scale','center'),
                          tuneGrid = lasso_grid,
                          trControl=ctrl)
    }
    
    if(input$v9=="glmnet"){
      elastic_grid = expand.grid(alpha = seq(0, .2, 0.01), lambda = seq(0, .1, 0.01)) 
      mod2ms <- train(mod2, data = training,
                           method=input$v9,
                           preProc=c('scale','center'),
                           tuneGrid = elastic_grid,
                           trControl=ctrl)
    }
    
    if(input$v9=='pcr'){
      mod2ms <- train(mod2, data = training,
                        method=input$v9,
                        preProc=c('scale','center'),
                        tuneGrid = expand.grid(ncomp = 2:10),
                        trControl=ctrl)
    }
    if(input$v9=="pls"){
      mod2ms <- train(mod2, data = training,
                        method=input$v9,
                        preProc=c('scale','center'),
                        tuneGrid = expand.grid(ncomp = 2:10),
                        trControl=ctrl)
    }
    if(input$v9=="robust"){
      mod2ms <- rlm(mod2, data=training, psi=psi.huber)
    }
    if(input$v9=="knn"){
      mod2ms <- train(mod2, data = training,
                        method = "kknn",   
                        preProc=c('scale','center'),
                        tuneLength = 10,
                        trControl = ctrl)
    }
    
    if(input$v9=="svm"){
      mod2ms <- train(mod2, data = training,
                        method = "svmRadial",
                        tuneGrid = data.frame(.C = seq(.1, 2, 0.1), .sigma = seq(0.01, 0.05, 0.01)), 
                        preProc=c('scale','center'),
                        trControl = ctrl)
    }
    
    if(input$v9=="trees"){
      ModelTree = value ~ wd+ws+ceil_hgt+visibility+atmos_pres+RH+month+dewair+lag1value
      mod2ms <- rpart(ModelTree, data = training)
      
    }
    
    if(input$v9 == "gradboost"){
      gbmGrid<-expand.grid(n.trees=seq(100,400,by=100),interaction.depth=seq(1,4,by=1),shrinkage=c(.001,.01,.1), n.minobsinnode=10)
      mod2ms <- train(mod2, data = training,
                        method = "gbm", # Generalized Boosted Regression Models
                        preProc=c('scale','center'),
                        tuneGrid = gbmGrid,
                        trControl = ctrl, 
                        verbose = FALSE)
      
    }
    
    if(input$v9 == "rforest"){
      mod2ms <- train(mod2, data = training,
                       method = "rf",
                       preProc=c('scale','center'),
                       trControl = ctrl,
                       ntree = 250,
                       tuneGrid = expand.grid(mtry = c(10,20,30)), 
                       verbose = FALSE)
    }
    
    
    test_results$mod2ms<- predict(mod2ms, testing)
    result=data.frame(postResample(pred = test_results$mod2ms,  obs = test_results$air_quality))
    
    yhat = test_results$mod2ms
    head(yhat) 
    y = test_results$air_quality
    error = y-yhat
    noise = error[1:30]
    sd.noise = sd(noise)
    # If noise is a normal variable, then a 90%-CI can be computed as
    lwr = yhat[31:length(yhat)] - 1.65*sd.noise
    upr = yhat[31:length(yhat)] + 1.65*sd.noise
    predictions = data.frame(real=y[31:length(y)], fit=yhat[31:length(yhat)], lwr=lwr, upr=upr)
    predictions = predictions %>% mutate(out=factor(if_else(real<lwr | real>upr,1,0)))
    #mean(predictions$out==1)
    
    plot9=qplot(test_results$mod2ms, test_results$air_quality) + 
      labs(title=paste("Linear Regression Observed VS Predicted",input$v9), x="Predicted", y="Observed") +
      lims(x = c(-20,40), y = c(-20, 40)) +
      geom_abline(intercept = 0, slope = 1, colour = "blue") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(legend.position = "none")
    
    plot10=ggplot()+aes(rownames(result),result[,1],fill=rownames(result))+
      geom_bar(stat="identity")+
      ggtitle(paste("Prediction measures",input$v9))+
      theme(plot.title = element_text(hjust = 0.5))
      
    
    plot11=ggplot()+aes(error)+geom_histogram(bins = 9,fill="lightblue",color="darkblue")+
      ggtitle(paste("Histogram Prediction error",input$v9))+
      theme(plot.title = element_text(hjust = 0.5))
    
    plot12=ggplot(predictions, aes(x=fit, y=real))+theme_minimal()+
      geom_point(aes(color=out)) + theme(legend.position="none") +
      lims(x = c(10.5,30), y = c(-20, 40)) +
      geom_ribbon(data=predictions,aes(ymin=lwr,ymax=upr),alpha=0.3) +
      labs(title = paste("Prediction intervals 90%-CI",input$v9), x = "prediction",y="Real value")
      
    
    grid.arrange(plot9,plot10,plot11,plot12,nrow = 2, ncol=2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

