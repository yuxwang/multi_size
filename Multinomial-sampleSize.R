library(shiny)
if (!require("tidyverse")){
  install.packages("tidyverse")
  library("tidyverse")
}
if (!require("shinycssloaders")){
  install.packages("shinycssloaders")
  library("shinycssloaders")
}

if (!require("DT")){
  install.packages("DT")
  library("DT")
}

if (!require("nnet")){
  install.packages("nnet")
  library("nnet")
}

########server
server <- function(input, output) {
  #generate 1 data set
  getData<- reactive({
    mX = matrix(rnorm(input$size *3), input$size, 3)
    # coefficients for each choice
    vCoef1 = rep(0,3)
    vCoef2 = rnorm(3)
    vCoef3 = rnorm(3)
    # vector of probabilities
    vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))
    # multinomial draws
    mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
    dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x1=mX[,1],x2=mX[,2],x3=mX[,3])
  })
  output$modelsum<-renderPrint({
    set.seed(1234)
    simuData<-getData()
    trainingRows <- sample(1:nrow(simuData), 0.8*nrow(simuData))
    training <- simuData[trainingRows, ]
    test <- simuData[-trainingRows, ]
    #Build Multinomial Model
    multinomModel <- multinom(y ~ x1 + x2 +x3, data = simuData) # multinom Model
    summary(multinomModel)
    #z<-summary(multinomModel)$coefficients/summary(multinomModel)$standard.errors #Z
    
  })
  
  output$matrixTable<-renderPrint({
    set.seed(1234)
    simuData<-getData()
    trainingRows <- sample(1:nrow(simuData), 0.8*nrow(simuData))
    training <- simuData[trainingRows, ]
    test <- simuData[-trainingRows, ]
    #Build Multinomial Model
    multinomModel <- multinom(y ~ x1 + x2 +x3, data = simuData) # multinom Model
    #Predict on Test Data
    predicted_class <- predict (multinomModel, test)
    table(predicted_class, test$y)
    mean(as.character(predicted_class) != as.character(test$y))
  })
  
  
  #generate 500 dataset
  output$MCerror<-renderPlot({
    testError<-rep(NA,500)
     for (i in 1:500){
    #simulated data
    getData1<- reactive({
      mX = matrix(rnorm(input$size *3), input$size, 3)
      # coefficients for each choice
      vCoef1 = rep(0,3)
      vCoef2 = rnorm(3)
      vCoef3 = rnorm(3)
      # vector of probabilities
      vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))
      # multinomial draws
      mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
      dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x1=mX[,1],x2=mX[,2],x3=mX[,3])
    })
    simuData<-getData1()
    trainingRows <- sample(1:nrow(simuData), 0.8*nrow(simuData))
    training <- simuData[trainingRows, ]
    test <- simuData[-trainingRows, ]
    #Build Multinomial Model
    multinomModel<- multinom(y ~ x1 + x2 +x3, data = training,trace=FALSE) # multinom Model
    #Predict on Test Data
    predicted_class <- predict (multinomModel, test)
    testError[i]<-mean(as.character(predicted_class) != as.character(test$y))}
    #testError
    #create plot
      hist(testError,main="Test Misclassification Error")
      abline(v=mean(testError),col="red")
      text(mean(testError),10,"mean error",col="red")
      axis(1, at=mean(testError),labels=round(mean(testError),2), col.axis="red", las=2)
    })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(getData(),
                  caption=paste0("Simulated Data"),
                  options = list(scrollX = TRUE))
  })
}

#########ui
ui <- fluidPage(
  # Application title
  titlePanel("Investigation of Multinomial Regression vs. Sample Size"),
  #introduction
  p("Multinomial logistic regression is used to model nominal outcome variables, in which the log odds of the outcomes are modeled as a linear combination of the predictor variables.", style = "font-family: 'times'; font-si16pt"), br(),
  p("To generate efficient models, annotated data is required.There is a need to estimate the size of the annotated sample required to reach a performance target.", style = "font-family: 'times'; font-si16pt"), br(),
  p("In this shiny app, we investigate accuracy with different sample size for multinomial logistic regression via a simulation study.","There are three levels for response variable y=1,2,and 3.", style = "font-family: 'times'; font-si16pt"), br(),
  br(),
  
  fluidRow(
    column(4,
           sliderInput("size", "Sample Size ",
                       min = 10, max = 500, value = 30, step = 5)),
    column(8,
           titlePanel("Test Error Examinination using Monte Carlo simulation with 500 samples"),
           p("For every sample with n=500, 80% of the sample set was classified into training data, and used for generating model, and then use the rest 20% dataset as test set, to figure out misclassification rate."),
           plotOutput(outputId ="MCerror","histgram of reclassification rate",
                      height=400,width = 500)%>% withSpinner(color="#0dc5c1"))
    ),
  h1("One sample generated from MC",style = "font-family: 'times'; font-si16pt"),
  p("Below is all about one data sample generated with specific sample size."),br(),
  fluidRow(
    column(6,
           titlePanel("Summary of multinumial regression model"),
           verbatimTextOutput("modelsum")%>% withSpinner(type =4)),
    column(6,
           titlePanel("Confusion Matrix and Misclassification Error"),
           verbatimTextOutput("matrixTable")%>% withSpinner(type =4))
            )
  )

shinyApp(ui = ui, server = server)
