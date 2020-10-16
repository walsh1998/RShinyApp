library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr) 
library(mice)
library(caret)

data <- as.data.frame(read_excel("Program Data - 10_06_2020_revised.xlsx",sheet = "(Revised)Finalized Schools"))
data1 <- as.data.frame(read_excel("Program Data - 10_06_2020_revised.xlsx", 
                    sheet = "(Revised)Finalized Schools_1"))

############ Data preprocessing #################################################
#check data types
data$University <- as.factor(data$University)
data$Region <- as.factor(data$Region)

data$AvgStartingSalary <- ifelse(data$AvgStartingSalary == 0, NA, data$AvgStartingSalary)

#check the proportion of missing values

source("DataQualityReport.R")
DataQualityReport(data) %>%
  select(Attributes,Type,NumberMissing,PercentComplete) %>%
  filter(NumberMissing > 0) %>%
  arrange(desc(NumberMissing))


#According to DataQualityReport, we delete the columns that have 
#more than 50 % missing values.
data$`AvgScholarshipAmt$` <- NULL 
data$`DepositCost$` <-NULL
data$PctInternational <- NULL 
data$AvgTOEFL <- NULL        
data$StudentGMATGRE  <- NULL  
data$Pctwomen  <- NULL
data$StudentWorkExpMonths <- NULL          
data$StudentAge  <- NULL 
data$StudentGPA  <- NULL 
data$`AppFeeCost$` <- NULL 
data$`Associated School` <- NULL
data$`%CompaniesSponsor` <- NULL

data$FullTime <- as.factor(ifelse(data$FT == 1 | is.na(data$FT) == TRUE, "Yes/Unknown", "No"))
data$NonTraditional <- as.factor(ifelse(data$PT == 1 | is.na(data$PT) == TRUE |data$Hybrid == 1 | is.na(data$Hybrid) == TRUE
                                        |data$Online == 1 | is.na(data$Online) == TRUE, "Yes/Unknown", "No"))


#Combine Business School, Engineering,Science, and other into 1 categorical variable
data$BusinessSchool <- ifelse(data$BusinessSchool == 0, NA, 1)
data$EngSchool <- ifelse(data$EngSchool == 0, NA, 1)
data$ScienceSchool <- ifelse(data$ScienceSchool == 0, NA, 1)

data$SchoolType[data$BusinessSchool==1] <- "Business"
data$SchoolType[data$EngSchool==1] <- "Engineering"
data$SchoolType[data$ScienceSchool==1] <- "Science"

data$SchoolType <- ifelse(is.na(data$SchoolType) == TRUE, "Other", data$SchoolType)

data$SchoolType <- as.factor(data$SchoolType)
#Delete Business School, EngSchool, ScienceSchool variables

data$BusinessSchool <- NULL
data$EngSchool <- NULL
data$ScienceSchool <- NULL

#Create categorical variable that combines anayltics and data science and both into 1 variable
data$ProgramSpecialty <- NA
data$ProgramSpecialty[data$DataScience==1] <- "Data Science"
data$ProgramSpecialty[data$Analytics==1] <- "Analytics"
data$ProgramSpecialty[data$DataScience==1 & data$Analytics == 1] <- "Both"

data$ProgramSpecialty <- as.factor(data$ProgramSpecialty)
#Delete analytics and data science variables

data$Analytics <- NULL
data$DataScience <- NULL

#Make costs that are NA = 0 so they show up in data table

data$AvgCost <- ifelse(is.na(data$AvgCost) == TRUE, 0, data$AvgCost)

#Handle FT, PT, Online, Hybrid variables

#Handle course offerings

#Changes all app variables to yes/no
yesnovariables <- c("RequireSOP", "GMATGRE", "MinGMATGRE", "TOEFLIELTSPTEscores"
                    , "NumRecLetters", "Interview", "Appfee", "Scholarships", "IndustryProjects",
                    "CareerFairs", "CorpInfoSessions", "CompanySponsorsListed", 
                    "STEMCertified", "R", "Python", "SAS", "DatabaseSQL", "DataMiningDescriptiveAnalytics",
                    "PredictiveAnalytics", "MachineLearning", 
                    "DeepLearning", "PrescriptiveAnalyticsOptimization", 
                    "Simulation", "DataVisualizationTableau", 
                    "BigDataHadoopSparkHive", 
                    "CommunicationPublicspeaking", 
                    "TextAnalyticsNLP",
                    "WebminingWebCrawlingDataScraping", 
                    "CloudAWSGCP", "ContainersKubernetesDocker",
                    "LinuxUbuntu", "DataEthics", "HiringCompaniesListed", "PlacementRolesListed")

for (i in 1:length(yesnovariables)){
  data[which(data[,yesnovariables[i]]== 1),yesnovariables[i]] <- "Yes" 
  data[which(data[,yesnovariables[i]]== 0),yesnovariables[i]] <- "No" 
  data[which(is.na(data[,yesnovariables[i]]== TRUE)),yesnovariables[i]] <- "Unknown"
}

data$STEMCertified <- as.factor(data$STEMCertified)

data$AvgCost <- as.numeric(rowMeans(data[,c('InstateCost', 'OutofstateCost')], na.rm=TRUE))
data$AvgCost <- ifelse(is.nan(data$AvgCost)== TRUE, NA, data$AvgCost)

data$CreditHrs <- ifelse(data$CreditHrs == "32 to 36", 34, data$CreditHrs)

data$`Placement Rate` <- as.numeric(data$`Placement Rate`)
data$AvgStartingSalary <- as.numeric(data$AvgStartingSalary)
data$CreditHrs <- as.numeric(data$CreditHrs)
data$DataEthics <- as.factor(data$DataEthics)
data$LinuxUbuntu <- as.factor(data$LinuxUbuntu)
data$ContainersKubernetesDocker <- as.factor(data$ContainersKubernetesDocker)
data$CloudAWSGCP <- as.factor(data$CloudAWSGCP)
data$WebminingWebCrawlingDataScraping <- as.factor(data$WebminingWebCrawlingDataScraping)
data$TextAnalyticsNLP <- as.factor(data$TextAnalyticsNLP)
data$CommunicationPublicspeaking <- as.factor(data$CommunicationPublicspeaking)
data$BigDataHadoopSparkHive <- as.factor(data$BigDataHadoopSparkHive)
data$DataVisualizationTableau <- as.factor(data$DataVisualizationTableau)
data$Simulation <- as.factor(data$Simulation)
data$PrescriptiveAnalyticsOptimization <- as.factor(data$PrescriptiveAnalyticsOptimization)
data$DeepLearning <- as.factor(data$DeepLearning)
data$MachineLearning <- as.factor(data$MachineLearning)
data$PredictiveAnalytics <- as.factor(data$PredictiveAnalytics)
data$DataMiningDescriptiveAnalytics <- as.factor(data$DataMiningDescriptiveAnalytics)
data$DatabaseSQL <- as.factor(data$DatabaseSQL)
data$SAS <- as.factor(data$SAS)
data$Python <- as.factor(data$Python)
data$R <- as.factor(data$R)
data$FT <- as.factor(data$FT)
data$PT <- as.factor(data$PT)
data$Online <- as.factor(data$Online)
data$Hybrid <- as.factor(data$Hybrid)
data$RequireSOP <- as.factor(data$RequireSOP)
data$GMATGRE <- as.factor(data$GMATGRE)
data$MinGMATGRE <- as.factor(data$MinGMATGRE)
data$TOEFLIELTSPTEscores <- as.factor(data$TOEFLIELTSPTEscores)
data$NumRecLetters <- as.factor(data$NumRecLetters)
data$Interview <- as.factor(data$Interview)
data$Appfee <- as.factor(data$Appfee)
data$Scholarships <- as.factor(data$Scholarships)
data$IndustryProjects <- as.factor(data$IndustryProjects)
data$CareerFairs <- as.factor(data$CareerFairs)
data$CorpInfoSessions <- as.factor(data$CorpInfoSessions)
data$CompanySponsorsListed <- as.factor(data$CompanySponsorsListed)
data$CreditHrs <- as.numeric(data$CreditHrs)

#Delete all variables related to application, min, max, and avg duration,year began,
#Delete FT, PT, Online, Hybrid, and the 2 versions
descdata <- data
drop <- c("RequireSOP", "GMATGRE", "MinGMATGRE", "TOEFLIELTSPTEscores","NumRecLetters", "Interview", "Appfee", "Scholarships", "IndustryProjects",
          "CareerFairs", "CorpInfoSessions", "CompanySponsorsListed", 
          "MinDuration", "MaxDuration", "AvgDuration", "PlacementRolesListed", 
          "HiringCompaniesListed", "YearBegan")
descdata = descdata[,!(names(descdata) %in% drop)]


############ Data preprocessing for prediction model#########################################

#check data types

data1$University <- as.factor(data1$University)

#For the missing values in different course categories, we assume that
#"NA" means that universities don't provide this kind of course and
#that's why they don't disclose this information on their websites. 
#Hence, we fill in "0" in all the "NA" missing values.

for (i in 47:65){ 
  data1[i][is.na(data1[i])]<-"0"
}


#According to DataQualityReport, we delete the columns that have 
#more than 50 % missing values.
data1$AvgScholarshipAmt. <- NULL 
data1$DepositCost <-NULL
data1$PctInternational <- NULL 
data1$AvgTOEFL <- NULL        
data1$StudentGMATGRE  <- NULL  
data1$Pctwomen  <- NULL
data1$StudentWorkExpMonths <- NULL          
data1$StudentAge  <- NULL 
data1$StudentGPA  <- NULL 
data1$AppFeeCost <- NULL 


#For the missing values in Placement Rate, AvgStartingSalary and others, 
#we use mice function to predict the missing values.


imputedValues <- mice(data=data1,seed=123, method="cart",m=1,maxit = 1)
data1 <- mice::complete(imputedValues,1)


############ AvgStartingSalary Prediction Model###########

#set target variable and independent variables
data_sal <- data1[,c(59,38:56)]
names(data_sal)[1]<-"y"


nzv <- nearZeroVar(data_sal, saveMetrics = TRUE)
data_sal<- data_sal[,c(TRUE,!nzv$zeroVar[2:ncol(data_sal)])]

##Data Partrition
inTrain <- createDataPartition(y = data_sal$y,   
                               p = .90,  
                               list = F)
train <- data_sal[inTrain,]  
test <- data_sal[-inTrain,]

ctrl <- trainControl(method="cv",     
                     number=5,        
                     classProbs = F,  
                     summaryFunction = defaultSummary, 
                     allowParallel=T)

m1 <- train(y ~ .,             
            data = train,        
            method = "lm",    
            trControl = ctrl,   
            metric = "RMSE"       
)

preds_sal <-predict(m1,newdata =data_sal)

############ PlacementRate Prediction Model###########

data_rate <- data1[,c(60,38:56)]
names(data_rate)[1]<-"y"

nzv <- nearZeroVar(data_rate, saveMetrics = TRUE)
data_rate<- data_rate[,c(TRUE,!nzv$zeroVar[2:ncol(data_rate)])]

##Data Partrition
inTrain_rate <- createDataPartition(y = data_rate$y,   
                                    p = .90,  
                                    list = F)
train_rate <- data_rate[inTrain_rate,]  
test_rate <- data_rate[-inTrain_rate,]

ctrl <- trainControl(method="cv",     
                     number=5,        
                     classProbs = F,  
                     summaryFunction = defaultSummary, 
                     allowParallel=T)

m2 <- train(y ~ .,             
            data = train_rate,        
            method = "lm",    
            trControl = ctrl,   
            metric = "RMSE"       
)


preds_rate <-predict(m2,newdata =data_rate)

###########Preparing for shiny app###################################

prediction<-as.data.frame(cbind(as.character(data1$University),data1$AvgStartingSalary,preds_sal,data1$PlacementRate,preds_rate))
names(prediction) <-c("University","ActualStartingSalary","PredictedStartingSalary",
                      "ActualPlacementRate","PredictedPlacemetnRate")

prediction$ActualStartingSalary <- as.numeric(prediction$ActualStartingSalary)
prediction$PredictedStartingSalary <-as.numeric(prediction$PredictedStartingSalary)
prediction$ActualPlacementRate <-as.numeric(prediction$ActualPlacementRate)
prediction$PredictedPlacemetnRate <-as.numeric(prediction$PredictedPlacemetnRate)


pred_course <- cbind(prediction,data1[,38:56])
pred_course$University <- as.factor(pred_course$University)

for(i in 6:24){
  pred_course[i]<-as.numeric(unlist(pred_course[i]))
}

top10_sal <- prediction %>%
  select(University,PredictedStartingSalary) %>%
  arrange(desc(PredictedStartingSalary))
top10_sal <-top10_sal[1:10,]

top10_rate <- prediction %>%
  select(University,PredictedPlacemetnRate) %>%
  arrange(desc(PredictedPlacemetnRate))
top10_rate <-top10_rate[1:10,]

totalcourse <- as.data.frame(colSums(pred_course[,6:24]))
colnames(totalcourse)[1] <-"numbers"
totalcourse$rate<-totalcourse$numbers/191
totalcourse <- cbind(as.data.frame(colnames(pred_course[6:24])),totalcourse$numbers,totalcourse$rate)
colnames(totalcourse) <- c("course","numbers","rate")

##################Shiny App#####################
ui <- fluidPage(
  navbarPage("MS Analytics Programs", theme = shinytheme("darkly"),
             tabPanel("Program Finder", 
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Desired Program Characteristics"),
                          sliderInput(inputId = "tuition", label = "Tuition",
                                      min = 0, max = 120000,
                                      value = c(30000, 50000)),
                          selectInput("region", "Region",
                                      choices = c("Does not matter", levels(descdata$Region))),
                          selectInput(inputId = "FT",
                                      label = "Are you looking for a Full Time program (vs. Online, Part-Time, or Hybrid)?",
                                      choices = c("Does not matter", levels(descdata$FullTime)),
                                      width = "220px"),
                          selectInput(inputId = "stem",
                                      label = "Are you looking for STEM Certified programs?",
                                      choices = c("Does not matter", levels(descdata$STEMCertified)),
                                      width = "220px"),
                          selectInput(inputId = "special",
                                      label = "What Program Specialty are you looking for?",
                                      choices = c("Does not matter", levels(descdata$ProgramSpecialty))),
                          selectInput(inputId = "schooltype",
                                      label = "What School are you looking to be in?",
                                      choices = c("Does not matter", levels(descdata$SchoolType))),
                          downloadButton("download_data1")
                        ), 
                        mainPanel(DT::dataTableOutput("table1", width = 900)) 
                      )),
             tabPanel("Statistics",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "test",
                                      label = "What variable do you want to use?",
                                      choices = c("AvgCost", "InstateCost", "OutofstateCost", "%ComingInWithGradDegrees", 
                                                  "Placement Rate", "AvgStartingSalary", 
                                                  "CreditHrs", "DurationMonths"))
                        ),
                        mainPanel(verbatimTextOutput("stat"))
                      )),
             tabPanel("Predicting Salary",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "school_1",
                                      label = "Select School",
                                      choices = levels(data$University),
                                      width = "220px"),
                          actionButton("predict_1", "Predict", icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        
                        mainPanel(verbatimTextOutput('actualvalue1'),
                                  verbatimTextOutput('predictedvalue1'),
                                  plotOutput("salaryplot"),
                                  plotOutput("course_2"),
                        )
                      )
             ),
             tabPanel("Predicting Placement Rate",                 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "school_2",
                                      label = "Select School",
                                      choices = levels(data$University),
                                      width = "220px"), 
                          actionButton("predict_2", "Predict", icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        mainPanel(verbatimTextOutput('actualvalue2'),
                                  verbatimTextOutput('predictedvalue2'),
                                  plotOutput("placementrateplot"),
                                  plotOutput("course_4")
                        )  
                      )
                      
                      
             )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    descdata <- subset(
      descdata,
      AvgCost >= input$tuition[1] & AvgCost <= input$tuition[2]
    )
    if (input$region != "Does not matter") {
      descdata <- subset(
        descdata,
        Region == input$region
      )
    }
    if (input$FT != "Does not matter") {
      descdata <- subset(
        descdata,
        FullTime == input$FT
      )
    }
    if (input$stem != "Does not matter") {
      descdata <- subset(
        descdata,
        STEMCertified == input$stem
      )
    }
    if (input$special != "Does not matter") {
      descdata <- subset(
        descdata,
        ProgramSpecialty == input$special
      )
    }
    if (input$schooltype != "Does not matter") {
      descdata <- subset(
        descdata,
        SchoolType == input$schooltype
      )
    }
    descdata
  })
  
  
  filtered_data2 <- reactive({
    data <- subset(
      data,
      AvgCost >= input$tuition[1] & AvgCost <= input$tuition[2]
    )
    if (input$region != "Does not matter") {
      data <- subset(
        data,
        Region == input$region
      )
    }
    if (input$FT != "Does not matter") {
      data <- subset(
        data,
        FullTime == input$FT
      )
    }
    if (input$stem != "Does not matter") {
      data <- subset(
        data,
        STEMCertified == input$stem
      )
    }
    if (input$special != "Does not matter") {
      data <- subset(
        data,
        ProgramSpecialty == input$special
      )
    }
    if (input$schooltype != "Does not matter") {
      descdata <- subset(
        descdata,
        SchoolType == input$schooltype
      )
    }
    data
  })
  
  
  
  # Replace the renderTable() with DT's version
  output$table1 <- DT::renderDataTable(
    descdata <- filtered_data(),
    options = list(scrollX = TRUE)
  )
  
  output$download_data1 <- downloadHandler(
    filename = "Programs.csv",
    content = function(file) {
      data <- filtered_data2()
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$stat <- renderPrint(
    summary(descdata[,input$test], na.rm = TRUE)
  )
  
  prediction_salary<- eventReactive(input$predict_1, {
    subset(prediction, University == input$school_1 ,  c("ActualStartingSalary", "PredictedStartingSalary"))
  })
  
  output$actualvalue1 <- renderText({
    prediction_sal <- prediction_salary()
    m1 <- prediction_sal$ActualStartingSalary
    m2 <- ifelse(m1>mean(prediction$ActualStartingSalary),
                 "Your Actual Starting Salary is higher than Actual Average Starging Salary",
                 "Your Actual Starting Salary is lower than Actual Average Starging Salary")
    print(paste("The Actaul Starting Salary is :",m1,"\n",m2))
    
  })
  
  output$predictedvalue1 <- renderText({
    prediction_sal <- prediction_salary()
    n1 <- prediction_sal$PredictedStartingSalary
    n2 <-ifelse(n1>mean(prediction$PredictedStartingSalary),
                "Your Predicted Starting Salary is higher than Predicted Average Starging Salary",
                "Your Predicted Starting Salary is lower than Predicted Average Starging Salary")
    print(paste("The Predicted Starting Salary is :",n1,"\n",n2))
    
  })
  
  
  output$salaryplot <- renderPlot({
    ggplot(top10_sal,aes(x=reorder(University,PredictedStartingSalary),y=PredictedStartingSalary))+
      geom_bar(stat='identity',aes(fill=University),width = .5)+
      labs(title= "Overall Top 10 school") +
      xlab("University")+
      coord_flip(ylim = c(85000,115000))
  })
  
  
  
  output$course_2 <- renderPlot({
    ggplot(totalcourse,aes(x=reorder(course,rate),y=rate))+
      geom_bar(stat='identity')+
      xlab("Course")+
      coord_flip()
    
  })
  
  
  prediction_rate <- eventReactive(input$predict_2, {
    subset(prediction, University == input$school_2 ,  c("ActualPlacementRate", "PredictedPlacemetnRate"))
  })
  
  
  output$actualvalue2 <- renderText({
    prediction_rate <- prediction_rate()
    o1<-prediction_rate$ActualPlacementRate
    o2<- ifelse(o1>mean(prediction$ActualPlacementRate),
                "Your Actual Placement Rate is higher than Actual Average Placement Rate",
                "Your Actual Placement Rate is lower than Actual Average Placement Rate")
    print(paste("The Actaul Placement rate is :",round(o1,2),"\n",o2))
    
  })
  
  
  output$predictedvalue2 <- renderText({
    prediction_rate <- prediction_rate()
    p1<- prediction_rate$PredictedPlacemetnRate
    p2<-ifelse(p1>mean(prediction$PredictedPlacemetnRate),
               "Your Predicted Placement Rate is higher than Predicted Average Placement Rate",
               "Your Predicted Placement Rate is lower than Predicted Average Placement Rate")
    print(paste("The Prediceted Placement rate is :",round(p1,2),"\n",p2))
  })
  
  output$placementrateplot <- renderPlot({
    ggplot(top10_rate,aes(x=reorder(University,PredictedPlacemetnRate),y=PredictedPlacemetnRate))+
      geom_bar(stat='identity',aes(fill=University),width = .5)+
      labs(title= "Overall Top 10 school")+
      xlab("University")+
      coord_flip(ylim = c(0.80, 1.10))
  })
  
  output$course_4 <- renderPlot({
    ggplot(totalcourse,aes(x=reorder(course,rate),y=rate))+
      geom_bar(stat='identity')+
      xlab("Course")+
      coord_flip()
    
  })
}

shinyApp(ui, server)
