#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)
library(e1071)


#install.packages('ggvis')\

setwd("C:\\Users\\conne\\Desktop\\KKBoxShinyApp\\KKBox2")

load('database')
load('Naive_Bayes_Model')


  ui = tagList(
    tags$head(
      tags$style(HTML("body{background: linear-gradient(to bottom, rgba(255,255,255,0.6) 100%,rgba(255,255,255,0.6) 0%), url(music-wall.jpg) repeat 0 0;
                      
                      }"))),
    navbarPage(
       theme = "cerulean",  # <--- To use a theme, uncomment this
      "KKBox Churn Prediction",
      tabPanel("Explore Historical Trends",style="font-size:20px",
                  sidebarPanel(
                    wellPanel(
                    sliderInput("ageInput", "Age", 0, 100, c(25, 40)),
                    br(),br(),
                    sliderInput("amountPaid", "Average Amount Paid", 0, 2000, c(25, 40), pre = "$"),
                    br(),br(),
                    sliderInput("uniquesongs", "Number of Unique Songs Played", 0, 1500, c(25, 40))
                  # radioButtons("registeredVia", "Product type",
                   #              choices = c("1", "2", "3", "4"),
                  #               selected = "1"),
                   # selectInput("isCancel", "Country",
                  #              choices = c("1", "2", "3"))
                  )),
                  mainPanel(
                    plotOutput("coolplot"),
                    br(),br(),
                    plotOutput("coolplot2")
                    #ggvisOutput("plot1")
                  )
               
      ),
      tabPanel("Predict Churn",
               
               sidebarPanel(
                 h4("Predict probablity of subscriber churn !!"),
                 h4("Enter the details of the subscriber, and hit Predict button"),
                 br(),br(),br(),
                 img(src='kkbox_logo.jpg', align = "center",width='350',height='200')
                 
                 
                 ,br(),br(),br(),wellPanel(
                   actionButton("go", "Predict!", 
                                style="display:inline-block;width:100%;text-align: center; font-size: 20px"),
                   br(),br(),
                   h2("Churn Probablity", style="display:inline-block;width:100%;text-align: center;font-size: 20px"),
                   
                   tags$style(type='text/css', '#value {color: blue;font-size: 20px;text-align: center}'), 
                   
                   verbatimTextOutput("value"))
                 # sliderInput("amountPaid",
                 #             h2("Amount Paid by Subscriber",
                 #                style = "font-family:Courier;text-align:center"),
                 #             value=50,min=1,max=400,step=10)
                 # 
                 
               ),br(),br(),
               mainPanel(
                 fluidRow(column(4, style="border: dotted 2px black;background-color:white",  
                                
                          
                                 sliderInput("ageInput",
                                             h2("Age of Subscriber",style = "font-family:Antiqua;text-align:center;color:black"),
                                             
                                             value=25,min=0,max=100,step=1))
                                 
                          ,
                          
                          
                          column(4,offset=2,style="border: dotted 2px black;background-color:white",sliderInput("number_of_unique",
                              h2("Number of Unique Songs",
                              style = "font-family:Antiqua;text-align:center;color:black"),
                              value=50,min=1,max=500,step=20))),br(),br(),
                    
                fluidRow(column(4,offset=3,style="border: dotted 2px black;background-color:white",sliderInput("amountPaid",
                              h2("Amount Paid by Subscriber",
                              style = "font-family:Antiqua;text-align:center;color:black"),
                              value=50,min=1,max=400,step=10)
                          )),br(),
                  
                  
                 
                 
                 fluidRow(
                   column(4,style="border: dotted 2px black;background-color:white", 
                                 
                                          
                          selectizeInput("number_of_times_canceled",
                                         h2("Cancel Opted", 
                                            style = "font-family:Antiqua;text-align:center;color:black"),
                                         c("1", "2", "3", "4",
                                           "5", "6", "7","8","9","10"),
                                         "1",
                                         multiple = FALSE))
                                           
                                 ,
                         
                          column(width = 4,style="border: dotted 2px black;background-color:white", offset = 2, 
                                    selectizeInput("registeredVia",
                                                          h2("Registered Via", 
                                                             style = "font-family:Antiqua;text-align:center"),
                                                          c("1", "2", "3", "4",
                                                            "5", "6", "7"),
                                                          "1",
                                                          multiple = FALSE)
                                  
                                 )
                          ),br(),br()
                          # ,wellPanel(
                          #           actionButton("go", "Predict!", 
                          #                        style="display:inline-block;width:100%;text-align: center; font-size: 20px"),
                          #           br(),br(),
                          #           h2("Churn Chances", style="display:inline-block;width:100%;text-align: center;font-size: 20px"),
                          #          
                          #           tags$style(type='text/css', '#value {color: blue;font-size: 20px;text-align: center}'), 
                          #           
                          #           verbatimTextOutput("value"))
                )
               )
    ))
  
  
  
  server = function(input, output) {
    
    output$coolplot <- renderPlot({
      filtered <-
        database %>%
        filter(bd <= input$ageInput[1],
               actual_amount_paid <= input$amountPaid[2],
               actual_amount_paid >= input$amountPaid[1]
            
        )
     g<- ggplot(filtered, aes(actual_amount_paid))
       g+ geom_density(aes(fill=is_churn), alpha=0.8) + 
        labs(title="Density plot", 
             subtitle="Average amount paid Grouped by Churn",
             x="Average amount paid",
             fill="# Churn")
    })
    
    
    output$coolplot2 <- renderPlot({
      data <-
        database %>%
        filter(bd <= input$ageInput[1],
               actual_amount_paid <= input$amountPaid[2],
                num_unq <=input$uniquesongs[2]
        )
      
      ggplot(data, aes(x=actual_amount_paid, y=num_unq)) + 
        geom_point(aes(col=is_churn)) + 
        
        labs(subtitle="Average Amount Paid Vs Number of Unique Songs", 
             y="Number of Unique Songs", 
             x="Average Amount Paid", 
             title="Scatterplot")
 #     ggplot(data, aes(num_unq)) +
#        geom_histogram()
    })
    
    output$value <- renderText({
      if (input$go > 0){
        pred <- predict(Naive_Bayes_Model,
                        newdata = data.frame(num_unq=as.numeric(input$number_of_unique),
                                             bd=input$ageInput,
                                             is_cancel=input$number_of_times_canceled,
                                             actual_amount_paid=input$amountPaid,
                                             registered_via=input$registeredVia
                        ), type = "raw"
        )
        
        a<-pred[1]
        paste(a*100,'%')
      }}
    )
  }
# Run the application 
shinyApp(ui = ui, server = server)

