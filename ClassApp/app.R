library(shiny)
library(tidyverse)
library(rsconnect)


## start with the blank 

#ui <- fluidPage()
#server <- function(input, output){}
#shinyApp(ui = ui, server = server)

## now add slider
#ui <- fluidPage(
#  sliderInput(inputId = "num",
#  label = "Choose a number",
#  value = 25, min = 1, max =100)
#)

#server <- function(input, output){}
#shinyApp(ui = ui, server = server)


##now try create an output, but only static. because using rnorm 100
#ui <- fluidPage(

#  sliderInput(inputId = "num",
#              label = "Choose a number",
#              value = 25, min = 1, max =100), 
#plotOutput("hist")
#)

#server <- function(input, output){
#  output$hist <- renderPlot({
#    data <- tibble(x = rnorm(100))
#    
#    ggplot(data, aes(x=x))+
#      geom_histogram()
#  })
#}
#shinyApp(ui = ui, server = server)

## now try create an output, with interactive histogram following the slider
#ui <- fluidPage(
#  sliderInput(inputId = "num",
#              label = "Choose a number",
#              value = 25, min = 1, max =100), 
#  plotOutput("hist")
#)

#server <- function(input, output){
#  output$hist <- renderPlot({
#    data <- tibble(x = rnorm(input$num)) #this part input$num that makes it interactive
    
#    ggplot(data, aes(x=x))+
#      geom_histogram()
#  })
#}
#shinyApp(ui = ui, server = server)


#now try adding text and stats but not connected
#ui <- fluidPage(
#  sliderInput(inputId = "num",
#              label = "Choose a number",
#              value = 25, min = 1, max =100),
#  textInput(inputId = "title",
#            label = "Choose a number", 
#            value = "Histogram of Random Normal Value"),
#   plotOutput("hist"), #creates space for a plot called hist
#  verbatimTextOutput("stats") # create a space for stats
#)

#server <- function(input, output){
#  output$hist <- renderPlot({
#    data <- tibble(x = rnorm(input$num)) #this part input$num that makes it interactive
    
#    ggplot(data, aes(x=x))+
#      geom_histogram() +
#      labs(title = input$title) #Add a new title
#  })
#  output$stats <- renderPrint({
#    summary(rnorm(input$num)) 
#  })
#}

#shinyApp(ui = ui, server = server)

#now trying adding text and stats that connected
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max =100),
  textInput(inputId = "title",
            label = "Choose a number", 
            value = "Histogram of Random Normal Value"),
  plotOutput("hist"), #creates space for a plot called hist
  verbatimTextOutput("stats") # create a space for stats
)

server <- function(input, output){
  data<-reactive({ 
    tibble(x = rnorm(input$num)) # 100 random normal points
  })
  output$hist <- renderPlot({ 
    ggplot(data(), aes(x = x))+ # make a histogram
      geom_histogram()+
      labs(title = input$title) #Add a new title
  })
  output$stats <- renderPrint({
    summary(data()) # calculate summary stats based on the numbers
  })
}
shinyApp(ui = ui, server = server)
