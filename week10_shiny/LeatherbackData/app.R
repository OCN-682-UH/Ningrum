#Load Libraries
library(shiny)    #for using shiny  
library(ggplot2)  #I'll use ggplot to visualize my data
library(dplyr)    #to manipulate my data
library(tidyverse)#to tidy my data
library(rsconnect)#to deploy my app 

#load data
data <- read_csv("monthlydata.csv")

#creating the User Interface (UI) ~ what your app should looks like
ui <- fluidPage(
  #Add title
  titlePanel("Leatherback Turtle Nest on Buru Island"), 
  #Add a Layout
  sidebarLayout(
    sidebarPanel(                    #using a sidebar layout
      selectInput(                   #create a select list
        inputId = "selected_year",   #the input slot that will be used to access the value
        label = "Select Year:",      #add label "Select Year"
        choices = unique(data$Year)  #the choices in select column in data in Year
      )
    ),
  #Add the main panel
    mainPanel(
      plotOutput("barPlot")          #add with a "barPlot"
    )
  )
)

#Creating the server
server <- function(input, output) {
  #setting the input by using:
  filtered_data <- reactive({             #function called "filtered_data" using reactivefunction, so the same object will be used
    data %>%                              #use data in "data" 
      filter(Year == input$selected_year) #filter it by Year, assign it with "input$selected_year"
  })
  
  #Render bar plot as an output
  output$barPlot <- renderPlot({ #render the plot
    #by using the ggplot function
    ggplot(filtered_data(),         #where ggplot using the filtered_data function in the input
           aes(x = Month,           #set the x axis as month
               y = total,           #set the y axis as total
               fill = season)) +    #add color in bar chart to represent the season
      geom_bar(stat = "identity")+  #add the geombar, using "identity" as the way it calculate
      #set the x axis label
      scale_x_continuous(           
        breaks = 1:12,              #using the number scale from 1 to 12 continuously
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") #assign the label in x axis of these name to replace name from 1 to 12
      ) +
      #Modify the axis and plot labels
      labs(
        title = paste("Turtle Nests in",    #set the title
                      input$selected_year), #where the year should be change following the input data
        x = "Month",                        #set x axis title
        y = "Total Nests",                  #set y axis title
        fill = "Season"                     #set the label of fill as "season"
      ) +
      #using theme minimal style for the barplot
      theme_minimal() 
  })
}
#launch your Shiny
shinyApp(ui = ui, server = server)