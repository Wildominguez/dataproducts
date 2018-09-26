#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Model Fitting for Optical Density Readings",
             windowTitle = "Model Fitting"),
  HTML('<hr style="color: blue;" height: 100>'),
    h4("Instructions:"),
  h4(" "),
  h5("Load a file containing your standard concentrations and corresponding optical density (OD) readings. The first column must contain the OD and the second column the concentration. You can select if your file contains or not Headers by selecting/deselecting the checkbox."), 
  h4(" "),
  h5("Once the appropriate file is loaded, a scatter plot of the data will be generated and a model (fourth order polynomial by default) will be fitted to your data. You can see the R-squared and adjusted R-squared below the graph. You can change the order of the polynomial from the dropdown menu (first to fifth)."),
  h5("Additionally, you can upload a file with OD readings to calculate concentration based on the model. This file must contain the OD readings in the first column. A scatter plot and a table with the data will be generated. You can download the calculated values from the OD readings in a csv file."),
  HTML('<hr style="color: blue;" height: 100>'),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("Input File. Must be in plain text format (*.csv), two columns: OD and concentration."),
      #upload a csv file
      fileInput("file", 
                label = "File input", 
                multiple = FALSE, 
                accept = c("text/csv", ".csv", "text/csv")),
      
      checkboxInput("header", 
                    "Check if file contains a header",
                    value = T),
      
      selectInput("order", 
                 label = "Choose polynomial order" , 
                 choices = c("First","Second","Third", "Fourth", "Fifth"),
                 selected = "Fourth"),
      
      HTML('<hr style="color: blue;" height: 100>'),
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #4b9de5;}"))
      ),
      
      h4("\nFile with unkowns. Must be in plain text format (*.csv), one column: OD."),
      #Unkowns: upload a csv file
      
      fileInput("file1", 
                label = "File input", 
                multiple = FALSE, 
                accept = c("text/csv", ".csv", "text/csv")),
      
      checkboxInput("header1", 
                    "Check if file contains a header",
                    value = T),
      HTML('<hr style="color: blue;" height: 100>'),
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #4b9de5;}"))
      ),
      
      h4("Click below to download csv file with predicted values"),
      
      downloadButton("predicted", "Download predicted values")
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("Plot"),
       hr("Model fitness"),
       verbatimTextOutput("stat", placeholder = T),
       h5("Predicted values"),
       plotlyOutput("Plot1"),
       h5("Predicted values"),
       dataTableOutput("table")
       
    )
  )
))
