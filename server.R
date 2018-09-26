#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)


shinyServer(function(input, output){
#main plot  
  output$Plot <- renderPlotly({
#Read file
        inFile <- input$file
 #       if (is.null(inFile))
#          return(NULL)
        h <- input$header
        f <- read.csv(inFile$datapath, header = h)
        if(is.null(inFile))
          f <- read.csv("example1.csv", header = TRUE)
        
        x <- (f[,1])
        y <- (f[,2])
        colnames(f) <- c("od","conc")
    
#Determine model to use
        if(is.null(inFile)){
          fit <- lm(0~0)
        }
        else{
            a <- input$order
            if (a == "First"){
              fit <- lm(y ~ x)
            }
            if (a == "Second"){
              fit <- lm(y ~ poly(x,2))
            }
            if (a == "Third"){
              fit <- lm(y ~ poly(x,3))
            }
            if (a == "Fourth"){
              fit <- lm(y ~ poly(x,4))
            }
            if (a == "Fifth"){
              fit <- lm(y ~ poly(x,5))
            }   
        }
    
        plot1 <- plot_ly(data = f,
                x = ~od,
                y = ~conc,
                type = "scatter", 
                mode = "markers")
        plot2 <- layout(plot1,title = "Concentration and Optical Density",
               xaxis = list(title = "Optical Density (OD)"),
               yaxis = list(title = "COncentration (ppm)"),
               showlegend = F)
        add_lines(plot2,
                  x = ~od,
                  y = fitted(fit))
        
      })
  
#Report the R-squared  
  output$stat <- renderPrint({
    #Read file
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    h <- input$header
    f <- read.csv(inFile$datapath, header = h)
    x <- (f[,1])
    y <- (f[,2])
    colnames(f) <- c("od","conc")
    
    #Determine model to use
      a <- input$order
      if (a == "First"){
        fit <- lm(y ~ x)
      }
      if (a == "Second"){
        fit <- lm(y ~ poly(x,2))
      }
      if (a == "Third"){
        fit <- lm(y ~ poly(x,3))
      }
      if (a == "Fourth"){
        fit <- lm(y ~ poly(x,4))
      }
      if (a == "Fifth"){
        fit <- lm(y ~ poly(x,5))
      }   
    r2 <- signif(summary(fit)$r.squared, digits = 4)
    adj.r2 <- signif(summary(fit)$adj.r.squared, digits = 4)
    
    cat("R-squared:",r2, "   Adjusted R-Squared:", adj.r2, sep = " ")
  })
  
############  Predictions
  output$Plot1 <- renderPlotly({
    #Read file
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    h <- input$header
    f <- read.csv(inFile$datapath, header = h)
    x <- (f[,1])
    y <- (f[,2])
    colnames(f) <- c("od","conc")
    
    #Determine model to use
    if(is.null(inFile)){
      fit <- lm(0~0)
    }
    else{
      a <- input$order
      if (a == "First"){
        fit <- lm(y ~ x)
      }
      if (a == "Second"){
        fit <- lm(y ~ poly(x,2))
      }
      if (a == "Third"){
        fit <- lm(y ~ poly(x,3))
      }
      if (a == "Fourth"){
        fit <- lm(y ~ poly(x,4))
      }
      if (a == "Fifth"){
        fit <- lm(y ~ poly(x,5))
      }   
    }
  
####actual predictions    
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    h1 <- input$header1
    f1 <- read.csv(inFile1$datapath, header = h1)
    colnames(f1) <- "x"
    
    predicted <- as.data.frame(predict(object = fit, 
                                       newdata = f1))

    colnames(predicted) <- "y"
    predicted$x <- f1$x
    
    plot_a <- plot_ly(data = predicted,
                      x = ~x,
                      y = ~y,
                      type = "scatter",
                      mode = "markers",
                      marker = list(size = 7, 
                                    color = 'rgba(255, 0, 0, 1)'))
    plot_b <- layout(plot_a,title = "Concentration and Optical Density",
                    xaxis = list(title = "Optical Density (OD)"),
                    yaxis = list(title = "Concentration (ppm)"),
                    showlegend = F)
    plot_b

  })

#####Generate matrix to export

  
  output$table <- renderDataTable({
    
    #Read file
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    h <- input$header
    f <- read.csv(inFile$datapath, header = h)
    x <- (f[,1])
    y <- (f[,2])
    colnames(f) <- c("od","conc")
    
    #Determine model to use
    if(is.null(inFile)){
      fit <- lm(0~0)
    }
    else{
      a <- input$order
      if (a == "First"){
        fit <- lm(y ~ x)
      }
      if (a == "Second"){
        fit <- lm(y ~ poly(x,2))
      }
      if (a == "Third"){
        fit <- lm(y ~ poly(x,3))
      }
      if (a == "Fourth"){
        fit <- lm(y ~ poly(x,4))
      }
      if (a == "Fifth"){
        fit <- lm(y ~ poly(x,5))
      }   
    }
    
    
    ####actual predictions    
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    h1 <- input$header1
    f1 <- read.csv(inFile1$datapath, header = h1)
    #    x1 <- (f1[,1])
    colnames(f1) <- "x"
    
    predicted <- as.data.frame(predict(object = fit, 
                                       newdata = f1))
    
    colnames(predicted) <- "y"
    predicted$x <- f1$x
    
    colnames(predicted) <- c("ppm", "OD")
    
    predicted
    
  })
  
  #####Reactive to obtain data for file export
  
  file_export <- reactive({
      #Read file
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      h <- input$header
      f <- read.csv(inFile$datapath, header = h)
      x <- (f[,1])
      y <- (f[,2])
      colnames(f) <- c("od","conc")
      
      #Determine model to use
      if(is.null(inFile)){
        fit <- lm(0~0)
      }
      else{
        a <- input$order
        if (a == "First"){
          fit <- lm(y ~ x)
        }
        if (a == "Second"){
          fit <- lm(y ~ poly(x,2))
        }
        if (a == "Third"){
          fit <- lm(y ~ poly(x,3))
        }
        if (a == "Fourth"){
          fit <- lm(y ~ poly(x,4))
        }
        if (a == "Fifth"){
          fit <- lm(y ~ poly(x,5))
        }   
      }
      
      
      ####actual predictions    
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      h1 <- input$header1
      f1 <- read.csv(inFile1$datapath, header = h1)
      #    x1 <- (f1[,1])
      colnames(f1) <- "x"
      
      predicted <- as.data.frame(predict(object = fit, 
                                         newdata = f1))
      
      colnames(predicted) <- "y"
      predicted$x <- f1$x
      
      colnames(predicted) <- c("ppm", "OD")
      
      predicted

    })
    

  
  ####Export file
    output$predicted <- downloadHandler(
      
      filename = function(){
        paste(Sys.Date(), 
              "_predicted.csv",
              sep = "")},
      content = function(file){
        write.csv(file_export(), file)},
      contentType = "text/csv"
      )
    
  
      
  
})
  
