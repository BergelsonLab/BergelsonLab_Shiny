library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})
  # which radioButton is chosen
  x <- reactive({input$radioButton})
  
  
  
  
  output$plot<-renderPlot({
  
    # select cdi data set
    if(d()==1){
      
      # download data set
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(df_cdi,file_out)
        }
      )
      

      updateSelectInput(session,"cdi_colChoices",label = "Select Columns for Data Table: ",
                        choices = cdi_choice[[x()]])
      
      
      
    } # end of d()==1
    
    
    
    
    # select motor data set
    else{
      
      # download data set
      output$downloadData <- downloadHandler(
        filename = "SSeedling_Survey_All_MOTOR_Data.csv",
        content = function(file_out){
          write.csv(df_motor,file_out)
        }
      )
    }
  }) # end of renderPlot
    
})