library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})

  
  output$plot<-renderPlot({
  
    # select cdi data set
    if(d()==1){
      
      # download data set
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(df_cdi,file_out, row.names=FALSE)
        }
      )
      
      # which radioButton is chosen
      x <- reactive({as.numeric(input$radioButton)})
      
      updateSelectInput(session,"cdi_colChoices",label = "Select Columns for Data Table: ",
                        choices = cdi_choice[[x()]])

      # #download filtered data
      filter_cdi <- reactive({
        df <- df_cdi[input$cdi_colChoices]
        
        if(dim(df)[2]==0){
          df_cdi[c(cdi_basic,cdi_choice[[x()]])]
        }else{
          cbind(df_cdi[cdi_basic],df)
        }
      })

      output$download_filter_cdi <- downloadHandler(
        filename = paste("Seedling Filtered",names(radio_cdi)[x()],"Data.csv"),
        content = function(file_out){

          write.csv(filter_cdi(),file_out,row.names=FALSE)
        })

      
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