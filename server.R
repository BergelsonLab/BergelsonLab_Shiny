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
          write.csv(df_cdi,file_out)
        }
      )
      
      # which radioButton is chosen
      x <- reactive({as.numeric(input$radioButton)})
      updateSelectInput(session,"cdi_colChoices",label = "Select Columns for Data Table: ",
                        choices = cdi_choice[[x()]])
      
      # #download filtered data
      
      # col_select <- reactive({
      #   cdi_choice[[x()]][input$cdi_colChoices] %>% as.character()
      # })
      # if(length(col_select) == 0){
        filter_cdi <- df_cdi[c(cdi_basic,names(cdi_choice[[x()]]))]
      # }else{
      #   filter_cdi <- df_cdi[,1]
      # }


      output$download_filter_cdi <- downloadHandler(
        filename = paste("Seedling Filtered",names(radio_cdi)[x()],"Data.csv"),
        content = function(file_out){
          write.csv(filter_cdi,file_out)
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