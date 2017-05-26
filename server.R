library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})
  y <- reactive({as.numeric(input$per_plot)})

  
  output$plot_env<-renderPlot({
  
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
      updateSelectInput(session,"cdi_plot_y",label = "Variable you want to view",
                        choices = c("",cdi_choice[[x()]]), selected = "")

      # display filtered data
      filter_cdi <- reactive({
        df <- df_cdi[input$cdi_colChoices]
        
        if(dim(df)[2]==0){
          df_cdi[c(cdi_basic,cdi_choice[[x()]])]
        }else{
          cbind(df_cdi[cdi_basic],df)
        }
      })
      output$table <- DT::renderDataTable({
        DT::datatable(filter_cdi())
      })
      
      # download filtered data
      output$download_filter_cdi <- downloadHandler(
        filename = paste("Seedling Filtered",names(radio_cdi)[x()],"Data.csv"),
        content = function(file_out){

          write.csv(filter_cdi(),file_out,row.names=FALSE)
        })
      
      ## plot

      output$plot <- renderPlot(
        if(input$cdi_plot_x != "" & input$cdi_plot_y !=""){
          df1 <- df_cdi[c(input$cdi_plot_x,input$cdi_plot_y)]
          plot1 <- ggplot(df1,na.rm = TRUE)+
            ggtitle(paste("The Distribution of",input$cdi_plot_y))+
            xlab(input$cdi_plot_x)+
            labs(fill=input$cdi_plot_y) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          if (y()==1){
            plot1 + geom_bar(aes(x = df1[,1],fill=as.factor(df1[,2])))
           }else{
            plot1 + geom_bar(aes(x = df1[,1],fill=as.factor(df1[,2])),position = "fill")+ ylab("percentage")
           }
          }
      ) # end of rederPlot
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