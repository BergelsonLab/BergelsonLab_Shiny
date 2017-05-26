library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})
  y <- reactive({as.numeric(input$per_plot)})
  y2 <- reactive({as.numeric(input$per_plot2)})

  
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

      # display selected data
      select_cdi <- reactive({
        df <- df_cdi[input$cdi_colChoices]
        
        if(dim(df)[2]==0){
          df_cdi[cdi_choice[[x()]]]
        }else{
          df
        }
      })
      output$table <- DT::renderDataTable({
        DT::datatable(select_cdi())
      })
      
      # download selected data
      output$download_selected_cdi <- downloadHandler(
        filename = paste("Seedling Selected",names(radio_cdi)[x()],"Data.csv"),
        content = function(file_out){

          write.csv(cbind(df_cdi[cdi_basic],select_cdi()),file_out,row.names=FALSE)
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
        filename = "Seedling_Survey_All_MOTOR_Data.csv",
        content = function(file_out){
          write.csv(df_motor,file_out,row.names = FALSE)
        }
      )
      
      # display selected data
      select_motor <- reactive({
        df <- df_motor[input$motor_colChoices]
        if(dim(df)[2]==0){
          df_motor[,-c(1,2)]
        }else{
          df
        }
      })
      
      output$table <- DT::renderDataTable({
        DT::datatable(select_motor())
      })
      
      # download selected data
      output$download_selected_motor <- downloadHandler(
        filename = "Seedling_Selected_Motor_Data.csv",
        content = function(file_out){
          write.csv(cbind(df_motor[motor_basic],select_motor()),file_out,row.names=FALSE)
      })
      
      ## plot
      output$plot <- renderPlot(
        if(input$motor_plot_x != "" & input$motor_plot_y !=""){
          df2 <- df_motor[c(input$motor_plot_x,input$motor_plot_y)]
          plot2 <- ggplot(df2,na.rm = TRUE)+
            ggtitle(paste("The Distribution of",input$motor_plot_y))+
            xlab(input$motor_plot_x)+
            labs(fill=input$motor_plot_y) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          if (y2()==1){
            plot2 + geom_bar(aes(x = df2[,1],fill=as.factor(df2[,2])))
          }else{
            plot2 + geom_bar(aes(x = df2[,1],fill=as.factor(df2[,2])),position = "fill")+ ylab("percentage")
          }
        }
      ) # end of rederPlot
      
    }
  }) # end of renderPlot
    
})