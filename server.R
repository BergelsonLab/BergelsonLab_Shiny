library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})
  y <- reactive({as.numeric(input$per_plot)})
  y2 <- reactive({as.numeric(input$per_plot2)})

  
  observe({
  
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
          cbind(df_cdi[cdi_basic],df)
        }
      })
      output$table <- DT::renderDataTable({
        DT::datatable(select_cdi(), rownames = FALSE)
      })
      
      # download selected data
      output$download_selected_cdi <- downloadHandler(
        filename = paste("Seedling Selected",names(radio_cdi)[x()],"Data.csv"),
        content = function(file_out){

          write.csv(select_cdi(),file_out,row.names=FALSE)
        })
      
      ## plot

      output$plot <- renderPlot(
        if(input$cdi_plot_x != "" & input$cdi_plot_y !=""){
          df1 <- df_cdi[c(input$cdi_plot_x,input$cdi_plot_y)]
          plot1 <- ggplot(df1,na.rm = FALSE)+
            ggtitle(paste("The Distribution of",input$cdi_plot_y))+
            xlab(input$cdi_plot_x)+
            labs(fill=input$cdi_plot_y) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          if (y()==1){
            plot1 + geom_bar(aes(x = df1[,1],fill=as.factor(df1[,2])),width=0.9)
           }else{
            plot1 + geom_bar(aes(x = df1[,1],fill=as.factor(df1[,2])),position = "fill",width=0.9)+ ylab("percentage")
           }
          }
      ) # end of rederPlot
    } # end of d()==1
    
    
    # select motor data set
    else if(d()==2) {
      
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
          df_motor
        }else{
          cbind(df_motor[motor_basic],df)
        }
      })
      
      output$table <- DT::renderDataTable({
        DT::datatable(select_motor(), rownames = FALSE)
      })
      
      # download selected data
      output$download_selected_motor <- downloadHandler(
        filename = "Seedling_Selected_Motor_Data.csv",
        content = function(file_out){
          write.csv(select_motor(),file_out,row.names=FALSE)
      })
      
      ## plot
      output$plot <- renderPlot(
        if(input$motor_plot_x != "" & input$motor_plot_y !=""){
          df2 <- df_motor[c(input$motor_plot_x,input$motor_plot_y)]
          plot2 <- ggplot(df2,na.rm = FALSE)+
            ggtitle(paste("The Distribution of",input$motor_plot_y))+
            xlab(input$motor_plot_x)+
            labs(fill=input$motor_plot_y) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          if (y2()==1){
            plot2 + geom_bar(aes(x = df2[,1],fill=as.factor(df2[,2])),width=0.9)
          }else{
            plot2 + geom_bar(aes(x = df2[,1],fill=as.factor(df2[,2])),position = "fill",width=0.9)+ ylab("percentage")
          }
        }
      )     # end of rederPlot
    }       # end of d()=2
        
    # merged data set
    else{
      # download data set
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_MOTOR_Data.csv",
        content = function(file_out){
          write.csv(df_merge,file_out,row.names = FALSE)
        })
      
      # which radioButton is chosen
      x_merge <- reactive({as.numeric(input$radioButton_merge)})
      updateSelectInput(session,"merge_colChoices", label = "Select Columns for Data Table",
                  choices = unique(c(cdi_basic,motor_basic,sort(c(cdi_choice[[x_merge()]],motor_choice)))))
   
      # display selected data
      select_merge <- reactive({
        df <- df_merge[input$merge_colChoices]
        
        if(dim(df)[2]==0){
          df_merge[unique(c(cdi_basic,motor_basic,sort(c(cdi_choice[[x_merge()]],motor_choice))))]
        }else{
          df
        }
      })
      
      observe({
        
        updateSelectInput(session, "merge_filter", label = "Variable to Filter",
                          choices = c("",names(select_if(select_merge(),is.numeric))),selected = "")
        
        observe({
          f_range <- reactive({
            if (input$merge_filter==""){
              c(0,0)
            }else{
              f_value <- unlist(df_merge[input$merge_filter])
              range(na.omit(as.numeric(as.character(f_value))))
            }
          })
          updateSliderInput(session,"merge_range","Filter Selected Data",value=f_range(),min=f_range()[1],max =f_range()[2],step = 1)
          
          df_filter <- reactive({
            if(input$merge_filter==""){
              select_merge()
            }else{
              select_merge() %>%
                filter_(paste(input$merge_filter,">=",input$merge_range[1],"&",input$merge_filter,"<=",input$merge_range[2]))
            }
          })
            
            output$table <- DT::renderDataTable({
              # display table
              DT::datatable(df_filter(), rownames = FALSE)
            }) # end of table
            
            # download selected&filted data
            output$download_selected_merge <- downloadHandler(
              filename = "Seedling_Selected_and_Filtered_Data.csv",
              content = function(file_out){
                write.csv(df_filter(),file_out,row.names=FALSE)
              })
          }) # end of observe 3
      })   # end of observe 2
      
      ## plot
      x_plot <- reactive({as.numeric(input$plot_sec)})
      updateSelectInput(session,"plot_var", label = "Variable(s) you wish to view in the plot",
                        choices = merge_choice[[x_plot()]])
      
      observe({
        plot_var_range <- reactive({
          var_value <- unlist(df_merge[input$plot_filter])
          range(na.omit(as.numeric(as.character(var_value))))
        })
        
        updateSliderInput(session,"plot_range","Filter Selected Data",value=plot_var_range(),min=plot_var_range()[1],max =plot_var_range()[2],step = 1)
        
        df_plot <- reactive({
          if(x_plot()<=6){
            df_res <- df_merge[c(input$plot_filter,"AgeMonthCDI_Corrected",input$plot_var)] %>%
              filter_(paste(input$plot_filter,">=",input$plot_range[1],"&",input$plot_filter,"<=",input$plot_range[2]))
          }else{
            df_res <-df_merge[c(input$plot_filter,"AgeMonthMotor_Corrected",input$plot_var)]%>%
              filter_(paste(input$plot_filter,">=",input$plot_range[1],"&",input$plot_filter,"<=",input$plot_range[2]))
          }
          return(df_res[,-1])
        })
        

        output$plot <- renderPlot({
          if(length(input$plot_var)>0){
            max_var <- sapply(df_plot()[,-1],function(x) max(na.omit(x))) %>% unlist() %>% max()
            df1 <- df_plot() %>%
              group_by_(names(df_plot())[1], names(df_plot())[2]) %>%
              summarise (n=n()) %>%
              mutate(rel.freq = n/sum(n)) %>%
              filter_(paste(names(df_plot())[2],"==",max_var))
            df1 <- df1[,c(1,4)]
            colnames(df1)[2] <- names(df_plot())[2]
            
            if (length(input$plot_var)>1){
              for (i in 3:dim(df_plot())[2]){
                df2 <- df_plot() %>%
                  group_by_(names(df_plot())[1], names(df_plot())[i]) %>%
                  summarise (n=n()) %>%
                  mutate(rel.freq = n/sum(n)) %>%
                  filter_(paste(names(df_plot())[i],"==",max_var))
                df2 <- df2[,c(1,4)]
                colnames(df2)[2] <- names(df_plot())[i]
                df1 = merge(df1,df2,key = names(df_plot())[1], all = TRUE)
              }
            } # end of 'if (length(input$plot_var)>1)'
            
            df_long <- melt(df1, id=names(df_plot())[1])

            ggplot(data = df_long, aes(x = df_long[,1], y = value, color = variable),na.rm = TRUE) +
              geom_point(alpha=0.5) +
              geom_smooth(method = "loess", se = FALSE) +
              ylab("percentage")+
              xlab(names(df_plot())[1])+
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
            
          } # end of 'if (length(input$plot_var)>0)'
        })# # end of rederPlot
        
      }) # observe 2(plot)

      
    }   # end of d()=3
  })    # end of observe 1
    
})