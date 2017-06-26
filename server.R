library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(knitr)
library(corrplot)
library(combinat)

shinyServer(function(input, output, session) {
  
  # which set of data is being analyzed
  d <- reactive({input$dataset})
  y <- reactive({as.numeric(input$per_plot)})

  
  observe({
  
    # select cdi data set
    if(d()==1){
      
      # download data set
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(df_cdi,file_out, row.names=FALSE)
        })
      
      # which cdi_set is chosen
      x <- reactive({as.numeric(input$cdi_set)})
      
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = cdi_choice[[x()]])
      updateSelectizeInput(session,"df_plot_y",label = "Variable(s) you want to view (up to 3 items)",
                        choices = cdi_choice[[x()]])
      updateSelectInput(session,"df_plot_x", label = "Color/Category",
                                            choices = c("",cdi_choice_x), selected = "")

      # display selected data
      select_cdi <- reactive({
        df <- df_cdi[cdi_choice[[x()]]]
        
        if(length(input$df_colChoices)>0 & all(input$df_colChoices %in% cdi_choice[[x()]])){
          df <- df_cdi[input$df_colChoices]
        }

        ### for each row, count column answers
        top_level <- df %>% unlist() %>% na.omit() %>% max()
        df_res <- df
        for (i in 0:top_level){
          df_count <- rowSums(df== i ) %>% as.data.frame()
          names(df_count) <- paste0("count_",i)
          df_res <- cbind(df_res,df_count)
        }
        
        cbind(df_cdi[cdi_basic],df_res)
      })
      output$table <- DT::renderDataTable({
        DT::datatable(select_cdi(), rownames = FALSE)
      })
      
      # download selected data
      output$download_selected <- downloadHandler(
        filename = paste("Seedling Selected",names(sets_cdi)[x()],"Data.csv"),
        content = function(file_out){

          write.csv(select_cdi(),file_out,row.names=FALSE)
        })
      
      ## plot

      output$plot <- renderPlot(
        if(all(input$df_plot_y %in% cdi_choice[[x()]]) & length(input$df_plot_y)>0 & input$df_plot_x !=""){
          df1 <- df_cdi[c(input$df_plot_x,input$df_plot_y)]
          
          ## correlation plot
          output$plot_corr <- renderPlot(
            if(dim(df1)[2]==3){
              mosaicplot(df1[,2]~df1[,3],main="",xlab=names(df1)[2],ylab=names(df1)[3],cex.axis = 1.5)
            }else if(dim(df1)[2]>3){
              combination <- combn(2:dim(df1)[2],2)
              par(mfrow=c(1,dim(combination)[2]))
              for (i in 1:dim(combination)[2]){
                cb_x <- combination[1,i]
                cb_y <- combination[2,i]
                mosaicplot(df1[,cb_x]~df1[,cb_y],main = "",xlab="",ylab="",cex.axis = 1.5)
                title(xlab=names(df1)[cb_x], ylab=names(df1)[cb_y], cex.lab=1.5)
              }
            }
          ) # end of plot_corr

          colnames(df1)[1] <- "x_axis"
          df1_long <- gather(df1,key = survey_question,value =  ans,- x_axis)
          
          plot1 <- ggplot(df1_long,na.rm = FALSE)+
            ggtitle("Distribution Plot")+
            xlab(input$df_plot_x)+
            labs(fill= "Answers") +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            facet_grid(. ~ survey_question)
          
          if (y()==1){
            plot1 + geom_bar(aes(x = x_axis,fill=as.factor(ans)),width=0.9)
           }else{
            plot1 + geom_bar(aes(x = x_axis,fill=as.factor(ans)),position = "fill",width=0.9)+ ylab("percentage")
           }
          }
      ) # end of rederPlot
    } # end of d()==1
    
    
    # select motor data set
    else if(d()==2) {
      
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = motor_choice, selected = NULL)
      updateSelectizeInput(session,"df_plot_y", label = "Variable(s) you want to view (up to 3 items)",
                           choices = motor_choice)
      updateSelectInput(session,"df_plot_x", label = "Color/Category",
                                            choices = c("",motor_choice_x), selected = "")
      
      # download data set
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_MOTOR_Data.csv",
        content = function(file_out){
          write.csv(df_motor,file_out,row.names = FALSE)
        }
      )
      
      # display selected data
      select_motor <- reactive({
        df <- df_motor[motor_choice]
        
        if(length(input$df_colChoices)>0 & all(input$df_colChoices %in% motor_choice)){
          df <- df_motor[input$df_colChoices]
        }
        
        ### for each row, count column answers
        top_level <- df %>% unlist() %>% na.omit() %>% max()
        df_res <- df
        for (i in 1:top_level){
          df_count <- rowSums(df== i ) %>% as.data.frame()
          names(df_count) <- paste0("count_",i)
          df_res <- cbind(df_res,df_count)
        }
        
        cbind(df_motor[motor_basic],df_res)
      })
      
      output$table <- DT::renderDataTable({
        DT::datatable(select_motor(), rownames = FALSE)
      })
      
      # download selected data
      output$download_selected <- downloadHandler(
        filename = "Seedling_Selected_Motor_Data.csv",
        content = function(file_out){
          write.csv(select_motor(),file_out,row.names=FALSE)
      })
      
      ## plot
      output$plot <- renderPlot(
        if(input$df_plot_x != "" & length(input$df_plot_y)>0 & all(input$df_plot_y %in% motor_choice)){
          
          df2 <- df_motor[c(input$df_plot_x,input$df_plot_y)]
          
          ## correlation plot
          output$plot_corr <- renderPlot(
            if(dim(df2)[2]==3){
              mosaicplot(df2[,2]~df2[,3],main="",xlab=names(df2)[2],ylab=names(df2)[3],cex.axis = 1.5)
            }else if(dim(df2)[2]>3){
              combination <- combn(2:dim(df2)[2],2)
              par(mfrow=c(1,dim(combination)[2]))
              for (i in 1:dim(combination)[2]){
                cb_x <- combination[1,i]
                cb_y <- combination[2,i]
                mosaicplot(df2[,cb_x]~df2[,cb_y],main = "",xlab="",ylab="",cex.axis = 1.5)
                title(xlab=names(df2)[cb_x], ylab=names(df2)[cb_y], cex.lab=1.5)
              }
            }
          ) # end of plot_corr

          colnames(df2)[1] <- "x_axis"
          df2_long <- gather(df2,key = survey_question,value =  ans,- x_axis)
          
          plot2 <- ggplot(df2_long,na.rm = FALSE)+
            ggtitle("Distribution Plot")+
            xlab(input$df_plot_x)+
            labs(fill= "Answers") +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            facet_grid(. ~ survey_question)
          
          if (y()==1){
            plot2 + geom_bar(aes(x = x_axis,fill=as.factor(ans)),width=0.9)
          }else{
            plot2 + geom_bar(aes(x = x_axis,fill=as.factor(ans)),position = "fill",width=0.9)+ ylab("percentage")
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
      
      # which checkbox is chosen
      x_merge <- reactive({as.numeric(input$checkbox_merge)})
      updateSelectInput(session,"merge_colChoices", label = "Select Columns for Data Table",
                  choices = unique(c(cdi_basic,motor_basic,unlist(c(cdi_choice,list(motor_choice))[x_merge()]))))
   
      # display selected data
      select_merge <- reactive({
        df <- df_merge[input$merge_colChoices]
        
        if(dim(df)[2]==0){
          df_merge[unique(c(cdi_basic,motor_basic,unlist(c(cdi_choice,list(motor_choice))[x_merge()])))]
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
        

        observe({
          
          if(length(input$plot_var)>0){

            if(any(!is.na(df_plot()[,-1]))){
            
            max_var <- sapply(df_plot()[,-1],function(x) max(na.omit(x))) %>% unlist()  %>% max()
            df1 <- df_plot() %>%
              group_by_(names(df_plot())[1], names(df_plot())[2]) %>%
              summarise (n=n()) %>%
              mutate(rel.freq = n/sum(n)) %>%
              filter_(paste(names(df_plot())[2],"==",max_var))
            df1 <- df1[,-2]
            colnames(df1)[3] <- paste0("per_",names(df_plot())[2])
            colnames(df1)[2] <- paste0("n_",names(df_plot())[2])
            
            if (length(input$plot_var)>1){
              for (i in 3:dim(df_plot())[2]){
                df2 <- df_plot() %>%
                  group_by_(names(df_plot())[1], names(df_plot())[i]) %>%
                  summarise (n=n()) %>%
                  mutate(rel.freq = n/sum(n)) %>%
                  filter_(paste(names(df_plot())[i],"==",max_var))
                df2 <- df2[,-2]
                colnames(df2)[3] <- paste0("per_",names(df_plot())[i])
                colnames(df2)[2] <- paste0("n_",names(df_plot())[i])
                df1 = merge(df1,df2,key = names(df_plot())[1], all = TRUE)
              }
            } # end of 'if (length(input$plot_var)>1)'
            
            col_melt_per <- grep("^per_",names(df1))
            df_long0 <- melt(df1[,c(1,col_melt_per)], id=names(df_plot())[1])
            df_long0$variable <- as.character(df_long0$variable)
            df_long0$variable <- str_match(df_long0$variable,"^per_(.+)")[,2]
            colnames(df_long0)[3] <- "percentage"
            
            col_melt_n <- grep("^n_",names(df1))
            df_long1 <- melt(df1[,c(1,col_melt_n)], id=names(df_plot())[1])
            df_long1$variable <- as.character(df_long1$variable)
            df_long1$variable <- str_match(df_long1$variable,"^n_(.+)")[,2]
            colnames(df_long1)[3] <- "number"
            
            
            
            df_long <- merge(df_long0,df_long1,  key = c(names(df_plot())[1], "variable"), all = TRUE)
            
            output$plot <- renderPlot({
              ggplot(data = df_long, aes(x = df_long[,1], y = percentage, color = variable),na.rm = TRUE) +
                geom_point(alpha=0.5) +
                geom_smooth(method = "loess", se = FALSE) +
                ylab("percentage")+
                xlab(names(df_plot())[1])+
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"))
            }) # end of render plot

            ###################### try click, start########

            output$click_info <- renderPrint({
              print(nearPoints(df_long, input$plot1_click, addDist = FALSE, xvar = names(df_long)[1], yvar = "percentage"), row.names = F)
            })
            ###################### try click, stop ########


            }   # end of 'if(length(plot_var)>0)'
          } # end of 'if (length(input$plot_var)>0)'
        })# # end of observe3(plot)   
        
      }) # observe 2(plot)

      
    }   # end of d()=3
  })    # end of observe 1
    
})