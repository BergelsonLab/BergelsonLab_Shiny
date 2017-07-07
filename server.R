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
  
  # observe1
  observe({
    ######################### which dataset is chosen #########################
    x <- reactive({as.numeric(input$cdi_set)})
    x_merge <- reactive({as.numeric(input$merge_set)})
    plot_section <- reactive({as.numeric(input$plot_sec)})
    
    ############# cdi
    if(d()==1){
      df_all <- df_cdi
      set_name <- "CDI"
      basic <- cdi_basic
      df_section <- df_cdi[cdi_choice[[x()]]]
      
      ### update 
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = cdi_choice[[x()]])
      updateSelectizeInput(session,"df_plot_y",label = "Variable(s) you want to view (up to 3 items)",
                           choices = cdi_choice[[x()]])
      updateSelectInput(session,"df_plot_x", label = "Color/Category",
                        choices = cdi_choice_x)
      
    ############# motor
    }else if (d()==2){
      df_all <- df_motor
      set_name <- "Motor"
      basic <- motor_basic
      df_section <- df_motor[motor_choice]
      
      ### update
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = motor_choice, selected = NULL)
      updateSelectizeInput(session,"df_plot_y", label = "Variable(s) you want to view (up to 3 items)",
                           choices = motor_choice)
      updateSelectInput(session,"df_plot_x", label = "Color/Category",
                        choices = motor_choice_x)
    
    ############# merged data    
    }else if (d()==3){
      df_all <- df_merge
      set_name <- "Merged"
      basic <- c(cdi_basic,motor_basic) %>% unique()
      df_section <- df_merge[unlist(c(cdi_choice,list(motor_choice))[x_merge()])]
      
      ### update
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = unlist(c(cdi_choice,list(motor_choice))[x_merge()]), selected = NULL)
      updateSelectInput(session,"plot_var", label = "Variable(s) you wish to view in the plot",
                        choices = merge_choice[[plot_section()]]) 
      
#########################################3 need to fix "update'!
      # updateSelectInput(session,"plot_sec", "Question section to view", choices = c(names(sets_cdi), "Motor")[as.numeric(input$merge_set)])
    }
    
    ######################### download full data set #############################
    output$downloadData <- downloadHandler(
      filename = paste0("Seedling_Survey_Full_",set_name, "_Data.csv"),
      content = function(file_out){
        write.csv(df_all,file_out, row.names=FALSE)
      })
    
    ######################## datatable for selected variables (count their types)
    df_select <- reactive({
      df <- df_section
      
      if(length(input$df_colChoices)>0 & all(input$df_colChoices %in% names(df_section))){
        df <- df_section[input$df_colChoices]
      }
  
      df_res <- df
      ### for each row, count column answers (no count for merger data)
      if (d()!=3){
        top_level <- df %>% unlist() %>% na.omit() %>% max()
        for (i in 0:top_level){
          df_count <- rowSums(df== i ) %>% as.data.frame()
          names(df_count) <- paste0("count_",i)
          df_res <- cbind(df_res,df_count)
        }
      }
      
      cbind(df_all[basic],df_res)
    }) # end of df_select
    
    output$table <- DT::renderDataTable({
      DT::datatable(df_select(),filter = "top", rownames = FALSE)
    })

    ############################ download selected & filtered data 
    output$download_selected <- downloadHandler(
      filename = paste("Seedling Selected & Filtered",set_name,"Data.csv"),
      content = function(file_out){
        write.csv(df_select()[input$table_rows_all,],file_out,row.names=FALSE)
      })
    
    ########################### plot
    ### distribution plot for cdi & mtor
    if (d()==1|d()==2){
      # observe2
      observe({
        updateSelectInput(session,"mosaic_choice", "Filter", choices = c("all", unique(df_all[[input$df_plot_x]])), selected = "all")
        output$plot <- renderPlot(
          if(all(input$df_plot_y %in% names(df_section)) & length(input$df_plot_y)>0){
            df1 <- df_all[c(input$df_plot_x,input$df_plot_y)]
            colnames(df1)[1] <- "x_axis"
            
            if (input$mosaic_choice == "all"){
              df1_mosaic <- df1
            }else{
              df1_mosaic <- df1 %>% filter(x_axis == input$mosaic_choice)
            }
            
            ## correlation plot
            output$plot_corr <- renderPlot(
              if(dim(df1_mosaic)[2]==3){
                if (length(unique(df1_mosaic[,2]))>1 |length(unique(df1_mosaic[,3]))>1){
                  mosaicplot(df1_mosaic[,2]~df1_mosaic[,3],main="",xlab=names(df1_mosaic)[2],ylab=names(df1_mosaic)[3],cex.axis = 1.5)
                }else{
                  plot(1:10,1:10,type = "n",xaxt='n',yaxt='n',xlab = "",ylab = "")
                  text(5,3,paste(names(df1_mosaic)[2],"are all",unique(df1_mosaic[,2])),cex=1.5)
                  text(5,8,paste(names(df1_mosaic)[3],"are all",unique(df1_mosaic[,3])),cex=1.5)
                  title(xlab=names(df1)[2], ylab=names(df1)[3], cex.lab=1.5)
                }
                
              }else if(dim(df1_mosaic)[2]>3){
                combination <- combn(2:dim(df1_mosaic)[2],2)
                par(mfrow=c(1,dim(combination)[2]))
                for (i in 1:dim(combination)[2]){
                  cb_x <- combination[1,i]
                  cb_y <- combination[2,i]
                  if (length(unique(df1_mosaic[,cb_x]))>1 |length(unique(df1_mosaic[,cb_y]))>1){
                    mosaicplot(df1_mosaic[,cb_x]~df1_mosaic[,cb_y],main = "",xlab="",ylab="",cex.axis = 1.5)
                  }else{
                    plot(1:10,1:10,type = "n",xaxt='n',yaxt='n',xlab = "",ylab = "")
                    text(5,3,paste(names(df1_mosaic)[cb_x],"are all",unique(df1_mosaic[,cb_x])),cex=1.5)
                    text(5,8,paste(names(df1_mosaic)[cb_y],"are all",unique(df1_mosaic[,cb_y])),cex=1.5)
                  }
                  
                  title(xlab=names(df1_mosaic)[cb_x], ylab=names(df1_mosaic)[cb_y], cex.lab=1.5)
                }
              }
            ) # end of plot_corr
            
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
          }else{
            output$plot_corr <- renderPlot(
              return(NULL)
            )
          }
        ) # end of renderPlot
      }) # end of observe2: mosaic plot
      
      # end of "d()=1|d()=2" for plot
    ### facet plot for merged data
    }else if (d()==3){
      # observe3
      observe({
        output$plot <- renderPlot(
          if(length(input$plot_var)>0 & input$plot_facet_v == "No"){
            df_plot <- df_all[c(input$plot_facet,input$plot_var)]
            colnames(df_plot)[1] <- "plot_facet"
            df_plot_long <- gather(df_plot,key = survey_question,value =  ans,- plot_facet)
            df_plot_long$plot_facet <- df_plot_long$plot_facet %>% as.factor()
            levels(df_plot_long$plot_facet) <- paste(input$plot_facet,"=",levels(df_plot_long$plot_facet))
            
            ggplot(df_plot_long,na.rm = FALSE)+
              geom_bar(aes(x = survey_question,fill=as.factor(ans)),width=0.9)+
              ggtitle("Comparison Plot")+
              labs(fill= "Answers") +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1))+
              facet_grid(. ~ plot_facet)
            
            
          }else if (length(input$plot_var)>0 & input$plot_facet_v != "No"){
            df_plot <- df_all[c(input$plot_facet,input$plot_facet_v, input$plot_var)]
            colnames(df_plot)[c(1,2)] <- c("plot_facet_h","plot_facet_v")
            df_plot_long <- gather(df_plot,key = survey_question,value =  ans,-c(plot_facet_h,plot_facet_v))
            
            df_plot_long$plot_facet_h <- df_plot_long$plot_facet_h %>% as.factor()
            levels(df_plot_long$plot_facet_h) <- paste(input$plot_facet,"=",levels(df_plot_long$plot_facet_h))
            
            df_plot_long$plot_facet_v <- df_plot_long$plot_facet_v %>% as.factor()
            levels(df_plot_long$plot_facet_v) <- paste(input$plot_facet_v,"=",levels(df_plot_long$plot_facet_v))
            
            ggplot(df_plot_long,na.rm = FALSE)+
              geom_bar(aes(x = survey_question,fill=as.factor(ans)),width=0.9)+
              ggtitle("Comparison Plot")+
              labs(fill= "Answers") +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1))+
              facet_grid(plot_facet_v ~ plot_facet_h)
          } # end of 'if (length(input$plot_var)>0)'
        )# # end of output$plot  
      }) # observe3: facet plot
      
    } # end of "d()==3" for plot

    
    
  }) # end of observe1: which dataset is chosen
  

  
}) # end of shinyServer