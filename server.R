library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(knitr)
library(corrplot)
library(combinat)

shinyServer(function(input, output, session) {
  
  # reactive expressions
  ### which dataset is selected
  d <- reactive({input$dataset})
  ### count plot or percentage plot
  y <- reactive({as.numeric(input$per_plot)})
  ### the selected CDI section if CDI dataset is selected
  x <- reactive({as.numeric(input$cdi_set)})
  ### the selected data section(s) for DataTable if Merged dataset is selected
  x_merge <- reactive({as.numeric(input$merge_set)})
  ### the selected data section for facet plot if Merged dataset is selected
  plot_section <- reactive({as.numeric(input$plot_sec)})
  
  # observe1 -- active reactive context for reactive expressions: d(),x(),x_merge(),plot_section()
  observe({
    
    ############# cdi
    if(d()==1){                                         # d(): update whenever the selected dataset changes
      df_all <- df_cdi                                  # full cdi dataset
      set_name <- "CDI"
      basic <- cdi_basic                                
      df_section <- df_cdi[cdi_choice[[x()]]]           # x(): update whenever the selected CDI section changes
      plot_level <- all_level[[x()]]                    # labels for sected section factors 
      
      ### update widgets to CDI mode 
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = cdi_choice[[x()]])
      updateSelectizeInput(session,"df_plot_y",label = "Variable(s) you want to view (up to 3 items)",
                           choices = cdi_choice[[x()]])
      updateSelectInput(session,"df_plot_x", label = "X-axis",
                        choices = dist_x)
      
      ############# motor
    }else if (d()==2){                                  # d(): update whenever the selected dataset changes
      ### update widgets to Motor mode
      df_all <- df_motor                                # full motor dataset
      set_name <- "Motor"
      basic <- motor_basic
      df_section <- df_motor[motor_choice]             
      plot_level <- all_level[[7]]                      # labels for sected section factors 
      
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = motor_choice, selected = NULL)
      updateSelectizeInput(session,"df_plot_y", label = "Variable(s) you want to view (up to 3 items)",
                           choices = motor_choice)
      updateSelectInput(session,"df_plot_x", label = "X-axis",
                        choices = dist_x)
      
      ############# merged data    
    }else if (d()==3){                                   # d(): update whenever the selected dataset changes
      df_all <- df_merge                                 # full merged dataset
      set_name <- "Merged"
      basic <- c(cdi_basic,motor_basic) %>% unique()
      df_section <- df_merge[unlist(merge_choice[x_merge()])] # x_merge(): update whenever the selected table data section(s) changes
      plot_level <- all_level[[plot_section()]]          # plot_section(): update whenever the selected plot data section changes
      
      ### update widgets to Merged data mode
      updateSelectInput(session,"df_colChoices",label = "Select Columns for Data Table:",
                        choices = merge_choice[x_merge()], selected = NULL)
      updateSelectInput(session,"plot_var", label = "Variable(s) you wish to view in the plot",
                        choices = merge_choice[[plot_section()]]) 
    }
    
    ######################### download full data set #############################
    output$downloadData <- downloadHandler(
      filename = paste0("Seedling_Survey_Full_",set_name, "_Data.csv"),
      content = function(file_out){
        write.csv(df_all,file_out, row.names=FALSE)
      })
    
    ######################## DataTable 
    # if no variables are selected, render the dataframe of basic child info variables and selected section(s);
    # if some variables are selected, render the dataframe of basic child info variables and selected variables
    # for CDI & Motor datsets, count the factor levels for each obs
    ### reactive expressions: update whenever input$df_colChoices changes
    df_select <- reactive({
      df <- df_section
      if(length(input$df_colChoices)>0 & all(input$df_colChoices %in% names(df_section))){
        df <- df_section[input$df_colChoices]
      }
      df_res <- df
      
      ### for each row, count factor levels (no count for merger data)
      if (d()!=3){                                                # d(): update whenever the selected dataset changes
        top_level <- df %>% unlist() %>% na.omit() %>% range()
        for (i in top_level[1]:top_level[2]){
          df_count <- rowSums(df== i ) %>% as.data.frame()
          names(df_count) <- paste0("count_",i)
          df_res <- cbind(df_res,df_count)
        }
      }
      
      ### combine selected variable(s)/section(s) with basic child info variables
      cbind(df_all[basic],df_res)
    }) # end of df_select
    
    ###  server-side version of DataTables: return reactive expressions "df_select()"
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
    # distribution plot for cdi & mtor
    if (d()==1|d()==2){                                  # d(): update whenever the selected dataset changes
      # observe2 -- observer for updating input "mosaic_choice"
      observe({
        ### update the mosaic plot filter choices according to the selected "X-axis" variable
        updateSelectInput(session,"mosaic_choice", "Filter by the X-axis Variable", choices = c("all", unique(df_all[[input$df_plot_x]])), selected = "all")
      })
      
      ### distribution plot
      output$plot <- renderPlot(
        if(all(input$df_plot_y %in% names(df_section)) & length(input$df_plot_y)>0){
          df1 <- df_all[c(input$df_plot_x,input$df_plot_y)]
          colnames(df1)[1] <- "x_axis"
          
          # filter plot data according to input "mosaic_choice"
          if (input$mosaic_choice == "all"){
            df1_mosaic <- df1
          }else{
            df1_mosaic <- df1 %>% filter(x_axis == input$mosaic_choice)
          }
          
          ### mosaic legend (what each number stands for)
          output$legend_mosaic <- renderText({
            filter_ind <- which(names(plot_level) %in% unlist(df1_mosaic[,-1]))
            paste(names(unlist(plot_level[filter_ind])),"=",unlist(plot_level[filter_ind]),"<br/>") 
          })
          
          ### mosaic plot
          output$plot2 <- renderPlot(
            ### if compare two variables
            if(dim(df1_mosaic)[2]==3){
              if (length(unique(df1_mosaic[,2]))>1 |length(unique(df1_mosaic[,3]))>1){
                mosaicplot(df1_mosaic[,2]~df1_mosaic[,3],main="",xlab=names(df1_mosaic)[2],ylab=names(df1_mosaic)[3],cex.axis = 1.5)
              }else{
                # if both variables are constant, plot a rectangular
                plot(1:10,1:10,type = "n",xaxt='n',yaxt='n',xlab = "",ylab = "")
                text(5,3,paste(names(df1_mosaic)[2],"are all",unique(df1_mosaic[,2])),cex=1.5)
                text(5,8,paste(names(df1_mosaic)[3],"are all",unique(df1_mosaic[,3])),cex=1.5)
                title(xlab=names(df1)[2], ylab=names(df1)[3], cex.lab=1.5)
              }
              ### if compare more than two variables (in pairs)
            }else if(dim(df1_mosaic)[2]>3){
              combination <- combn(2:dim(df1_mosaic)[2],2)
              par(mfrow=c(1,dim(combination)[2]))
              for (i in 1:dim(combination)[2]){
                cb_x <- combination[1,i]
                cb_y <- combination[2,i]
                if (length(unique(df1_mosaic[,cb_x]))>1 |length(unique(df1_mosaic[,cb_y]))>1){
                  mosaicplot(df1_mosaic[,cb_x]~df1_mosaic[,cb_y],main = "",xlab="",ylab="",cex.axis = 1.5)
                }else{
                  # if both variables are constant, plot a rectangular
                  plot(1:10,1:10,type = "n",xaxt='n',yaxt='n',xlab = "",ylab = "")
                  text(5,3,paste(names(df1_mosaic)[cb_x],"are all",unique(df1_mosaic[,cb_x])),cex=1.5)
                  text(5,8,paste(names(df1_mosaic)[cb_y],"are all",unique(df1_mosaic[,cb_y])),cex=1.5)
                }
                title(xlab=names(df1_mosaic)[cb_x], ylab=names(df1_mosaic)[cb_y], cex.lab=1.5)
              }
            }
          ) # end of plot2
          
          # long format dataframe for facet plot
          df1_long <- gather(df1,key = survey_question,value =  ans,- x_axis)
          
          # match factor levels
          df1_long$ans <- factor(df1_long$ans , levels = names(plot_level), labels = plot_level)
          
          ### distribution plot
          plot1 <- ggplot(df1_long,na.rm = FALSE)+
            ggtitle("Distribution Plot")+
            xlab(input$df_plot_x)+
            labs(fill= "Answers") +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            facet_grid(. ~ survey_question)
          
          # reactive expression: y()==1 count plot
          if (y()==1){                                          
            plot1 + geom_bar(aes(x = x_axis,fill=ans),width=0.9)
            
            # percentage plot
          }else{
            plot1 + geom_bar(aes(x = x_axis,fill=ans),position = "fill",width=0.9)+ ylab("percentage")
          }
        }else{
          output$plot2 <- renderPlot(
            return(NULL)
          )
        }
      ) # end of renderPlot
      
      # end of "d()=1|d()=2" for plot
    }else if (d()==3){                                  # d(): update whenever the selected dataset changes
      
      # the range of the variable for horizontal facet plot
      ### reactive expression: update whenever "input$plot_facet" changes
      f_range <- reactive({
        f_value <- unlist(df_merge[input$plot_facet])
        range(na.omit(as.numeric(as.character(f_value))))
      })
      
      # observe3 -- observer for updating input "collapse_range"
      observe({
        # allow users to customize the range of facet variable used for collapse plot
        updateSliderInput(session,"collapse_range",value=f_range(),min=f_range()[1],max =f_range()[2],step = 1)
      })
      
      # observe4 -- active reactive context for updating plot dataframe
      observe({
        
        # horizontal facet plot
        if(length(input$plot_var)>0 & input$plot_facet_v == "No"){
          
          # popup: plot data
          output$table_plot <- DT::renderDataTable({
            DT::datatable(df_all[c(input$plot_facet,input$plot_var)],filter = "top", rownames = FALSE)
          })
          
          # download plot data 
          output$download_popup <- downloadHandler(
            filename = paste("Seedling Plot Data.csv"),
            content = function(file_out){
              write.csv(df_all[c(input$plot_facet,input$plot_var)],file_out,row.names=FALSE)
            })
          
          
          df_plot <- df_all[c(input$plot_facet,input$plot_var)]
          colnames(df_plot)[1] <- "plot_facet"
          
          # long format for facet plot
          df_plot_long <- gather(df_plot,key = survey_question,value =  ans,- plot_facet)
          
          # update factor levels 
          ### find which data section input$plot_facet comes from
          #print(grep(sprintf("([^_]+|^)(%s)", input$plot_facet), unlist(lapply(merge_choice, function(x) paste(unlist(x),collapse=" ")))))
          plot_level_facet <- all_level[[grep(sprintf("([^_]+|^)(%s)", input$plot_facet), unlist(lapply(merge_choice, function(x) paste(unlist(x),collapse=" "))))]]
          #### match factor labels for facet variable
          df_plot_long$plot_facet <- factor(df_plot_long$plot_facet, levels = names(plot_level_facet), labels = paste(input$plot_facet,"=",plot_level_facet))
          ### match factor labels for selected x-axis variables(s)
          df_plot_long$ans <- factor(df_plot_long$ans , levels = names(plot_level), labels = plot_level)
          
          # facet plot
          output$plot <- renderPlot(
            ggplot(df_plot_long,na.rm = FALSE)+
              geom_bar(aes(x = survey_question,fill=ans),width=0.9)+
              ggtitle("Comparison Plot")+
              labs(fill= "Answers") +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1))+
              facet_grid(. ~ plot_facet)) #end of "output$plot"
          
          # horizontal & vertical facet plot  
        }else if(length(input$plot_var)>0 & input$plot_facet_v != "No"){
          ### popup table: plot data
          output$table_plot <- DT::renderDataTable({
            DT::datatable(df_all[c(input$plot_facet,input$plot_facet_v, input$plot_var)],filter = "top", rownames = FALSE)
          })
          
          df_plot <- df_all[c(input$plot_facet,input$plot_facet_v, input$plot_var)]
          colnames(df_plot)[c(1,2)] <- c("plot_facet_h","plot_facet_v")
          
          # long format dataframe for facet plot
          df_plot_long <- gather(df_plot,key = survey_question,value =  ans,-c(plot_facet_h,plot_facet_v))
          
          # update factor levels
          ### find which data section input$plot_facet comes from
          plot_level_facet <- all_level[[grep(sprintf("([^_]+|^)(%s)", input$plot_facet), unlist(lapply(merge_choice, function(x) paste(unlist(x),collapse=" "))))]]
          ### match factor labels for horizontal facet variable
          df_plot_long$plot_facet_h <- factor(df_plot_long$plot_facet_h, levels = names(plot_level_facet), labels = paste(input$plot_facet,"=",plot_level_facet))
          ### match factor labels for selected x-axis variables(s)
          df_plot_long$ans <- factor(df_plot_long$ans , levels = names(plot_level), labels = unlist(plot_level))
          ### match factor labels for vertical facet variable
          df_plot_long$plot_facet_v <- df_plot_long$plot_facet_v %>% as.factor()
          levels(df_plot_long$plot_facet_v) <- paste(input$plot_facet_v,"=",levels(df_plot_long$plot_facet_v))
          
          output$plot <- renderPlot(ggplot(df_plot_long,na.rm = FALSE)+
                                      geom_bar(aes(x = survey_question,fill= ans),width=0.9)+
                                      ggtitle("Comparison Plot")+
                                      labs(fill= "Answers") +
                                      theme(plot.title = element_text(hjust = 0.5),
                                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                            axis.text.x = element_text(angle = 30, hjust = 1))+
                                      facet_grid(plot_facet_v ~ plot_facet_h)) # end of "output$plot"
          
          # if incomplete info provided, return nothing
        }else{
          # popup table: plot data
          output$table_plot <- DT::renderDataTable({
            return(NULL)
          })
          
          # download plot data 
          output$download_popup <- downloadHandler(
            filename = paste("Seedling Plot Data.csv"),
            content = function(file_out){
              NULL
            })
          # facet plot
          output$plot <- renderPlot(
            return(NULL)
          )
        } 
        
        ### collapsed plot
        output$plot2 <- renderPlot(
          
          # horizontal collapsed facet plot
          if (length(input$plot_var)>0 & input$plot_facet_v == "No" & input$collapse_or_not == "Yes"){
            
            # reactive expression: refilter data whenever the collapsed range changes
            df_filter <- reactive({
              df_plot2 <- df_all[c(input$plot_facet,input$plot_var)]
              colnames(df_plot2)[1] <- "plot_facet"
              df_plot_long2 <- gather(df_plot2,key = survey_question,value =  ans,- plot_facet)
              df_plot_long2$plot_facet <- df_plot_long2$plot_facet %>% as.character() %>% as.numeric()
              df_plot_long2 %>%
                filter(plot_facet >= input$collapse_range[1] & plot_facet <= input$collapse_range[2])
            })
            
            # print the factor labels for horizontal facet variable
            output$legend_collapse <- renderText({
              plot_level_facet <- all_level[[grep(sprintf("([^_]+|^)(%s)", input$plot_facet), unlist(lapply(merge_choice, function(x) paste(unlist(x),collapse=" "))))]]
              filter_ind <- which(names(plot_level_facet) %in% df_filter()[,1]) # df_filter(): refilter data whenever the collapsed range changes
              paste(names(unlist(plot_level_facet[filter_ind])),"=",unlist(plot_level_facet[filter_ind]), "<br/>") 
            })
            
            ggplot(df_filter(),na.rm = FALSE)+        # df_filter(): refilter data whenever the collapsed range changes
              geom_bar(aes(x = survey_question,fill=factor(ans , levels = names(plot_level), labels = unlist(plot_level))),width=0.9)+
              ggtitle(paste("Collapsed Plot for",input$plot_facet,">=",input$collapse_range[1],"&",input$plot_facet,"<=",input$collapse_range[2]))+
              labs(fill= "Answers") +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1))
            
            # horizontal & vertical collapsed facet plot
          }else if(length(input$plot_var)>0 & input$plot_facet_v != "No" & input$collapse_or_not == "Yes"){
            
            # reactive expression: refilter data whenever the collapsed range changes
            df_filter <- reactive({
              df_plot2 <- df_all[c(input$plot_facet,input$plot_facet_v, input$plot_var)]
              colnames(df_plot2)[c(1,2)] <- c("plot_facet_h","plot_facet_v")
              
              df_plot_long2 <- gather(df_plot2,key = survey_question,value =  ans,-c(plot_facet_h,plot_facet_v))
              df_plot_long2$plot_facet_h <- df_plot_long2$plot_facet_h %>% as.character() %>% as.numeric()
              df_plot3 <- df_plot_long2 %>%
                filter(plot_facet_h >= input$collapse_range[1] & plot_facet_h <= input$collapse_range[2])
              
              df_plot3$plot_facet_v <- df_plot3$plot_facet_v %>% as.factor()
              levels(df_plot3$plot_facet_v) <- paste(input$plot_facet_v,"=",levels(df_plot3$plot_facet_v))
              
              df_plot3
            })
            
            # print the factor labels for horizontal facet variable
            output$legend_collapse <- renderText({
              plot_level_facet <- all_level[[grep(sprintf("([^_]+|^)(%s)", input$plot_facet), unlist(lapply(merge_choice, function(x) paste(unlist(x),collapse=" "))))]]
              filter_ind <- which(names(plot_level_facet) %in% df_filter()[,1])   # df_filter(): refilter data whenever the collapsed range changes
              paste(names(unlist(plot_level_facet[filter_ind])),"=",unlist(plot_level_facet[filter_ind]), "<br/>") 
            })
            
            ggplot(df_filter(),na.rm = FALSE)+        # df_filter(): refilter data whenever the collapsed range changes
              geom_bar(aes(x = survey_question,fill=factor(ans , levels = names(plot_level), labels = unlist(plot_level))),width=0.9)+
              ggtitle(paste("Collapsed Plot for",input$plot_facet,">=",input$collapse_range[1],"&",input$plot_facet,"<=",input$collapse_range[2]))+
              labs(fill= "Answers") +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1))+
              facet_grid(plot_facet_v ~.)
          }
        ) # end of "output$plot2"
      }) # end of observe4
    } # end of "d()==3" for plot
  }) # end of observe1: which dataset is chosen
}) # end of shinyServer