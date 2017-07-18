
###################################### library package #######################################
library(shiny)
library(shinyBS)

######################### UI

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),

  fluidRow(
    # select tag font size
    tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),

    #column1
    column(3,
           selectInput("dataset", "Choose Dataset", choices = list("CDI" = 1, 
                                                                   "Motor" = 2,
                                                                   "CDI and Motor Merged" = 3),
                       selected = 1),
           # Create button to download csv file from the app to user's local machine.
           downloadButton("downloadData", "Download Full Data")
    ), # end of column1
    
    
    # Only display these widgets if user selected CDI dataset becasue these only apply to CDI survey
    conditionalPanel(
      condition = "input.dataset == 1",
      # column2
      column(5,
             helpText("Select the section you want to analyze from CDI dataset"),
             # Allows user to select 1 of the choices; this input impacts the possible inputs for table and plot
             selectInput("cdi_set", label = "Select section of words/phrases: ",
                         choices = sets_cdi)
      )
    ), # end of conditionalPanel (dataset1) 
    ### conditionalPanel for merged dataset
    conditionalPanel(
      condition = "input.dataset == 3",
      # column2
      column(4,selectInput("merge_set", label = "Select the section(s) you want to analyze",
                           choices = c(sets_cdi, "Motor" = 7), multiple = TRUE))) # end of conditionalPanel (dataset3)
  ), # end of fluidRow
#################################################### tab ###########################################  
  h2("Survey Results", align = "center"),
  tabsetPanel(
    id = 'tab',

################################################### tab: table ####################################
    tabPanel('table',
               column(4,selectInput("df_colChoices", label = "Select Columns for Data Table",
                                    choices = c("SubjectNumber","Child_gender"),selected = NULL,
                                    multiple = TRUE)),
               column(4,helpText("Download the datatable below to a csv file."),
                      downloadButton("download_selected", "Download Selected & Filtered Data")),
             DT::dataTableOutput("table")),

#################################################### tab: plot ###################################
    tabPanel('Plot',
             
             ### cdi & motor plot
             conditionalPanel(
               condition = "input.dataset ==1||input.dataset ==2",
               column(3,selectizeInput("df_plot_y", label = "Variable(s) you want to view (up to 3 items)",
                                       choices = cdi_choice[[1]],multiple = TRUE, options = list(maxItems = 3))),
               column(3,selectInput("df_plot_x", label = "Color/Category",
                                    choices = c(cdi_choice_x))),
               column(2,radioButtons('per_plot', 'Y-axis', list('Count'=1,'Percentage'=2), selected = 2))
             ),
             
             ### merge dataset plot
             conditionalPanel(
               condition = "input.dataset == 3",
               column(4,selectInput("plot_sec", "Question section to view", choices = c(sets_cdi,"Motor"=7)),
                      actionButton("show_data", "View Plot Data")),
               column(4,selectInput("plot_var", label = "Variable(s) you wish to view in the plot",
                                    choices = merge_choice[[1]],
                                    multiple = TRUE)),
               column(4,selectInput("plot_facet", "Variable for faceting (horizontal)", choices = merge_choice),
                      selectInput("plot_facet_v", "Variable for faceting (vertical)", choices = c("No","Child_gender"))
               ),

               ### popup: plot data
               bsModal("plot_data", "Data used for plot", "show_data", size = "large",
                       div(style = 'overflow-x: scroll',
                           DT::dataTableOutput("table_plot"),
                           downloadButton("download_popup", "Download Plot Data")))
             ),

             column(12, plotOutput("plot")),
             
             ### mosaic plot for dataset 1&2
             conditionalPanel(
               condition = "input.dataset == 1||input.dataset == 2",
               column(12,strong("Quantity Comparison")),
               column(4,selectInput("mosaic_choice", "Filter the Color/Category variable", choices = c("all"), selected = "all")),
               column(4,helpText("Legend:"),htmlOutput("legend_mosaic"))),
             
             ### collapsed plot for merged data
             conditionalPanel(
               condition = "input.dataset ==3",
               column(4,selectInput("collapse_or_not","Show Collapsed Plot", choices = c("Yes","No"), selected = "No"))
             ),
             conditionalPanel(
               condition = "input.dataset ==3 & input.collapse_or_not=='Yes'",
               column(4,sliderInput("collapse_range",label = tags$h5("Collapsed Range (horizontal faceting variable)"),value=c(0,1),min=0,max =1,step = 1)),
               column(4,htmlOutput("legend_collapse"))
             ),
             
             plotOutput("plot2",height = 300)
    )# end of tabPanel"Plot"
  ) # end of tabsetPanel
))
