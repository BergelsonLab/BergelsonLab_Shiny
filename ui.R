
###################################### library package #######################################
library(shiny)
library(dplyr)

###################################### load data #############################################
# Read in cdi datafile to work with as df_cdi
df_cdi <- read.csv("C:/Users/Liwen/Downloads/final_cdi_merged_cleaned.csv", stringsAsFactors = FALSE)

# Read in motor datafile to work with as df_motor
df_motor <- read.csv("C:/Users/Liwen/Downloads/final_motor_merged_cleaned.csv",  stringsAsFactors = FALSE)

###################################### clean data ############################################

###################################### clean df_cdi
###### drop rows with missing value
df_cdi = df_cdi %>%na.omit()
###### convert txt level to number
df_cdi[df_cdi=='Yes'] <- 1                       # Convert 'Yes' values to = 1
df_cdi[df_cdi=='No'] <- 0                        # Convert 'No' values to = 0
df_cdi[df_cdi=='Often'] <- 2                     # Convert 'Often' to = 2
df_cdi[df_cdi=='Sometimes'] <- 1                 # Convert 'Sometimes' to = 1
df_cdi[df_cdi=='Never'] <- 0                     # Convert 'Never' to = 0
df_cdi[df_cdi=='Not Yet'] <- 0                   # Convert 'Not Yet' to = 0

###### set ResponseID as character
df_cdi$ResponseID <- as.character(df_cdi$ResponseID)
###### change type to numeric if possible (note: df_cdi has already dropped missing values)

df_num = suppressWarnings(data.matrix(df_cdi)) %>% as.data.frame()
ind_num <- which(sapply(df_num,function(x) any(is.na(x))==FALSE))
df_cdi[,ind_num] <- sapply(df_cdi[,ind_num], as.numeric) # numeric column index


####################################### clean df_motor
###### drop rows with missing value
df_motor <- df_motor %>% na.omit()
###### set ResponseID as character
df_motor$ResponseID <- as.character(df_motor$ResponseID)


################################ merge cdi dataset and motor dateset

###### duplicated columns
col_dup <- setdiff(names(df_cdi)[names(df_cdi) %in% names(df_motor)],c("Subject_Number_","Subject_Month_"))
colnames(df_cdi)[which(names(df_cdi) %in% col_dup)] <- paste0(col_dup,"_cdi")
colnames(df_motor)[which(names(df_motor) %in% col_dup)] <- paste0(col_dup,"_motor")

df_merge = merge(df_cdi,df_motor,by=c("Subject_Number_","Subject_Month_"))

######################################### Shiny UI ###############################################

######################## options

###### CDI survey sections
col_unerstand = grep("^Understand_.*",names(df_cdi),value = TRUE) # column names starting with "Understand_"
col_first_sign = col_unerstand[1:3]                               # A. First Signs of Understanding
col_phrases = col_unerstand[4:(length(col_unerstand)-2)]          # B. Phrases
col_start_talk = col_unerstand[(length(col_unerstand)-1):length(col_unerstand)] # C. Starting to Talk
col_talk = grep("^Talk_.*",names(df_cdi),value = TRUE)            # D.Vocabulary Checklist
col_gestures = grep("^Gestures_.*",names(df_cdi),value = TRUE)    # column names starting with "Gestures_"
col_first_gest = col_gestures[1:12]                               # A. First Communicative Gestures
col_gest = col_gestures[13:length(col_gestures)]                  # B.-E.

radio_cdi <- list("First Signs of Understanding" = 1, "Phrases Understood" = 2,
     "Starting to Produce" = 3, "Vocab Checklist" = 4, "Gestures_ASN" = 5, "Games and Routines" = 6)

cdi_color_choices <- c(1:ncol(df_cdi))
names(cdi_color_choices) <- names(df_cdi)

cdi_choice = list(col_first_sign,col_phrases,col_start_talk,col_talk,col_first_gest,col_gest)
cdi_basic= c("SubjectNumber_Month", "Subject_Number_","Subject_Month_","AgeMonthCDI_Uncorrected",
             "AgeDaysCDI", "WithinTenDays_final", "AgeMonthCDI_Corrected","Child_gender")
cdi_choice_x <- c("Child_gender","AgeMonthCDI_Corrected")

###### Motor
###### find factor variables
num_ans <- sapply(df_motor,function(x) length(unique(x)))
motor_choice <- num_ans[num_ans>1 & num_ans < 10] %>% names()
motor_basic <- c("Subject_Number_","Subject_Month_", "Age_Corrected",
                 "weight","ageweight")

motor_choice_x <- c("Age_Corrected")

######################### UI

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  # Sidebar with widgets for user manipulation of data and display in main panel
  # 1st Download Button and Radio Button remain present regardless of user input
  sidebarLayout(
    sidebarPanel(
      # Drop down menu to allow users to select either CDI survey or Motor survey to analyze
      selectInput("dataset", "Choose Dataset", choices = list("final_cdi_merged_cleaned.csv" = 1, 
                                                              "final_motor_merged_cleaned.csv" = 2,
                                                              "cdi_and_motor_merged.csv" = 3),
                  selected = 1),
      # Help text describes what the following widget does and what the user should do
      helpText("Download data to a csv file containing all rows and columns from the selected .csv file above"),
      # Create button to download csv file from the app to user's local machine.
      downloadButton("downloadData", "Download Full Data"),
      
      hr(),
      strong("Select Columns"),
      
      # Only display these widgets if user selected CDI dataset becasue these only apply to CDI survey
      conditionalPanel(
        
        condition = "input.dataset == 1",
        helpText("Select word/phrase options to analyze from CDI dataset"),
        # Allows user to select 1 of the choices; this input impacts the possible inputs for table and plot
        radioButtons("radioButton", label = "Select set of words/phrases: ",
                     choices = radio_cdi),
        hr(),
        helpText("Select the columns that you wish to view in the data table"),
        selectInput("cdi_colChoices", label = "Select Columns for Data Table",
                    choices = cdi_choice[[1]],
                    multiple = TRUE),
        helpText("Download the selected data from datatable to csv file."),
        downloadButton("download_selected_cdi", "Download Selected Data"),
        hr(),
        
        # plot
        strong("Plot"),
        helpText("Select the two variables you wish to view in the plot."),
        selectInput("cdi_plot_y", label = "Variable you want to view",
                       choices = c("",cdi_choice[[1]]), selected = ""),
        selectInput("cdi_plot_x", label = "Color/Category",
                    choices = c("",cdi_choice_x), selected = ""),
        radioButtons('per_plot', 'Y-axis', list('Count'=1,'Percentage'=2), selected = 2)
 
      ), # end of conditionalPanel (dataset1) 
      
      conditionalPanel(
        condition = "input.dataset == 2",
        helpText("Select columns that you wish to view in the table"),
        selectInput("motor_colChoices", label = "Select Columns for Data Table",
                    choices = motor_choice,
                    multiple = TRUE),
        downloadButton("download_selected_motor", "Download Selected Data"),
        hr(),
        #plot
        strong("Plot"),
        helpText("Select the two variables you wish to view in the plot."),
        selectInput("motor_plot_y", label = "Variable you want to view",
                    choices = c("",motor_choice), selected = ""),
        selectInput("motor_plot_x", label = "Color/Category",
                    choices = c("",motor_choice_x), selected = ""),
        radioButtons('per_plot2', 'Y-axis', list('Count'=1,'Percentage'=2), selected = 2)
        
      ), # end of conditionalPanel (dataset2)
      conditionalPanel(
        condition = "input.dataset == 3",
        helpText("Select word/phrase options to analyze from CDI dataset"),
        radioButtons("radioButton_merge", label = "Select set of words/phrases from CDI dataset: ",
                     choices = radio_cdi),
        hr(),        
        helpText("Select the columns that you wish to view in the data table (from both CDI and Motor datasets)"),
        selectInput("merge_colChoices", label = "Select Columns for Data Table",
                    choices = unique(c(cdi_basic,motor_basic,sort(c(cdi_choice[[1]],motor_choice)))),
                    multiple = TRUE),
        # filter selected data
        br(),
        selectInput("merge_filter", "Variable to Filter", choices = c(""),selected = ""),
        sliderInput("merge_range","Filter Selected Data",value=c(0,1),min=0,max =1,step = 1)
                                                                                                          
        
      ) # end of conditionalPanel (dataset merged)
      ), # end of sidebarPanel

    # Show a the table and plot from user input in sidebar panel widgets
    mainPanel(
      h2("Survey Results", align = "center"),
      tabsetPanel(
        id = 'tab',
        tabPanel('table',DT::dataTableOutput("table")),
        tabPanel('Plot',plotOutput("plot", height = 500))
      )
      )
)
))
