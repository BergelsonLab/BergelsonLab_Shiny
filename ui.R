
###################################### library package #######################################
library(shiny)
library(dplyr)
library(stringr)

###################################### load data #############################################
# Read in cdi datafile to work with as df_cdi
df_cdi <- read.csv("B:/Seedlings/Subject_Info_and_Paperwork/CDI & Motor/final_cdi_merged_cleaned.csv", stringsAsFactors = FALSE)

# Read in motor datafile to work with as df_motor
df_motor <- read.csv("B:/Seedlings/Subject_Info_and_Paperwork/CDI & Motor/final_motor_merged_cleaned.csv",  stringsAsFactors = FALSE)

###################################### clean data ############################################

###################################### clean df_cdi

###### check missing
df_cdi[df_cdi==""] <- NA
df_cdi <- df_cdi[!is.na(df_cdi$ResponseID),]       # drop obs with missing ResponseID
df_cdi <- df_cdi[!df_cdi$SubjectNumber %in% c("P08_17","08semns1","08semns2"),] 


###### dealing with partial missing
# ind_understand = grep("SubjectNumber|Subject_Month_|AgeMonthCDI_Corrected|^Understand_.*",names(df_cdi),value = FALSE) # column names starting with "Understand_"
# df_cdi_understand <- df_cdi[,ind_understand]
# df_cdi_understand$Subject_Number_ <- str_extract(df_cdi_understand$Subject_Number_,"\\d+")
# df_cdi_understand$Subject_Month_ <- str_extract(df_cdi_understand$Subject_Month_,"\\d+") %>% as.numeric()
# df_cdi_understand <- df_cdi_understand  %>% arrange(Subject_Number_,AgeMonthCDI_Corrected)
# 


###### For missing obs, if answering "yes" earlier, the missing cell will be "yes"
###### For missing obs, if answering "no" later, the missing cell will be "no"

###### drop rows with missing value
#df_cdi = df_cdi %>%na.omit()
###### convert txt level to number
df_cdi[df_cdi=='Yes'] <- 1                       # Convert 'Yes' values to = 1
df_cdi[df_cdi=='No'] <- 0                        # Convert 'No' values to = 0
df_cdi[df_cdi=='Often'] <- 2                     # Convert 'Often' to = 2
df_cdi[df_cdi=='Sometimes'] <- 1                 # Convert 'Sometimes' to = 1
df_cdi[df_cdi=='Never'] <- 0                     # Convert 'Never' to = 0
df_cdi[df_cdi=='Not Yet'] <- 0                   # Convert 'Not Yet' to = 0

###### change type to numeric if possible (note: df_cdi has already dropped missing values)
df_num = suppressWarnings(data.matrix(df_cdi)) %>% as.data.frame()
ind_num <- which(sapply(df_num,function(x) any(is.na(x))==FALSE))
df_cdi[,ind_num] <- sapply(df_cdi[,ind_num], as.numeric) # numeric column index

###### drop "ResponseID
df_cdi$ResponseID <- NULL

###### gender information from cdi
df_gender <- df_cdi %>% select(SubjectNumber,Child_gender)
df_gender$Subj <- str_match(df_gender$SubjectNumber,"(.+)_")[,2]
df_gender <- df_gender %>% 
  select(Subj,Child_gender) %>% 
  unique()


####################################### clean df_motor

###### check missing
df_motor <- df_motor[!is.na(df_motor$ResponseID),]     # drop obs with missing ResponseID
df_motor <- df_motor[!df_motor$Subject.Number_Month %in% c("P08_17","08semns1","08semns2"),] 
###### drop rows with missing value
#df_motor <- df_motor %>% na.omit()

###### add gender information
df_motor <- merge(df_motor,df_gender, key="Subj",all.x = TRUE)

###### consistent name
colnames(df_motor)[which(names(df_motor) %in% "Subject.Number_Month")] <- "SubjectNumber"

###### drop "ResponseID"
df_motor$ResponseID <- NULL

################################ merge cdi dataset and motor dateset

###### duplicated columns
col_dup <- setdiff(names(df_cdi)[names(df_cdi) %in% names(df_motor)],c("SubjectNumber","Child_gender"))
colnames(df_cdi)[which(names(df_cdi) %in% col_dup)] <- paste0(col_dup,"_cdi")
colnames(df_motor)[which(names(df_motor) %in% col_dup)] <- paste0(col_dup,"_motor")

df_merge = merge(df_cdi,df_motor,key=c("SubjectNumber","Child_gender"))

# ###### change type to numeric if possible (note: df_cdi has already dropped missing values)
# df_merge_num = suppressWarnings(data.matrix(df_merge)) %>% as.data.frame()
# sum_merge <- sapply(df_merge,function(x) sum(is.na(x)))
# sum_merge_num <- sapply(df_merge_num,function(x) sum(is.na(x)))
# ind_merge_num <- which(sum_merge == sum_merge_num)
# df_merge[,ind_merge_num] <- sapply(df_merge[,ind_merge_num], as.numeric)

######################################### Shiny UI ###############################################

######################## options

###### CDI survey sections
col_understand = grep("^Understand_.*",names(df_cdi),value = TRUE) # column names starting with "Understand_"
col_first_sign = col_understand[1:3]                               # A. First Signs of Understanding
col_phrases = col_understand[4:(length(col_understand)-2)]        # B. Phrases
col_start_talk = col_understand[(length(col_understand)-1):length(col_understand)] # C. Starting to Talk
col_talk = grep("^Talk_.*",names(df_cdi),value = TRUE)            # D.Vocabulary Checklist
col_gestures = grep("^Gestures_.*",names(df_cdi),value = TRUE)    # column names starting with "Gestures_"
col_first_gest = col_gestures[1:12]                               # A. First Communicative Gestures
col_gest = col_gestures[13:length(col_gestures)]                  # B.-E.

radio_cdi <- list("First Signs of Understanding" = 1, "Phrases Understood" = 2,
     "Starting to Produce" = 3, "Vocab Checklist" = 4, "Gestures_ASN" = 5, "Games and Routines" = 6)


cdi_choice = list(col_first_sign,col_phrases,col_start_talk,col_talk,col_first_gest,col_gest)
cdi_basic= c("SubjectNumber","AgeMonthCDI_Corrected",
             "AgeDaysCDI", "WithinTenDays_final","Child_gender")
cdi_choice_x <- c("Child_gender","AgeMonthCDI_Corrected")

###### Motor
###### find factor variables
num_ans <- sapply(df_motor,function(x) length(unique(x)))


motor_choice <- names(df_motor)[which(names(df_motor)=="rest_on_body"):which(names(df_motor)=="somersault")]
motor_basic <- c("SubjectNumber", "AgeMonthMotor_Corrected","AgeDaysMotor",
                 "weight","ageweight","Child_gender")

motor_choice_x <- c("Child_gender","AgeMonthMotor_Corrected")

merge_choice = c(cdi_choice,list(motor_choice))

######################### UI

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  ##################### TRY: new layout
  
  fluidRow(
    #column1
    column(2,
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
      
      #column2
      column(3,
             strong("Select Columns"),
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
             downloadButton("download_selected_cdi", "Download Selected Data")
             ), #end of column2
      
      #column3
      column(3,
             strong("Plot"),
             helpText("Select the two variables you wish to view in the plot."),
             selectInput("cdi_plot_y", label = "Variable you want to view",
                         choices = c("",cdi_choice[[1]]), selected = ""),
             selectInput("cdi_plot_x", label = "Color/Category",
                         choices = c("",cdi_choice_x), selected = ""),
             radioButtons('per_plot', 'Y-axis', list('Count'=1,'Percentage'=2), selected = 2)
             )
    ), # end of conditionalPanel (dataset1) 
    
    conditionalPanel(
      condition = "input.dataset == 2",
      
      column(3,
             strong("Select Columns"),
             helpText("Select columns that you wish to view in the table"),
             selectInput("motor_colChoices", label = "Select Columns for Data Table",
                         choices = motor_choice,
                         multiple = TRUE),
             downloadButton("download_selected_motor", "Download Selected Data")
             ),
      
      column(3,
             strong("Plot"),
             helpText("Select the two variables you wish to view in the plot."),
             selectInput("motor_plot_y", label = "Variable you want to view",
                         choices = c("",motor_choice), selected = ""),
             selectInput("motor_plot_x", label = "Color/Category",
                         choices = c("",motor_choice_x), selected = ""),
             radioButtons('per_plot2', 'Y-axis', list('Count'=1,'Percentage'=2), selected = 2)
             )
    ), # end of conditionalPanel (dataset2)
    conditionalPanel(
      condition = "input.dataset == 3",
      
      column(3,
             helpText("Select word/phrase options to analyze from CDI dataset"),
             radioButtons("radioButton_merge", label = "Select set of words/phrases from CDI dataset: ",
                          choices = radio_cdi),
             hr(),        
             helpText("Select the columns that you wish to view in the data table (from both CDI and Motor datasets)"),
             selectInput("merge_colChoices", label = "Select Columns for Data Table",
                         choices = unique(c(cdi_basic,motor_basic,sort(c(cdi_choice[[1]],motor_choice)))),
                         multiple = TRUE)
             ),
      
      column(3,
             strong("filter selected data"),
             selectInput("merge_filter", "Variable to Filter", choices = c(""),selected = ""),
             sliderInput("merge_range","Filter Selected Data",value=c(0,1),min=0,max =1,step = 1),
             helpText("Download the selected & filtered data from datatable to csv file."),
             downloadButton("download_selected_merge", "Download Selected & Filtered Data")
             ),
      
      column(3,
             strong("Plot"),
             selectInput("plot_sec", "Question section to view", choices = c(radio_cdi,"Motor"=7)),
             selectInput("plot_var", label = "Variable(s) you wish to view in the plot",
                         choices = merge_choice[[1]],
                         multiple = TRUE),
             selectInput("plot_filter", "Variable to Filter", choices = c("weight",unlist(merge_choice))),
             sliderInput("plot_range","Filter Selected Data",value=c(0,1),min=0,max =1,step = 1)
      )
    ) # end of conditionalPanel (dataset merged)
  ),
  
  h2("Survey Results", align = "center"),
  tabsetPanel(
    id = 'tab',
    tabPanel('table',DT::dataTableOutput("table")),
    tabPanel('Plot',plotOutput("plot", height = 500))
  )
))
