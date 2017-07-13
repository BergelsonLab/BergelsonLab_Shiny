
###################################### library package #######################################
library(shiny)
library(dplyr)
library(stringr)
library(shinyBS)

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
###### filter dataa
df_cdi <- df_cdi %>% filter(grepl("[a-z]",SubjectNumber,ignore.case = TRUE)==FALSE)
df_cdi$Subj <- str_match(df_cdi$SubjectNumber,"(^\\d+)_")[,2]
colnames(df_cdi)[which(names(df_cdi) == "AgeMonthCDI_Corrected")] <- "AgeMonth_Corrected"

###### for child gender, change all "Female" to "F", 'Male" to "M"
##### Shiny Table doesn't distinguish capital letters. Cannot filter "male" obs (Fe'male' also contain male)
df_cdi$Child_gender[which(df_cdi$Child_gender=="Female")] <- "F"
df_cdi$Child_gender[which(df_cdi$Child_gender=="Male")] <- "M"

###### convert txt level to number
df_cdi[df_cdi=='Yes'] <- 1                       # Convert 'Yes' values to = 1
df_cdi[df_cdi=='No'] <- 0                        # Convert 'No' values to = 0
df_cdi[df_cdi=='Often'] <- 2                     # Convert 'Often' to = 2
df_cdi[df_cdi=='Sometimes'] <- 1                 # Convert 'Sometimes' to = 1
df_cdi[df_cdi=='Never'] <- 0                     # Convert 'Never' to = 0
df_cdi[df_cdi=='Not Yet'] <- 0                   # Convert 'Not Yet' to = 0



###### drop "ResponseID
df_cdi$ResponseID <- NULL

###### gender information from cdi
df_gender <- df_cdi %>% select(SubjectNumber,Child_gender)
df_gender$Subj <- str_match(df_gender$SubjectNumber,"(.+)_")[,2] %>% str_pad(2,side = "left",pad = 0)
df_gender <- df_gender %>% 
  select(Subj,Child_gender) %>% 
  unique()

###### factor level

# First Signs of Understanding
cdi_level_1 <- list("0" = "no", "1" = "yes", "2"="2", "3"="3", "4"="4","5"="5")
# Phrases Understood
cdi_level_2 <- list("0" = "no","1" = "yes", "2"="2","3"="3","4"="4","5"="5")
# Starting to Produce
cdi_level_3 <- list("0" = "Never", "1" = "Sometimes", "2" = "Often","3"="3","4"="4","5"="5")
# Vocab Checklist
cdi_level_4 <- list("0" = "Doesn't Understand", "1" = "Understands", "2" = 'Understands and says', "3"="3","4"="4","5"="5")
# Gestures_ASN
cdi_level_5 <- list("0" = "Not Yet", "1" = "Sometimes","2" = "Often","3"="3","4"="4","5"="5")
# Games and Routines
cdi_level_6 <- list("0" = "no", "1" = "yes", "2"="2", "3"="3","4"="4","5"="5")

####################################### clean df_motor

###### check missing
df_motor <- df_motor[!is.na(df_motor$ResponseID),]     # drop obs with missing ResponseID

###### filter dataa
df_motor <- df_motor %>% filter(grepl("[a-z]",Subj,ignore.case = TRUE)==FALSE)

###### add gender information
df_motor$Subj <- df_motor$Subj %>% str_pad(2,side = "left",pad = 0)
df_motor <- merge(df_motor,df_gender, key="Subj",all.x = TRUE)

###### consistent name
colnames(df_motor)[which(names(df_motor) %in% "Subject.Number_Month")] <- "SubjectNumber"
colnames(df_motor)[which(names(df_motor) == "AgeMonthMotor_Corrected")] <- "AgeMonth_Corrected"

###### drop "ResponseID"
df_motor$ResponseID <- NULL

###### missing data: drop "text" variables
df_motor <- df_motor %>% select(-crawling_belly_TEXT, -crawling_hands_knees_TEXT,-cruising_TEXT,-walking_TEXT)

###### factor levels
motor_level <- list("0"="0",
                    "1"="Sure that child does NOT show behavior",
                    "2"="Child probably does NOT show behavior yet",
                    "3"="Unsure whether child could do this or not",
                    "4"="Child probably shows this behavior",
                    "5"="Sure that child shows this behavior and remember a paricular instance")


################################ merge cdi dataset and motor dateset

###### duplicated columns
col_dup <- setdiff(names(df_cdi)[names(df_cdi) %in% names(df_motor)],c("Subj","SubjectNumber","Child_gender","AgeMonth_Corrected"))
colnames(df_cdi)[which(names(df_cdi) %in% col_dup)] <- paste0(col_dup,"_cdi")
colnames(df_motor)[which(names(df_motor) %in% col_dup)] <- paste0(col_dup,"_motor")

df_merge = merge(df_cdi,df_motor,key=c("Subj","Child_gender","AgeMonth_Corrected"))

###### change type to numeric if possible (note: df_cdi has already dropped missing values)
df_merge_num = suppressWarnings(data.matrix(df_merge)) %>% as.data.frame()
sum_merge <- sapply(df_merge,function(x) sum(is.na(x)))
sum_merge_num <- sapply(df_merge_num,function(x) sum(is.na(x)))
ind_merge_num <- which(sum_merge == sum_merge_num)
df_merge[,ind_merge_num] <- sapply(df_merge[,ind_merge_num], as.numeric)

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

sets_cdi <- list("First Signs of Understanding" = 1, "Phrases Understood" = 2,
                 "Starting to Produce" = 3, "Vocab Checklist" = 4, "Gestures_ASN" = 5, "Games and Routines" = 6)


cdi_choice = list(col_first_sign,col_phrases,col_start_talk,col_talk,col_first_gest,col_gest)
cdi_basic= c("SubjectNumber","AgeMonth_Corrected",
             "AgeDaysCDI", "WithinTenDays_final","Child_gender")
cdi_choice_x <- c("Child_gender","AgeMonth_Corrected")

###### Motor
###### find factor variables
num_ans <- sapply(df_motor,function(x) length(unique(x)))


motor_choice <- names(df_motor)[which(names(df_motor)=="rest_on_body"):which(names(df_motor)=="somersault")]
motor_basic <- c("SubjectNumber", "AgeMonth_Corrected","AgeDaysMotor",
                 "weight","ageweight","Child_gender")

motor_choice_x <- c("Child_gender","AgeMonth_Corrected")

merge_choice = c(cdi_choice,list(motor_choice))
names(merge_choice) <- c(names(sets_cdi),"Motor")

###### levels
all_level <- list(cdi_level_1,cdi_level_2,cdi_level_3,cdi_level_4,cdi_level_5,cdi_level_6,motor_level)

######################### UI

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  ##################### TRY: new layout
  
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
                       div(style = 'overflow-x: scroll',DT::dataTableOutput("table_plot")))
             ),

             column(12, plotOutput("plot")),
             
             ### mosaic plot for dataset 1&2
             conditionalPanel(
               condition = "input.dataset == 1||input.dataset == 2",
               column(6,strong("Quantity Comparison"),
                      selectInput("mosaic_choice", "Filter", choices = c("all"), selected = "all"))),
             
             ### collapsed plot for merged data
             conditionalPanel(
               condition = "input.dataset ==3",
               column(4,selectInput("collapse_or_not","Show Collapsed Plot", choices = c("Yes","No"), selected = "No"))
             ),
             conditionalPanel(
               condition = "input.dataset ==3 & input.collapse_or_not=='Yes'",
               column(6,sliderInput("collapse_range",label = tags$h5("Collapsed Range (horizontal faceting variable)"),value=c(0,1),min=0,max =1,step = 1))
             ),
             
             plotOutput("plot2",height = 300)
    )# end of tabPanel"Plot"
  ) # end of tabsetPanel
))
