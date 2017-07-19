######################################## Global objects #############################
library(dplyr)
library(stringr)
####################################### load datasets
load("./data.Rda")

####################################### global variables
###### CDI survey sections
col_understand = grep("^Understand_.*",names(df_cdi),value = TRUE) # column names starting with "Understand_"
col_first_sign = col_understand[1:3]                               # A. First Signs of Understanding
col_phrases = col_understand[4:(length(col_understand)-2)]         # B. Phrases
col_start_talk = col_understand[(length(col_understand)-1):length(col_understand)] # C. Starting to Talk
col_talk = grep("^Talk_.*",names(df_cdi),value = TRUE)             # D.Vocabulary Checklist
col_gestures = grep("^Gestures_.*",names(df_cdi),value = TRUE)     # column names starting with "Gestures_"
col_first_gest = col_gestures[1:12]                                # A. First Communicative Gestures
col_gest = col_gestures[13:length(col_gestures)]                   # B.-E.

### cdi sections
sets_cdi <- list("First Signs of Understanding" = 1, "Phrases Understood" = 2,
                 "Starting to Produce" = 3, "Vocab Checklist" = 4, "Gestures_ASN" = 5, "Games and Routines" = 6)
cdi_choice = list(col_first_sign,col_phrases,col_start_talk,col_talk,col_first_gest,col_gest)

### basic child info (fixed columns in shiny data table)
cdi_basic= c("SubjectNumber","AgeMonth_Corrected", "AgeDaysCDI", "WithinTenDays_final","Child_gender")

### factor level
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

###### Motor
### select motor columns
motor_choice <- names(df_motor)[which(names(df_motor)=="rest_on_body"):which(names(df_motor)=="somersault")]

### basic child info (fixed columns in shiny data table)
motor_basic <- c("SubjectNumber", "AgeMonth_Corrected","AgeDaysMotor",
                 "weight","ageweight","Child_gender")

### factor levels
motor_level <- list("0"="0",
                    "1"="Doesn't show",
                    "2"="Probably doesn't show",
                    "3"="Unsure",
                    "4"="Probably shows",
                    "5"="Shows")

###### Merged dataset
### data sections
merge_choice = c(cdi_choice,list(motor_choice))
names(merge_choice) <- c(names(sets_cdi),"Motor")

### factor levels
all_level <- list(cdi_level_1,cdi_level_2,cdi_level_3,cdi_level_4,cdi_level_5,cdi_level_6,motor_level)

### x-axis for distribution plot
dist_x <- c("Child_gender","AgeMonth_Corrected")