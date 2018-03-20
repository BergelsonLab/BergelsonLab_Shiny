library(dplyr)
library(stringr)

###################################### load csv data #############################################
# Read in cdi datafile to work with as df_cdi
df_cdi <- read.csv("data/cdi.csv", stringsAsFactors = FALSE)
# Read in motor datafile to work with as df_motor
df_motor <- read.csv("data/motor.csv",  stringsAsFactors = FALSE)

###################################### clean data ############################################

###################################### clean df_cdi
df_cdi[df_cdi==""] <- NA
df_cdi <- df_cdi[!is.na(df_cdi$ResponseID),]       # drop obs with missing ResponseID
df_cdi <- df_cdi %>% filter(grepl("[a-z]",SubjectNumber,ignore.case = TRUE)==FALSE)
df_cdi$Subj <- str_match(df_cdi$SubjectNumber,"(^\\d+)_")[,2]
colnames(df_cdi)[which(names(df_cdi) == "AgeMonthCDI_Corrected")] <- "AgeMonth_Corrected"

###### for child gender, change all "Female" to "F", 'Male" to "M"
##### Shiny DataTable doesn't distinguish capital letters. Cannot filter "male" obs (Fe'male' also contain male)
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

###### cdi gender information
df_gender <- df_cdi %>% select(SubjectNumber,Child_gender)
df_gender$Subj <- str_match(df_gender$SubjectNumber,"(.+)_")[,2] %>% str_pad(2,side = "left",pad = 0)
df_gender <- df_gender %>% 
  select(Subj,Child_gender) %>% 
  unique()

####################################### clean df_motor
df_motor <- df_motor[!is.na(df_motor$ResponseID),]     # drop obs with missing ResponseID
df_motor <- df_motor %>% filter(grepl("[a-z]",Subj,ignore.case = TRUE)==FALSE)

###### add gender information
df_motor$Subj <- df_motor$Subj %>% str_pad(2,side = "left",pad = 0)
df_motor <- merge(df_motor,df_gender, key="Subj",all.x = TRUE)

###### consistent colname
colnames(df_motor)[which(names(df_motor) %in% "Subject.Number_Month")] <- "SubjectNumber"
colnames(df_motor)[which(names(df_motor) == "AgeMonthMotor_Corrected")] <- "AgeMonth_Corrected"

###### drop "ResponseID"
df_motor$ResponseID <- NULL

###### missing data: drop "text" variables
df_motor <- df_motor %>% select(-crawling_belly_TEXT, -crawling_hands_knees_TEXT,-cruising_TEXT,-walking_TEXT)

################################ merge cdi dataset and motor dateset

###### duplicated columns
col_dup <- setdiff(names(df_cdi)[names(df_cdi) %in% names(df_motor)],c("Subj","SubjectNumber","Child_gender","AgeMonth_Corrected"))
colnames(df_cdi)[which(names(df_cdi) %in% col_dup)] <- paste0(col_dup,"_cdi")
colnames(df_motor)[which(names(df_motor) %in% col_dup)] <- paste0(col_dup,"_motor")

df_merge = merge(df_cdi,df_motor,key=c("Subj","Child_gender","AgeMonth_Corrected"))

###### change type to numeric if applicable
df_merge_num = suppressWarnings(data.matrix(df_merge)) %>% as.data.frame()
sum_merge <- sapply(df_merge,function(x) sum(is.na(x)))
sum_merge_num <- sapply(df_merge_num,function(x) sum(is.na(x)))
ind_merge_num <- which(sum_merge == sum_merge_num)
df_merge[,ind_merge_num] <- sapply(df_merge[,ind_merge_num], as.numeric)

################################# save cleaned data
save(df_cdi,df_motor,df_merge,file="./data.Rda")
rm(list = ls())