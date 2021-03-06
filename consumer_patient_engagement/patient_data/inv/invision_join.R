username <- "tvickers"

require(sqldf)
require(dplyr)
require(data.table)

setwd(paste0("C:/Users/",username, "/Documents/GitRepositories/Ad_Hoc/consumer_patient_engagement/consumer_data"))
source("consumer_myhome.R")

cm_clean <- function(df, fields = NA, doLowercase = T, dobothTrim = T, catchBlank = F) {
  #for each field in the 'fields vector, convert change to lowercase and trim front and back of whitespace
  if(sum(is.na(fields)) < 1) {
    if(doLowercase) {
      df[,fields] <- sapply(df[,fields], tolower)
      print(paste(fields, "changed to lowercase"))
    }
    if(dobothTrim) {
      df[,fields] <- sapply(df[,fields], trimws, which = "both")
      print(paste(fields, "trimmed whitespace"))
    }
    if(catchBlank) {
      
    }
    return(df)
  }
  print("no change to fields")
  return(df)
}

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/patientdata_input/inv"))
inv_commercial <- read.table("inv_commercial.txt", comment.char = "", quote="", fill=T, sep = "\t", stringsAsFactors = F)
inv_test <- read.table("inv_withnew_test.txt", comment.char = "", quote="", fill=T, sep = "\t", stringsAsFactors = F)

adtall_names <- c("load_date", "firstname", "lastname", "gender", "dateofbirth", "adtsystemname", "medicalrecordnumber", "accountnumber", "facilitycode", "facilityname",
"admissiondate", "dischargedate", "financialclasscode", "financialclassdescription", "patienttype", "patienttypedesc", "servicelinecode", "servicelinedesc",
"serviceareacode", "serviceareadesc", "middleinitial", "city", "state", "zip", "email", "phone", "primaryinsurancename", "emailmarketingoptin",
"region", "facility","addressline1" ,"addressline2", "patientmaritalstatus", "admissionfinaldate", "primaryfinancialclass", "admissionyear",
"admissionmonth", "servicecode", "agefromadmission", "surgicalprocedure1", "surgicalprocedure2", "surgicalprocedure3", "drgcategory", "birthyear",
"firstproceduredate", "lastservicedate", "doctornumber", "hospitalservicecode", "providerspecialty", "appointment_booking_id")

VR_names <- c("grouping_operator", "loadtime", "joined_grouping_operator", "previousadmitdate", "currentadmitdate", "nextadmitdate", "yearsfromprev", "yearstonext", "neworreturn")

full_table_names <- c(adtall_names, VR_names)

names(inv_commercial) <- adtall_names
names(inv_test) <- full_table_names

## INSERT QUALITY CHECKS
table(inv_test$admissiondate==inv_test$currentadmitdate) #admission dates line up
length(unique(inv_commercial$medicalrecordnumber))
length(unique(inv_test$medicalrecordnumber))

inv_commercial_noextra <- subset(inv_commercial, select = -c(surgicalprocedure1, surgicalprocedure2, surgicalprocedure3, firstproceduredate, lastservicedate, drgcategory, doctornumber, providerspecialty))
inv_commercial_noextra <- unique(inv_commercial_noextra)

inv_test_noextra <- subset(inv_test, select = -c(surgicalprocedure1, surgicalprocedure2, surgicalprocedure3, firstproceduredate, lastservicedate, drgcategory, doctornumber, providerspecialty))
inv_test_noextra <- unique(inv_test_noextra)

#try run
inv_commercial <- inv_test_noextra

##

#cast medicalrecordnumber to character
inv_commercial$medicalrecordnumber <- as.character(inv_commercial$medicalrecordnumber)

#clean
inv_commercial <- cm_clean(inv_commercial, c("firstname", "lastname", "medicalrecordnumber"))

#name key preparation
#1) ignore middile initial fields (no such field)

#2) remove puncuation from FN, LN
inv_commercial$firstname <- gsub("\\.", "", inv_commercial$firstname) # remove periods
inv_commercial$firstname <- gsub("\\,", "", inv_commercial$firstname) # remove commas
inv_commercial$firstname <- gsub("-", "", inv_commercial$firstname) # remove hyphens

inv_commercial$lastname <- gsub("\\.", "", inv_commercial$lastname) # remove periods
inv_commercial$lastname <- gsub("\\,", "", inv_commercial$lastname) # remove commas
inv_commercial$lastname <- gsub("-", "", inv_commercial$lastname) # remove hyphens

#3) collapse remaining whitespace
inv_commercial$firstname <- gsub(" ", "", inv_commercial$firstname)
inv_commercial$lastname <- gsub(" ", "", inv_commercial$lastname)

#4) set to lowercase (already done)

#5) trim trailing whitespace (already done)

#create fullname field
inv_commercial$fullname <- paste(inv_commercial$firstname, inv_commercial$lastname, sep = " ")

#create joinkeys
inv_commercial$inv_name_dob_key <- paste0(inv_commercial$firstname, inv_commercial$lastname, inv_commercial$dateofbirth)
inv_commercial$inv_name_key <- paste0(inv_commercial$firstname, inv_commercial$lastname)

#cast
inv_commercial$admissiondate <- as.Date(as.character(inv_commercial$admissiondate), format="%Y%m%d")

#filter to the appropriate date range
smart_filter <- function(df, filter_name, start_date=NULL, end_date=NULL) {
  #first get the vector I want to filter by... have to do it this way to allow naming parameter
  filt_vec <- df[[filter_name]]
  #apply actual filter logic to just the vector
  log_vec <- filt_vec>=start_date & filt_vec <= end_date
  #use the vector to filter the dataframe
  filt_df <- df %>% filter(log_vec)
  return(filt_df)
}

inv_commercial_FY18 <- smart_filter(inv_commercial, "admissiondate", "2017-07-01", "2018-06-30")

# rought test of joins for myhome
withdob_results <- sqldf(
"SELECT * FROM inv_commercial_FY18 AS a JOIN all_withdob_key AS b ON a.inv_name_dob_key=b.joinkey"
)

length(unique(withdob_results$medicalrecordnumber))

withoutdob_results <- sqldf(
  "SELECT * FROM inv_commercial_FY18 AS a JOIN all_withoutdob_key AS b ON a.inv_name_key=b.joinkey"
)

length(unique(withoutdob_results$medicalrecordnumber))


####Start to implement the logic above in a series of functions
apply_maxadmit_logic <- function(df) {
  #This function is meant to be inserted into the data.table dt[i,j,k] format as the value for 'j' where the function operates on the subquery (.SD)
  #it finds the max admit date for each MRN and adds it as a new column to the dataframe for that MRN
  max_admit <- max(df$admissiondate)
  df$maxadmit <- max_admit
  return(df)
}

filter_bad_results <- function(df, max_admit, interaction_name, grouping_variable) {
  #after running the apply_maxadmit_logic() function, this function does the date comparison for interaction dates and admit dates.
  df$include <- df[[max_admit]] >= df[[interaction_name]]
  #it then filters to include only the records with an interaction date earlier than the admit date
  df = df %>% filter(include)
  #finally, it returns a vector containing the unique medical record numbers of the 'good' results
  filterback <- unique(df[[grouping_variable]])
  return(filterback)
}

#change to data.table
withdob_results <- data.table(withdob_results)
withoutdob_results <- data.table(withoutdob_results)

#set max admit
withdob_results <- withdob_results[,apply_maxadmit_logic(.SD), medicalrecordnumber]
withoutdob_results <- withoutdob_results[,apply_maxadmit_logic(.SD), medicalrecordnumber]

#filter out unwanted records
withdob_results <- filter_bad_results(withdob_results, "maxadmit", "interaction_date", "medicalrecordnumber")
withoutdob_results <- filter_bad_results(withoutdob_results, "maxadmit", "interaction_date", "medicalrecordnumber")

all_results <- unique(c(withdob_results, withoutdob_results))

inv_commercial_FY18_results <- inv_commercial_FY18 %>% filter(medicalrecordnumber %in% all_results)
length(unique(inv_commercial_FY18_results$medicalrecordnumber))
length(unique(inv_commercial_FY18$medicalrecordnumber))

#########################======Notes, scratch work, and validation======#####





