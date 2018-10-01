username <- "tvickers"

require(sqldf)
require(dplyr)

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

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/consumerdata_input/searchandschedule"))
iq_reg <- read.csv("iq_registrations_20150102_20180917.csv", stringsAsFactors = F)

#cast dates
iq_reg$birthdate <- as.Date(iq_reg$birthdate)
iq_reg$registration_date <- as.Date(iq_reg$registration_date)

#format birthdate
iq_reg$birthdate <- format(iq_reg$birthdate, "%Y%m%d")

#clean neames
iq_reg <- cm_clean(iq_reg, c("firstname", "lastname"))

#name key preparation
#1) ignore middile initial fields (no such field)

#2) remove puncuation from FN, LN
iq_reg$firstname <- gsub("\\.", "", iq_reg$firstname) # remove periods
iq_reg$firstname <- gsub("\\,", "", iq_reg$firstname) # remove commas
iq_reg$firstname <- gsub("-", "", iq_reg$firstname) # remove hyphens

iq_reg$lastname <- gsub("\\.", "", iq_reg$lastname) # remove periods
iq_reg$lastname <- gsub("\\,", "", iq_reg$lastname) # remove commas
iq_reg$lastname <- gsub("-", "", iq_reg$lastname) # remove hyphens

#3) collapse remaining whitespace
iq_reg$firstname <- gsub(" ", "", iq_reg$firstname)
iq_reg$lastname <- gsub(" ", "", iq_reg$lastname)

#4) set to lowercase (already done)

#5) trim trailing whitespace (already done)

#create joinkeys

iq_reg$joinkey <- paste0(iq_reg$firstname, iq_reg$lastname, iq_reg$birthdate)

#cut to relevant columns

iq_reg_key <- subset(iq_reg, select =c(registration_date, joinkey))
iq_reg_key <- unique(iq_reg_key)
names(iq_reg_key) <- c("interaction_date", "joinkey")
