### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

require(plyr)
require(dplyr)

setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/emmiengage"))
emmiengage <- read.table("emmiengage_20180907.txt", comment.char = "", quote = "", fill=T, sep="\t")

nm <- c("loadtime", "textbox2", "roll_up_name", "admin_grp_nm", "mrn", "patient_last_name", "patient_first_name",
  "parent_access_code", "access_code",  "phone_number", "email_address", "doctor_first_name", "doctor_last_name",
  "patient_dob", "program", "date_issued", "date_started", "date_completed", "view_by_date", "user_name",
  "location", "site_ordr_id", "max_section_viewed", "total_sections", "total_views",  "id__of_times_started",
    "id__of_times_completed",  "friends___family_starts",  "textbox32",  "emmi_type_code",  "percent_complete")

names(emmiengage) <- nm

emmiengage$identifier <- paste(emmiengage$patient_first_name, emmiengage$patient_last_name, emmiengage$patient_dob)

step1 <- emmiengage %>% group_by(identifier, date_issued) %>% tally()

step2 <- ddply(step1, ~ identifier, summarize, min(date_issued))

#check results
step2 %>% summarise(n_distinct(identifier))

names(step2) <- c("identifier", "date_issued")

step2$date_issued <- as.Date(as.character(step2$date_issued), format= "%Y%m%d")

step3 <- step2 %>% group_by(date_issued) %>% tally()

step4 <- merge(step3, date_table, by.x="date_issued", by.y="day")

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/emmiengage"))
write.csv(step4, "emmiengage_counts.csv", row.names=F)
