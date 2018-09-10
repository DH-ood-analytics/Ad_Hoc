### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

require(plyr)
require(dplyr)

setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/docent"))
docent <- read.table("docent_membertable_20180907.txt", comment.char = "", quote = "", fill=T, sep="\t")

nm <- c("a.loadtime", "a.id", "a.completed_at",	"a.episode_type",	"a.name",	"a.patient_id",	"a.referral_source",	"a.state",	"b.loadtime",	"b.id",
  "b.activity_state",	"b.all_day_start_date",	"b.clinical_event_facility_id",	"b.clinical_event_location_id",	"b.clinical_event_status_id",
  "b.completed_at",	"b.csn",	"b.end_time",	"b.event_type_id",	"b.imported_room_number",	"b.instructions",	"b.journey_id",	"b.start_time",	"b.type")

names(docent) <-nm

step1 <- docent %>% group_by(a.id, b.all_day_start_date) %>% tally()

#remove nas
step1.1 <- step1[which(!is.na(step1$b.all_day_start_date)),]

step2 <- ddply(step1.1, ~ a.id, summarize, min(b.all_day_start_date))


names(step2) <- c("patient_id", "all_day_start_date")

step2$all_day_start_date <- as.Date(as.character(step2$all_day_start_date), format= "%Y%m%d")

step3 <- step2 %>% group_by(all_day_start_date) %>% tally()

step4 <- merge(step3, date_table, by.x="all_day_start_date", by.y="day")

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/docent"))
write.csv(step4, "docent_counts.csv", row.names=F)
