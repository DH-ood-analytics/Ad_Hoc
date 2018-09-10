### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

require(dplyr)
require(ggplot2)
require(scales)

setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

setwd("C:/Users/tvickers/Box Sync/github_local/ContributionMargin/output/cm_results_20180906")

cm_results <- read.csv("cm_results_postfilt.csv", stringsAsFactors = F)
#There are duplicates in the cm_results table... which is weird. After seeing this, I've fixed it in the General_CM_Script as of 9/06/2018
cm_results <- unique(cm_results)

##surgical insertion of SL_Categorization to avoid having to rerun the CM_results entirely
setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/ContributionMargin/user_defined_functions"))
source("cm_processing.R")
get_SL_lookup()

names(cm_results)[names(cm_results)=="SL_Category"] <- "SL_Category_old"

cm_results <- merge(cm_results, SL_lkp[,c("sl_key", "SL_Category")], by="sl_key")
#### End surgical incision

#check out the prefiltered cm_results table
setwd("C:/Users/tvickers/Box Sync/github_local/ContributionMargin/output/cm_results_20180906")
cm_results_prefilt <- read.csv("cm_results_prefilter_20180906.csv", stringsAsFactors = F)

get_slstuff <- cm_results %>% group_by(SL_Category) %>% tally()

# interesting...
pc <- cm_results %>% group_by(yyyy_mm_dd) %>% filter(SL_Category=="Primary Care") %>% tally()
pc$yyyy_mm_dd <- as.Date(pc$yyyy_mm_dd)
ggplot(pc, aes(x=yyyy_mm_dd, y=n)) + geom_smooth() + geom_line() + ggtitle("Clinic Apointments") + xlab("Admit Date") + ylab("Volume") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)

er <- cm_results %>% group_by(yyyy_mm_dd) %>% filter(SL_Category=="Emergency Room") %>% tally()
er$yyyy_mm_dd <- as.Date(er$yyyy_mm_dd)
ggplot(er, aes(x=yyyy_mm_dd, y=n)) + geom_smooth() + geom_line() + ggtitle("Emergency Room") + xlab("Admit Date") + ylab("Volume") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)

uc <- cm_results %>% group_by(yyyy_mm_dd) %>% filter(SL_Category=="Urgent Care") %>% tally()
uc$yyyy_mm_dd <- as.Date(uc$yyyy_mm_dd)
ggplot(uc, aes(x=yyyy_mm_dd, y=n)) + geom_smooth() + geom_line() + ggtitle("Urgent Care") + xlab("Admit Date") + ylab("Volume") + theme_bw() + theme(plot.title = element_text(hjust=0.5))

fv <- cm_results %>% group_by(yyyy_mm_dd, visit_class, SL_Category) %>% tally()
fv$yyyy_mm_dd <- as.Date(fv$yyyy_mm_dd)
ggplot(fv, aes(x=yyyy_mm_dd, y=n)) + geom_line(aes(color=visit_class)) + facet_wrap(~.SL_Category) + ggtitle("VB") + xlab("Admit Date") + ylab("Volume") + theme_bw() + theme(plot.title = element_text(hjust=0.5))

ggplot(fv, aes(x=yyyy_mm_dd, y=n)) + facet_grid(SL_Category~.) + geom_line(aes(color=visit_class))

#write out the results - Overview Tab
setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/cm_table"))
#write.csv(pc, "primary_care_20180906.csv", row.names=F)
#write.csv(uc, "urgent_care_20180906.csv", row.names=F)
#write.csv(er, "emergency_room_20180906.csv", row.names=F)

#Build the Detail Tab results
cm_results_pc <- cm_results %>% filter(SL_Category=="Primary Care")
cm_results_er <- cm_results %>% filter(SL_Category=="Emergency Room")
cm_results_uc <- cm_results %>% filter(SL_Category=="Urgent Care")

cm_results_pc_byday <- cm_results_pc %>% group_by(adt_admitdate) %>% tally()
cm_results_er_byday <- cm_results_er %>% group_by(adt_admitdate) %>% tally()
cm_results_uc_byday <- cm_results_uc %>% group_by(adt_admitdate) %>% tally()

cm_results_pc_byday$adt_admitdate <- as.Date(cm_results_pc_byday$adt_admitdate)
cm_results_er_byday$adt_admitdate <- as.Date(cm_results_er_byday$adt_admitdate)
cm_results_uc_byday$adt_admitdate <- as.Date(cm_results_uc_byday$adt_admitdate)

cm_results_pc_byday <- merge(cm_results_pc_byday, date_table, by.x="adt_admitdate", by.y="day", all.x=T)
cm_results_er_byday <- merge(cm_results_er_byday, date_table, by.x="adt_admitdate", by.y="day", all.x=T)
cm_results_uc_byday <- merge(cm_results_uc_byday, date_table, by.x="adt_admitdate", by.y="day", all.x=T)

#write out the results - Detail Tab
setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/MOR/input/cm_table"))
#write.csv(cm_results_pc_byday, "cm_results_pc_byday_20180906.csv", row.names=F)
#write.csv(cm_results_er_byday, "cm_results_er_byday_20180906.csv", row.names=F)
#write.csv(cm_results_uc_byday, "cm_results_uc_byday_20180906.csv", row.names=F)

er$year <- year(er$yyyy_mm_dd)

er_2017 <- er %>% filter(year=="2017")

mean(er_2017$n)

list_holder <- list()
for(i in 1:75000) {
list_holder[[i]] <- mean(sample(er_2017$n, 5))
}

eh <- unlist(list_holder)

hist(eh)

rando <- sample(1:10000, 1000, replace = T)

hist(rando)
mean(rando)

list_holder <- list()
for(i in 1:150000) {
  list_holder[[i]] <- mean(sample(rando,100, replace =F))
}
eh <- unlist(list_holder)

hist(eh)

abline(v=mean(rando), col="blue")
