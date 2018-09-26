### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/InQuicker/input_data"))
source("source_inq_local_sharedtest.R")

#grab the date_table
setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

iq_master_withdate <- merge(iq_master, date_table, by.x = "registration_date", by.y = "day", all.x=T)


get_service <- iq_master_withdate %>% group_by(service) %>% tally()

#split Emergency Room
iq_master_er <- iq_master_withdate %>% filter(service=="Emergency Room" | service=="Pediatric Emergency Room")

#split Urgent Care
iq_master_uc <- iq_master_withdate %>% filter(service=="Urgent Care")

#complement of others
iq_master_other <- iq_master_withdate %>% filter(!(service=="Emergency Room" | service=="Pediatric Emergency Room" | service=="Urgent Care"))

##prep the output
iq_er_rslts <- iq_master_er %>% group_by(year, yyyy_mm_dd, month_name, calendar_month_sort, fy_month_sort, qrtr, fy_year, fy_qrtr, registration_date) %>% tally()
iq_uc_rslts <- iq_master_uc %>% group_by(year, yyyy_mm_dd, month_name, calendar_month_sort, fy_month_sort, qrtr, fy_year, fy_qrtr, registration_date) %>% tally()
iq_other_rslts <- iq_master_other %>% group_by(year, yyyy_mm_dd, month_name, calendar_month_sort, fy_month_sort, qrtr, fy_year, fy_qrtr, registration_date) %>% tally()

names(iq_er_rslts)

#prep graphs
iq_er_graph <- iq_master_er %>% group_by(yyyy_mm_dd) %>% tally()
iq_uc_graph <- iq_master_uc %>% group_by(yyyy_mm_dd) %>% tally()
iq_other_graph <- iq_master_other %>% group_by(yyyy_mm_dd) %>% tally()

iq_er_graph$registration_date <- as.Date(iq_er_graph$yyyy_mm_dd)
iq_uc_graph$registration_date <- as.Date(iq_uc_graph$yyyy_mm_dd)
iq_other_graph$registration_date <- as.Date(iq_other_graph$yyyy_mm_dd)

#graphs
ggplot(iq_er_graph, aes(registration_date, n)) + geom_smooth(method = "auto") + geom_line(alpha = 0.25) + ggtitle("Emergency Room") + xlab("Date") + ylab("Schedules") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)
ggplot(iq_uc_graph, aes(registration_date, n)) + geom_smooth(method = "auto") + geom_line(alpha = 0.25) + ggtitle("Urgent Care") + xlab("Date") + ylab("Schedules") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)
ggplot(iq_other_graph, aes(registration_date, n)) + geom_smooth(method = "auto") + geom_line(alpha = 0.25) + ggtitle("Clinics") + xlab("Date") + ylab("Schedules") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)

#write out the results
setwd(paste0("C:/Users/", username ,"/Box Sync/github_local/Ad hoc/MOR/input/inquicker_schedules"))
#write.csv(iq_er_rslts, "iq_er.csv", row.names=F)
#write.csv(iq_uc_rslts, "iq_uc.csv", row.names=F)
#write.csv(iq_other_rslts, "iq_other.csv", row.names=F)
