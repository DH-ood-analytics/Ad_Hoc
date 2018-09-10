### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

require(dplyr)
require(gpplot2)

#grab the date_table
setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

#datadir
setwd(paste0("C:/Users/", username ,"/Box Sync/github_local/Ad hoc/MOR/input/google_analytics/raw"))

#read in data
fad_raw_2b <- read.csv("number_of_fad_searches.csv", stringsAsFactors = F)
profview_raw_2c <- read.csv("number_of_provider_profile_views.csv", stringsAsFactors = F)
fad_users_day <- read.csv("users_with_fad_search_day.csv", stringsAsFactors = F)
fad_users_month <- read.csv("users_with_fad_search_month.csv", stringsAsFactors = F)
fad_users_quarter <- read.csv("users_with_fad_search_quarter.csv", stringsAsFactors = F)
fad_users_cyyear <- read.csv("users_with_fad_search_cyyear.csv", stringsAsFactors = F)

#rename BQ user counts
names(fad_users_day) <- c("date", "user_count")
names(fad_users_month) <- c("month", "user_count")
names(fad_users_quarter) <- c("cy_quarter", "user_count")
names(fad_users_cyyear) <- c("cy_year", "user_count")

#create date field
fad_raw_2b$date <- as.Date(as.character(fad_raw_2b$date), format ="%Y%m%d")
profview_raw_2c$date <- as.Date(as.character(profview_raw_2c$date), format ="%Y%m%d")
fad_users_day$date <- as.Date(as.character(fad_users_day$date), format = "%Y%m%d")
fad_users_month$month <- as.Date(as.character(fad_users_month$month), format = "%Y%m%d")

#minor adjustment to date_table
date_table$year_qrtr <- paste(date_table$year, paste0("0",date_table$qrtr), sep = "-")

#merge metrics and rename
fad_raw_2b$fad_searches <- fad_raw_2b$metric20 + fad_raw_2b$metric91
profview_raw_2c$profile_views <- profview_raw_2c$metric27 + profview_raw_2c$metric97

#remove old metrics
fad_raw_2b$metric20 <- NULL; fad_raw_2b$metric91 <- NULL
profview_raw_2c$metric27 <- NULL; profview_raw_2c$metric97 <- NULL

#merge date table
fad_clean_2b <- merge(fad_raw_2b, date_table, by.x="date", by.y="day", all.x=T)
profview_clean_2c <- merge(profview_raw_2c, date_table, by.x="date", by.y="day", all.x=T)
fad_users_day_clean <- merge(fad_users_day, date_table, by.x = "date", by.y = "day", all.x=T)
fad_users_month_clean <- merge(fad_users_month, date_table, by.x = "month", by.y = "day", all.x=T)
fad_users_quarter_clean <- merge(fad_users_quarter, unique(subset(date_table, select = c("year_qrtr", "year", "qrtr", "fy_year", "fy_qrtr"))), by.x="cy_quarter", by.y = "year_qrtr", all.x=F)

#create curves

#fad
ggplot(fad_clean_2b, aes(date, fad_searches)) + geom_smooth(method = "auto") + geom_line(alpha = 0.25) + ggtitle("FAD Searches") + xlab("Date") + ylab("Profile Views") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)
#profile views
ggplot(profview_clean_2c, aes(date, profile_views)) + geom_smooth(method = "auto") + geom_line(alpha = 0.25) + ggtitle("Profile Views") + xlab("Date") + ylab("Profile Views") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels=comma)

#write out the results
setwd(paste0("C:/Users/", username ,"/Box Sync/github_local/Ad hoc/MOR/input/cleaned"))
#write.csv(fad_clean_2b, "fad_searches.csv", row.names=F)
#write.csv(profview_clean_2c, "profile_views.csv", row.names=F)
#write.csv(fad_users_day_clean, "fad_users_day.csv", row.names=F)
#write.csv(fad_users_month_clean, "fad_users_month.csv", row.names=F)
#write.csv(fad_users_quarter_clean, "fad_users_quarter.csv", row.names=F)
#write.csv(fad_users_cyyear, "fad_users_cyyear.csv", row.names=F)

