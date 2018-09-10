### GLOBAL VARIABLE ###
username <- Sys.getenv("USERNAME")
#######################

#dependencies
require(RGoogleAnalytics)
library(jsonlite)

#GA Functions
runGAquery <- function(queryinit, tok = token, paginate = F, setdaywise = F) {
  if(setdaywise) {
    queryinit$start.date <- as.Date(queryinit$start.date)+1
    queryinit$end.date <- as.Date(queryinit$end.date)+1
  }
  qry <- QueryBuilder(queryinit)
  qry_data <- GetReportData(qry, tok, paginate_query = paginate, split_daywise = setdaywise)
  return(qry_data)
}

clean.GA.names <- function(df) {
  names(df)[names(df)=="Date"] <- "date"
  names(df)[names(df)=="Users"] <- "users"
  names(df)[names(df)=="Sessions"] <- "sessions"
  names(df)[names(df)=="Device.Category"] <- "deviceCategory"
  names(df)[names(df)=="Source...Medium"] <- "sourceMedium"
  names(df)[names(df)=="Campaign"] <- "campaign"
  names(df)[names(df)=="Default.Channel.Grouping"] <- "channelGrouping"
  names(df)[names(df)=="Pageviews"] <- "pageviews"
  names(df)[names(df)=="Unique.Pageviews"] <- "uniquePageviews"
  names(df)[names(df) %in% c("InQuicker.Service.ID..cd42.","Custom.Dimension.042")] <- "dimension42"
  names(df)[names(df)=="inQuicker.Completes..cm41."] <- "metric41"
  names(df)[names(df)=="Custom.Metric.020.Value"] <- "metric20"
  names(df)[names(df)=="Custom.Metric.091.Value"] <- "metric91"
  names(df)[names(df)=="Custom.Metric.041.Value"] <- "metric41"
  names(df)[names(df)=="Custom.Metric.040.Value"] <- "metric40"
  names(df)[names(df)=="Custom.Metric.042.Value"] <- "metric42"
  names(df)[names(df)=="Custom.Metric.022.Value"] <- "metric22"
  names(df)[names(df)=="Custom.Metric.093.Value"] <- "metric93"
  return(df)
}

#datadir
setwd(paste0("C:/Users/", username ,"/Box Sync/github_local/Ad hoc/MOR"))

httr::set_config( config( ssl_verifypeer = 0L ) )

client.id  <- "1000770094646-g1v17493s7i6vd00f198b80a59dtr0o0.apps.googleusercontent.com"
client.secret <- "KIG9HIM8QH_3R9FkSq-N6djn"

token <- Auth(client.id,client.secret)

save(token,file="./token_file")
load("./token_file")
ValidateToken(token)

all_production <- "ga:106076928"
start_date <- "2015-01-01"
end_date <- as.character(Sys.Date()-1)

#2a - Number of people searching FAD
#Segment ID - gaid::hHnt3B0ZTGWSuUxIZrUEWA
#Segment Name - User Has at Least One FAD Search
#Segment Definition - Filter Users (Include) CM91 per user > 0 OR CM20 per user > 0

## MOVED THIS SECTION TO BIGQUERY. SEE PROJECT QUERIES UNDER THE 'DH-GA-Export' Project titled...
#Count of Users with at Least One FAD Search by day
#Count of Users with at Least One FAD Search by month
#Count of Users with at Least One FAD Search by quarter
#Count of Users with at Least One FAD Search by cy_year


#2b - Number of FAD Searches (cm20 + cm91)

num_fad_srch.init <- Init(start.date = start_date,
                             end.date = end_date,
                             dimensions = c("ga:date"),
                             metrics = c("ga:metric20", "ga:metric91"),
                             max.results = 10000,
                             table.id = all_production)

num_fad_srch.dat <- runGAquery(num_fad_srch.init, setdaywise = T)


#2c - Number of provider profile views (cm27 + cm97)

num_prof_view.init <- Init(start.date = start_date,
                               end.date = end_date,
                               dimensions = c("ga:date"),
                               metrics = c("ga:metric27", "ga:metric97"),
                               max.results = 10000,
                               table.id = all_production)

num_prof_view.dat <- runGAquery(num_prof_view.init, setdaywise = T)

setwd(paste0("C:/Users/", username ,"/Box Sync/github_local/Ad hoc/MOR/input/raw"))

write.csv(num_prof_view.dat, "number_of_provider_profile_views.csv", row.names=F)
write.csv(num_fad_srch.dat, "number_of_fad_searches.csv", row.names=F)

#in support of growth:
  
#number of Digital Membership
#three parameters for Search
#three paramters for Schedule

#3a) number of clinic appointments
#3b) number of UC appointments
#3c) number of ED appointments

















