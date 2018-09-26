username <- "tvickers"

require(readxl)
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

# Load data
setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/consumerdata_input/myhome"))
dhome_ids_ga <- read.csv("dhome_ids_ga.csv", stringsAsFactors = F)

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/consumerdata_input/myhome"))
prod_ciam <- read_excel("CIAMProdData.xlsx", sheet="Sheet1")

#clean data
dhome_ids_ga$date <- as.Date(as.character(dhome_ids_ga$date), format = "%Y%m%d")

prod_ciam$dateofbirth <- as.Date(prod_ciam$dateofbirth)
prod_ciam <- cm_clean(prod_ciam, c("personid", "firstname", "lastname"))

#create fields
prod_ciam$fullname <- paste(prod_ciam$firstname, prod_ciam$lastname, sep = " ")

#add 'istest' field to prod_ciam
testvalues_vector <- c("albers|test|eimer.org|geoff warren|nichole mccloud|meredith mcneill|prod725|prod-ilir-5-3 prod|asdfasdf|hello there|foo bar|meredithprod2|demo amwell|my home demo|tawnya prod 0706 v2|tawnya prod 14|tawnya prod created 5/3|r12 tawnya prod account|tawnya prod created 7/6|tawnya infantino - hotmail2|tawnya prod 3")
prod_ciam$istest <- grepl(testvalues_vector, prod_ciam$fullname)

#remove test records
prod_ciam <- prod_ciam %>% filter(!istest)

# create ciam_prod data with interactions
prod_ciam_withInteractionDate <- sqldf(
  "SELECT * FROM dhome_ids_ga AS a JOIN prod_ciam AS b ON a.value=b.personid"
)

#review join integrity
length(unique(dhome_ids_ga$value))
length(unique(prod_ciam$personid))
table(unique(dhome_ids_ga$value) %in% unique(prod_ciam$personid))

#split ciam into two tables--one with dob, the other without
date_ciam_withdob <- prod_ciam_withInteractionDate %>% filter(has_dob==1)
date_ciam_withoutdob <- prod_ciam_withInteractionDate %>% filter(has_dob==0)

#create keys that join to john's GECB data
date_ciam_withdob$dateofbirth <- format(date_ciam_withdob$dateofbirth, "%Y%m%d")
date_ciam_withoutdob$dateofbirth <- format(date_ciam_withoutdob$dateofbirth, "%Y%m%d")

date_ciam_withdob$ciam_name_dob_key <- paste0(date_ciam_withdob$firstname, date_ciam_withdob$lastname, date_ciam_withdob$dateofbirth)
date_ciam_withoutdob$ciam_name_key <- paste0(date_ciam_withoutdob$firstname, date_ciam_withoutdob$lastname)

withdob_key <- data.frame("ciam_name_dob_key" = date_ciam_withdob$ciam_name_dob_key, "date" = date_ciam_withdob$date)
withoutdob_key <- data.frame("ciam_name_key" = date_ciam_withoutdob$ciam_name_key, "date" = date_ciam_withoutdob$date)

withdob_key <- unique(withdob_key)
withoutdob_key <- unique(withoutdob_key)

withdob_key <- merge(withdob_key, date_table, by.x="date", by.y = "day", all.x = T)
withoutdob_key <- merge(withoutdob_key, date_table, by.x="date", by.y = "day", all.x = T)
