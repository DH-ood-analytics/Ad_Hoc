username <- "tvickers"

setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Ad_Hoc/consumer_patient_engagement/consumer_data"))
source("consumer_myhome.R")
setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Ad_Hoc/consumer_patient_engagement/consumer_data"))
source("consumer_searchandschedule.R")

names(withdob_key)
names(withoutdob_key)
names(iq_reg_key)

all_withdob_key <- rbind(withdob_key, iq_reg_key)
all_withoutdob_key <- withoutdob_key
