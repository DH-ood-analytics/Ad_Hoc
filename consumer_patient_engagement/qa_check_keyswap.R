username <- "tvickers"

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/consumerdata_input/myhome"))
old_prod_ciam <- read.csv("ciam_withinteractiondate_rawname.csv", stringsAsFactors = F)

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/consumerdata_input/searchandschedule"))
iq_reg <- read.csv("iq_registrations_20150102_20180917.csv", stringsAsFactors = F)

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/patientdata_input/ms4"))
ms4_test <- read.table("ms4_withnew_20180927.txt", comment.char = "", quote="", fill=T, sep = "\t", stringsAsFactors = F)

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/patientdata_input/inv"))
inv_test <- read.table("inv_withnew_20180927.txt", comment.char = "", quote="", fill=T, sep = "\t", stringsAsFactors = F)

setwd(paste0("C:/Users/", username, "/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/create_keys/Key'd"))
dir()

adtall_names <- c("load_date", "firstname", "lastname", "gender", "dateofbirth", "adtsystemname", "medicalrecordnumber", "accountnumber", "facilitycode", "facilityname",
                  "admissiondate", "dischargedate", "financialclasscode", "financialclassdescription", "patienttype", "patienttypedesc", "servicelinecode", "servicelinedesc",
                  "serviceareacode", "serviceareadesc", "middleinitial", "city", "state", "zip", "email", "phone", "primaryinsurancename", "emailmarketingoptin",
                  "region", "facility","addressline1" ,"addressline2", "patientmaritalstatus", "admissionfinaldate", "primaryfinancialclass", "admissionyear",
                  "admissionmonth", "servicecode", "agefromadmission", "surgicalprocedure1", "surgicalprocedure2", "surgicalprocedure3", "drgcategory", "birthyear",
                  "firstproceduredate", "lastservicedate", "doctornumber", "hospitalservicecode", "providerspecialty", "appointment_booking_id")

VR_names <- c("grouping_operator", "loadtime", "joined_grouping_operator", "previousadmitdate", "currentadmitdate", "nextadmitdate", "yearsfromprev", "yearstonext", "neworreturn")

full_table_names <- c(adtall_names, VR_names)

names(ms4_test) <- full_table_names
names(inv_test) <- full_table_names

inv_keys_redo <- read.csv("inv_keys_df.csv", stringsAsFactors = F)
ms4_keys_redo <- read.csv("ms4_keys_df.csv", stringsAsFactors = F)
iq_keys_redo <- read.csv("iq_keys_df.csv", stringsAsFactors = F)
ciam_keys_redo <- read.csv("myhome_keys_df.csv", stringsAsFactors = F)

full_myhome <- cbind(old_prod_ciam, ciam_keys_redo)
review_myhome <- subset(full_myhome, select =c(firstname, lastname, name_dob_key, name_key))

full_iq <- cbind(iq_reg, iq_keys_redo)
review_iq <- subset(full_iq, select =c(firstname, lastname, name_dob_key, name_key))

full_inv <- cbind(inv_test, inv_keys_redo)
review_inv <- subset(full_inv, select = c(firstname, lastname, name_dob_key, name_key))
  
full_ms4 <- cbind(ms4_test, ms4_keys_redo)
review_ms4 <- subset(full_ms4, select = c(firstname, lastname, name_dob_key, name_key))




