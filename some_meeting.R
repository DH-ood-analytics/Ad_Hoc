username <- "tvickers"

require(dplyr)
require(readxl)
require(stringr)
require(sqldf)

numextract <- function(string) {
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

desperation_function <- function(df1, df2=NULL, df3=NULL, df1_filtstart=NULL, df1_filtend=NULL, df2_filtstart=NULL, df2_filtend=NULL,df3_filtstart=NULL, df3_filtend=NULL) {
df1 <- df1 %>% filter(yyyy_mm_dd>=df1_filtstart & yyyy_mm_dd <= df1_filtend)
df2 <- df2 %>% filter(yyyy_mm_dd>=df1_filtstart & yyyy_mm_dd <= df1_filtend)
df3 <- df3 %>% filter(yyyy_mm_dd>=df1_filtstart & yyyy_mm_dd <= df1_filtend)
list_return <- list()
list_return[[1]] <- df1
list_return[[2]] <- df2
list_return[[3]] <- df3
return(list_return)
}

eh <- desperation_function(df1 = new_comm_df_withdate, df1_filtstart = "2018-06-01", df1_filtend = "2018-06-01",
                           df2 = pcntdob, df2_filtstart = "2018-06-01", df2_filtend = "2018-06-01",
                           df3 = pcntnodob, df3_filtstart = "2018-06-01", df3_filtend = "2018-06-01")


next_desperation_function <- function(list_input) {
  patient_pop <- list_input[[1]]
  prod_dob <- list_input[[2]]
  
}


setwd(paste0("C:/Users/", username, "/Documents/GitRepositories/Utility_Scripts"))
source("generateDateTable.R")

setwd("C:/Users/tvickers/Box Sync/github_local/Ad hoc/some_meeting")
dhome_from_iq <- read.table("dhomeid_from_iq.txt", sep="\t", quote="", fill=T, comment.char = "", stringsAsFactors = F)
names(dhome_from_iq) <- c("firstname", "lastname", "clientid", "dhomeid")

dhomes <- dhome_from_iq %>% filter(!(dhomeid=="" | dhomeid=="NULL"))

clientids <- dhome_from_iq %>% filter(!(clientid=='""' | clientid=="" | clientid=="NULL" |clientid=="undefined"|clientid=="error with ga"))
length(unique(clientids$clientid))

setwd("C:/Users/tvickers/Box Sync/github_local/Ad hoc/some_meeting")
prod_ciam <- read_excel("CIAMProdData.xlsx", sheet="Sheet1")
prod_ciam$dateofbirth <- as.Date(prod_ciam$dateofbirth)
prod_ciam$personid <- tolower(prod_ciam$personid)
prod_ciam$firstname <- tolower(prod_ciam$firstname)
prod_ciam$lastname <- tolower(prod_ciam$lastname)
prod_ciam$fullname <- paste(prod_ciam$firstname, prod_ciam$lastname, sep = " ")
prod_ciam$istest <- grepl("albers|test|eimer.org|geoff warren|nichole mccloud|meredith mcneill|prod725|prod-ilir-5-3 prod|asdfasdf|hello there|foo bar|meredithprod2|demo amwell|my home demo|tawnya prod 0706 v2|tawnya prod 14|tawnya prod created 5/3|r12 tawnya prod account|tawnya prod created 7/6|tawnya infantino - hotmail2|tawnya prod 3", prod_ciam$fullname)

setwd("C:/Users/tvickers/Box Sync/github_local/Ad hoc/Consumer_Patient_Engagement/archive")

new_com_df_names <- c("Charges_Units", "Charges_Frequency", "Charges_ChargeAmount", "ChargesWorkRVUBase", "Charges_TotalAdjustedRVUBase", "ChargeInformation_InvoiceNumber",
  "ChargeInformation_TransactionSequence", "ChargeInformation_VisitCategory", "ChargeInformation_IsOriginalInvoice", "ChargeInformation_IsChargeCorrectedInvoice",
  "ChargeInformation_Modifier1", "ChargeInformation_Modifier2", "ChargeInformation_Modifier3", "ChargeInformation_TesBatchNumber", "Groups_GroupName",
  "Groups_GroupNumber", "PaymentCode_PaymentCodeName", "BillingProvider_ProviderNumber", "BillingProvider_ProviderName", "RenderingProvider_ProviderNumber",
  "RenderingProvider_ProviderName", "Division_DivisionName", "BillingLocation_LocationName", "BillingLocation_PlaceOfService", "BillingArea_BillingAreaName",
  "ReferringPhysician_ReferringProviderName",	"ReferringPhysician_ReferringProvideNumber", "ServiceDate_Date", "PostDate_Date", "PostPeriod_Date", "Patient_PatientName",
  "Patient_DateOfBirth", "Patient_MRN", "Patient_Gender",	"OriginalFSC_FinancialClassName", "OriginalFSC_FSCClass", "OriginalFSC_FSCCategory", "OriginalFSC_FSCNumber",
  "Diagnosis_DiagnosisDescription", "Diagnosis_DiagnosisCode", "Diagnosis_DiagnosisVersion", "ProcedureCodes_ProcedureCode", "ProcedureCodes_ProcedureName",
  "name_dob_key",	"name_key")

new_comm_df <- read.csv("new_com_df.csv", stringsAsFactors = F, header=F)
new_comm_df <- new_comm_df[2:NROW(new_comm_df),]
names(new_comm_df) <- tolower(new_com_df_names)

est_com_df_names <- c("Charges_Units", "Charges_Frequency", "Charges_ChargeAmount", "ChargesWorkRVUBase", "Charges_TotalAdjustedRVUBase", "ChargeInformation_InvoiceNumber",
                      "ChargeInformation_TransactionSequence", "ChargeInformation_VisitCategory", "ChargeInformation_IsOriginalInvoice", "ChargeInformation_IsChargeCorrectedInvoice",
                      "ChargeInformation_Modifier1", "ChargeInformation_Modifier2", "ChargeInformation_Modifier3", "ChargeInformation_TesBatchNumber", "Groups_GroupName",
                      "Groups_GroupNumber", "PaymentCode_PaymentCodeName", "BillingProvider_ProviderNumber", "BillingProvider_ProviderName", "RenderingProvider_ProviderNumber",
                      "RenderingProvider_ProviderName", "Division_DivisionName", "BillingLocation_LocationName", "BillingLocation_PlaceOfService", "BillingArea_BillingAreaName",
                      "ReferringPhysician_ReferringProviderName",	"ReferringPhysician_ReferringProvideNumber", "ServiceDate_Date", "PostDate_Date", "PostPeriod_Date", "Patient_PatientName",
                      "Patient_DateOfBirth", "Patient_MRN", "Patient_Gender",	"OriginalFSC_FinancialClassName", "OriginalFSC_FSCClass", "OriginalFSC_FSCCategory", "OriginalFSC_FSCNumber",
                      "Diagnosis_DiagnosisDescription", "Diagnosis_DiagnosisCode", "Diagnosis_DiagnosisVersion", "ProcedureCodes_ProcedureCode", "ProcedureCodes_ProcedureName")

est_comm_df <- read.csv("exs_com_df.csv", stringsAsFactors = F, header=F)
est_comm_df <- est_comm_df[2:NROW(est_comm_df),]
names(est_comm_df) <- tolower(est_com_df_names)

keys <- read.csv("exs_keys.csv", stringsAsFactors = F)
est_comm_df <- cbind(est_comm_df, keys)

all_comm_df <- rbind(new_comm_df, est_comm_df)

new_comm_df$service_date <- as.Date(strftime(new_comm_df$servicedate_date))
new_comm_df_withdate <- merge(new_comm_df, date_table, by.x="service_date", by.y = "day", all.x=T)

all_comm_df$service_date <- as.Date(strftime(all_comm_df$servicedate_date))
all_comm_df_withdate <- merge(all_comm_df, date_table, by.x="service_date", by.y = "day", all.x=T)

#prep join work

prod_ciam_nt <- prod_ciam %>% filter(!istest)

prod_ciam_nt_withdob <- prod_ciam_nt %>% filter(has_dob==1)
prod_ciam_nt_withdob$dateofbirth <- format(prod_ciam_nt_withdob$dateofbirth, "%Y%m%d")
prod_ciam_nt_withdob$ciam_name_dob_key <- paste0(prod_ciam_nt_withdob$firstname, prod_ciam_nt_withdob$lastname, prod_ciam_nt_withdob$dateofbirth)
prod_ciam_nt_withdob_keyonly <- data.frame("ciam_name_dob_key" = prod_ciam_nt_withdob$ciam_name_dob_key)
prod_ciam_nt_withdob_keyonly <- unique(prod_ciam_nt_withdob_keyonly)

prod_ciam_nt_withoutdob <- prod_ciam_nt %>% filter(has_dob==0)
prod_ciam_nt_withoutdob$ciam_name_key <- paste0(prod_ciam_nt_withoutdob$firstname, prod_ciam_nt_withoutdob$lastname)


prod_ciam_nt_withoutdob_keyonly <- data.frame("ciam_name_key" = prod_ciam_nt_withoutdob$ciam_name_key)
prod_ciam_nt_withoutdob_keyonly <- unique(prod_ciam_nt_withoutdob_keyonly)

#do join stuff
runone <- sqldf(
 "SELECT * FROM new_comm_df AS a JOIN prod_ciam_nt_withdob_keyonly AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

runtwo <- sqldf(
  "SELECT * FROM new_comm_df AS a JOIN prod_ciam_nt_withoutdob_keyonly AS b ON a.name_key=b.ciam_name_key"
)

names(runone)[names(runone) %in% "ciam_name_dob_key"] <- "joined_key"
names(runtwo)[names(runtwo) %in% "ciam_name_key"] <- "joined_key"

all_run <- rbind(runone, runtwo)

ciam_mrns <- data.frame("ciam_mrns" = unique(all_run$patient_mrn))
setwd("C:/Users/tvickers/Box Sync/github_local/Ad hoc/some_meeting")
write.csv(ciam_mrns, "ciam_mrns.csv", row.names=F)


#begin ga join
setwd("C:/Users/tvickers/Box Sync/github_local/Ad hoc/some_meeting")
dhome_ids_ga <- read.csv("dhome_ids_ga.csv", stringsAsFactors = F)
dhome_ids_ga$date <- as.Date(as.character(dhome_ids_ga$date), format = "%Y%m%d")

length(unique(dhome_ids_ga$value))

#check join integrity
unique_prod_ciam_id <- unique(prod_ciam$personid)
unique_ga_id <- unique(dhome_ids_ga$value)
table(unique_ga_id %in% unique_prod_ciam_id)
misses <- unique_ga_id[!unique_ga_id %in% unique_prod_ciam_id] # 1399 dhomeIDS were in GA but not in CIAM.

#GA total unique: 28026
#CIAM total unique: 31768
#GA ids in CIAM: 26627
#GA ids not in CIAM: 1399

prod_ciam_withInteractionDate <- sqldf(
  "SELECT * FROM dhome_ids_ga AS a JOIN prod_ciam AS b ON a.value=b.personid"
)

#prep join work

prod_ciam_withInteractionDate$dateofbirth <- as.Date(prod_ciam_withInteractionDate$dateofbirth)
prod_ciam_withInteractionDate$firstname <- tolower(prod_ciam_withInteractionDate$firstname)
prod_ciam_withInteractionDate$lastname <- tolower(prod_ciam_withInteractionDate$lastname)
prod_ciam_withInteractionDate$fullname <- paste(prod_ciam_withInteractionDate$firstname, prod_ciam_withInteractionDate$lastname, sep = " ")
prod_ciam_withInteractionDate$istest <- grepl("albers|test|eimer.org|geoff warren|nichole mccloud|meredith mcneill|prod725|prod-ilir-5-3 prod|asdfasdf|hello there|foo bar|meredithprod2", prod_ciam_withInteractionDate$fullname)

prod_ciam_nt <- prod_ciam_withInteractionDate %>% filter(!istest)

prod_ciam_nt_withdob <- prod_ciam_nt %>% filter(has_dob==1)
prod_ciam_nt_withdob$dateofbirth <- format(prod_ciam_nt_withdob$dateofbirth, "%Y%m%d")
prod_ciam_nt_withdob$ciam_name_dob_key <- paste0(prod_ciam_nt_withdob$firstname, prod_ciam_nt_withdob$lastname, prod_ciam_nt_withdob$dateofbirth)
prod_ciam_nt_withdob_key <- data.frame("ciam_name_dob_key" = prod_ciam_nt_withdob$ciam_name_dob_key, "date" = prod_ciam_nt_withdob$date)
prod_ciam_nt_withdob_key <- unique(prod_ciam_nt_withdob_key)
prod_ciam_nt_withdob_key <- merge(prod_ciam_nt_withdob_key, date_table, by.x="date", by.y="day", all.x=T)

prod_ciam_nt_withoutdob <- prod_ciam_nt %>% filter(has_dob==0)
prod_ciam_nt_withoutdob$ciam_name_key <- paste0(prod_ciam_nt_withoutdob$firstname, prod_ciam_nt_withoutdob$lastname)
prod_ciam_nt_withoutdob_key <- data.frame("ciam_name_key" = prod_ciam_nt_withoutdob$ciam_name_key, "date" = prod_ciam_nt_withoutdob$date)
prod_ciam_nt_withoutdob_key <- unique(prod_ciam_nt_withoutdob_key)
prod_ciam_nt_withoutdob_key <- merge(prod_ciam_nt_withoutdob_key, date_table, by.x="date", by.y="day", all.x=T)

ncomm_20170901 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2017-09-01")
pcntdob_20170901 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2017-09-01")
pcntnodob_20170901 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2017-09-01")

ncomm_20171001 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2017-10-01")
pcntdob_20171001 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2017-10-01")
pcntnodob_20171001 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2017-10-01")

ncomm_20171101 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2017-11-01")
pcntdob_20171101 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2017-11-01")
pcntnodob_20171101 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2017-11-01")

ncomm_20171201 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2017-12-01")
pcntdob_20171201 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2017-12-01")
pcntnodob_20171201 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2017-12-01")

ncomm_20180101 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-01-01")
pcntdob_20180101 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-01-01")
pcntnodob_20180101 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-01-01")

ncomm_20180201 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-02-01")
pcntdob_20180201 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-02-01")
pcntnodob_20180201 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-02-01")

ncomm_20180301 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-03-01")
pcntdob_20180301 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-03-01")
pcntnodob_20180301 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-03-01")

ncomm_20180401 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-04-01")
pcntdob_20180401 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-04-01")
pcntnodob_20180401 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-04-01")

ncomm_20180501 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-05-01")
pcntdob_20180501 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-05-01")
pcntnodob_20180501 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-05-01")

ncomm_20180601 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-06-01")
pcntdob_20180601 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-06-01")
pcntnodob_20180601 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-06-01")

allcomm_20180701 <- all_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-07-01")
pcntdob_20180701 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-07-01")
pcntnodob_20180701 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-07-01")

allcomm_20180801 <- all_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-08-01")
pcntdob_20180801 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-08-01")
pcntnodob_20180801 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-08-01")

ncomm_20180901 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-09-01")
pcntdob_20180901 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd=="2018-09-01")
pcntnodob_20180901 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd=="2018-09-01")

ncomm_allyear <- new_comm_df_withdate %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-30")


pcntdob_allyear <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd>="2017-09-01" & yyyy_mm_dd <= "2018-08-01")
pcntnodob_allyear <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd>="2017-09-01" & yyyy_mm_dd <= "2018-08-01")

ncomm_FY18 <- new_comm_df_withdate %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")
pcntdob <- prod_ciam_nt_withdob_key
pcntnodob <- prod_ciam_nt_withoutdob_key

ncomm_Aug18 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-08-01")
pcntdob <- prod_ciam_nt_withdob_key
pcntnodob <- prod_ciam_nt_withoutdob_key

ncomm_Jul18 <- new_comm_df_withdate %>% filter(yyyy_mm_dd=="2018-07-01")
pcntdob <- prod_ciam_nt_withdob_key
pcntnodob <- prod_ciam_nt_withoutdob_key

allcomm_FY18 <- all_comm_df_withdate %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")
pcntdob_FY18 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")
pcntnodob_FY18 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")

allcomm_all <- all_comm_df_withdate
pcntdob_FY18 <- prod_ciam_nt_withdob_key %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")
pcntnodob_FY18 <- prod_ciam_nt_withoutdob_key %>% filter(yyyy_mm_dd>="2017-07-01" & yyyy_mm_dd <= "2018-06-01")

#do join stuff
r1_ncommFY18 <- sqldf(
  "SELECT * FROM ncomm_FY18 AS a JOIN pcntdob AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_ncommFY18 <- sqldf(
  "SELECT * FROM ncomm_FY18 AS a JOIN pcntnodob AS b ON a.name_key=b.ciam_name_key"
)
names(r1_ncommFY18)[names(r1_ncommFY18) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_ncommFY18)[names(r2_ncommFY18) %in% "ciam_name_key"] <- "joined_key"

r_ncommFY18_prodALL <- rbind(r1_ncommFY18, r2_ncommFY18)
#

r1_ncommAug18 <- sqldf(
  "SELECT * FROM ncomm_Aug18 AS a JOIN pcntdob AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_ncommAug18 <- sqldf(
  "SELECT * FROM ncomm_Aug18 AS a JOIN pcntnodob AS b ON a.name_key=b.ciam_name_key"
)
names(r1_ncommAug18)[names(r1_ncommAug18) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_ncommAug18)[names(r2_ncommAug18) %in% "ciam_name_key"] <- "joined_key"

r_ncommAug18_prodALL <- rbind(r1_ncommAug18, r2_ncommAug18)
#

r1_ncommJul18 <- sqldf(
  "SELECT * FROM ncomm_Jul18 AS a JOIN pcntdob AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_ncommJul18 <- sqldf(
  "SELECT * FROM ncomm_Jul18 AS a JOIN pcntnodob AS b ON a.name_key=b.ciam_name_key"
)
names(r1_ncommJul18)[names(r1_ncommJul18) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_ncommJul18)[names(r2_ncommJul18) %in% "ciam_name_key"] <- "joined_key"

r_ncommJul18_prodALL <- rbind(r1_ncommJul18, r2_ncommJul18)
#


r1_all <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntdob_FY18 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_all <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntnodob_FY18 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_all)[names(r1_all) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_all)[names(r2_all) %in% "ciam_name_key"] <- "joined_key"

r_all <- rbind(r1_all, r2_all)
#

r1_all_Jul18 <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntdob_20180701 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_all_Jul18 <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntnodob_20180701 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_all_Jul18)[names(r1_all_Jul18) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_all_Jul18)[names(r2_all_Jul18) %in% "ciam_name_key"] <- "joined_key"

r_all_Jul18 <- rbind(r1_all_Jul18, r2_all_Jul18)
#

r1_all_Aug18 <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntdob_20180801 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_all_Aug18 <- sqldf(
  "SELECT * FROM allcomm_all AS a JOIN pcntnodob_20180801 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_all_Aug18)[names(r1_all_Aug18) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_all_Aug18)[names(r2_all_Aug18) %in% "ciam_name_key"] <- "joined_key"

r_all_Aug18 <- rbind(r1_all_Aug18, r2_all_Aug18)
#

r1_20170901 <- sqldf(
  "SELECT * FROM ncomm_20170901 AS a JOIN pcntdob_20170901 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20170901 <- sqldf(
  "SELECT * FROM ncomm_20170901 AS a JOIN pcntnodob_20170901 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20170901)[names(r1_20170901) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20170901)[names(r2_20170901) %in% "ciam_name_key"] <- "joined_key"

r_all_20170901 <- rbind(r1_20170901, r2_20170901)
#

r1_20171001 <- sqldf(
  "SELECT * FROM ncomm_20171001 AS a JOIN pcntdob_20171001 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20171001 <- sqldf(
  "SELECT * FROM ncomm_20171001 AS a JOIN pcntnodob_20171001 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20171001)[names(r1_20171001) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20171001)[names(r2_20171001) %in% "ciam_name_key"] <- "joined_key"

r_all_20171001 <- rbind(r1_20171001, r2_20171001)
#

r1_20171101 <- sqldf(
  "SELECT * FROM ncomm_20171101 AS a JOIN pcntdob_20171101 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20171101 <- sqldf(
  "SELECT * FROM ncomm_20171101 AS a JOIN pcntnodob_20171101 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20171101)[names(r1_20171101) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20171101)[names(r2_20171101) %in% "ciam_name_key"] <- "joined_key"

r_all_20171101 <- rbind(r1_20171101, r2_20171101)
#

r1_20171201 <- sqldf(
  "SELECT * FROM ncomm_20171201 AS a JOIN pcntdob_20171201 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20171201 <- sqldf(
  "SELECT * FROM ncomm_20171201 AS a JOIN pcntnodob_20171201 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20171201)[names(r1_20171201) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20171201)[names(r2_20171201) %in% "ciam_name_key"] <- "joined_key"

r_all_20171201 <- rbind(r1_20171201, r2_20171201)
#

r1_20180101 <- sqldf(
  "SELECT * FROM ncomm_20180101 AS a JOIN pcntdob_20180101 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180101 <- sqldf(
  "SELECT * FROM ncomm_20180101 AS a JOIN pcntnodob_20180101 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180101)[names(r1_20180101) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180101)[names(r2_20180101) %in% "ciam_name_key"] <- "joined_key"

r_all_20180101 <- rbind(r1_20180101, r2_20180101)
#

r1_20180201 <- sqldf(
  "SELECT * FROM ncomm_20180201 AS a JOIN pcntdob_20180201 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180201 <- sqldf(
  "SELECT * FROM ncomm_20180201 AS a JOIN pcntnodob_20180201 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180201)[names(r1_20180201) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180201)[names(r2_20180201) %in% "ciam_name_key"] <- "joined_key"

r_all_20180201 <- rbind(r1_20180201, r2_20180201)
#

r1_20180301 <- sqldf(
  "SELECT * FROM ncomm_20180301 AS a JOIN pcntdob_20180301 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180301 <- sqldf(
  "SELECT * FROM ncomm_20180301 AS a JOIN pcntnodob_20180301 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180301)[names(r1_20180301) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180301)[names(r2_20180301) %in% "ciam_name_key"] <- "joined_key"

r_all_20180301 <- rbind(r1_20180301, r2_20180301)
#

r1_20180401 <- sqldf(
  "SELECT * FROM ncomm_20180401 AS a JOIN pcntdob_20180401 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180401 <- sqldf(
  "SELECT * FROM ncomm_20180401 AS a JOIN pcntnodob_20180401 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180401)[names(r1_20180401) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180401)[names(r2_20180401) %in% "ciam_name_key"] <- "joined_key"

r_all_20180401 <- rbind(r1_20180401, r2_20180401)
#

r1_20180501 <- sqldf(
  "SELECT * FROM ncomm_20180501 AS a JOIN pcntdob_20180501 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180501 <- sqldf(
  "SELECT * FROM ncomm_20180501 AS a JOIN pcntnodob_20180501 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180501)[names(r1_20180501) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180501)[names(r2_20180501) %in% "ciam_name_key"] <- "joined_key"

r_all_20180501 <- rbind(r1_20180501, r2_20180501)
#

r1_20180601 <- sqldf(
  "SELECT * FROM ncomm_20180601 AS a JOIN pcntdob_20180601 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180601 <- sqldf(
  "SELECT * FROM ncomm_20180601 AS a JOIN pcntnodob_20180601 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180601)[names(r1_20180601) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180601)[names(r2_20180601) %in% "ciam_name_key"] <- "joined_key"

r_all_20180601 <- rbind(r1_20180601, r2_20180601)
#

r1_20180701 <- sqldf(
  "SELECT * FROM allcomm_20180701 AS a JOIN pcntdob_20180701 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180701 <- sqldf(
  "SELECT * FROM allcomm_20180701 AS a JOIN pcntnodob_20180701 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180701)[names(r1_20180701) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180701)[names(r2_20180701) %in% "ciam_name_key"] <- "joined_key"

r_all_20180701 <- rbind(r1_20180701, r2_20180701)
#

r1_20180801 <- sqldf(
  "SELECT * FROM allcomm_20180801 AS a JOIN pcntdob_20180801 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180801 <- sqldf(
  "SELECT * FROM allcomm_20180801 AS a JOIN pcntnodob_20180801 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180801)[names(r1_20180801) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180801)[names(r2_20180801) %in% "ciam_name_key"] <- "joined_key"

r_all_20180801 <- rbind(r1_20180801, r2_20180801)
#

r1_20180901 <- sqldf(
  "SELECT * FROM ncomm_20180901 AS a JOIN pcntdob_20180901 AS b ON a.name_dob_key=b.ciam_name_dob_key"
)

r2_20180901 <- sqldf(
  "SELECT * FROM ncomm_20180901 AS a JOIN pcntnodob_20180901 AS b ON a.name_key=b.ciam_name_key"
)
names(r1_20180901)[names(r1_20180901) %in% "ciam_name_dob_key"] <- "joined_key"
names(r2_20180901)[names(r2_20180901) %in% "ciam_name_key"] <- "joined_key"

r_all_20180901 <- rbind(r1_20180901, r2_20180901)
#

###As of 9/20/2018, John and I decided that patient_mrn should be the metric we use to represent an individual. Eg, it is what we will tally.
# counting 'joined_key' underrepresents (could be more than one mariagarcia) while MRN overrepresents (one mariagarcia could expand into 47
# mariagarcias)
unique_ncommFY_prodALL <- length(unique(r_ncommFY18_prodALL$patient_mrn))
unique_ncommAug18_prodALL <- length(unique(r_ncommAug18_prodALL$patient_mrn))
unique_ncommJul18_prodALL <- length(unique(r_ncommJul18_prodALL$patient_mrn))
unique_Jul18 <- length(unique(r_all_Jul18$patient_mrn))
unique_Aug18 <- length(unique(r_all_Aug18$patient_mrn))
unique_all <- length(unique(r_all$patient_mrn))
unique_allcomm_FY18 <- length(unique(r_all_FY18$patient_mrn))
unique_allyear <- length(unique(r_all_allyear$joined_key))
unique_20170901 <- length(unique(r_all_20170901$joined_key))
unique_20171001 <- length(unique(r_all_20171001$joined_key))
unique_20171101 <- length(unique(r_all_20171101$joined_key))
unique_20171201 <- length(unique(r_all_20171201$joined_key))
unique_20180101 <- length(unique(r_all_20180101$joined_key))
unique_20180201 <- length(unique(r_all_20180201$joined_key))
unique_20180301 <- length(unique(r_all_20180301$joined_key))
unique_20180401 <- length(unique(r_all_20180401$joined_key))
unique_20180501 <- length(unique(r_all_20180501$joined_key))
unique_20180601 <- length(unique(r_all_20180601$joined_key))
unique_20180701 <- length(unique(r_all_20180701$patient_mrn))
unique_20180801 <- length(unique(r_all_20180801$patient_mrn))
unique_20180901 <- length(unique(r_all_20180901$joined_key))

rslts <- all_run_withdate %>% group_by(yearmon) %>% summarize(n_distinct(joined_key))
write.csv(rslts, "ciam_bymonth_activity.csv", row.names=F)

###key work
testies <- subset(r_all_FY18, select =c(patient_mrn, joined_key))
moretest <- testies %>% group_by(joined_key) %>% summarise(n_distinct(patient_mrn))
testies <- unique(testies$patient_mrn)
#write.csv(testies, "unique_mrns_commercial_FY18.csv", row.names=F)

out_20180701 <- unique(r_all_20180701$patient_mrn)
out_20180801 <- unique(r_all_20180801$patient_mrn)
#write.csv(out_20180701, "unique_mrns_commercial_Jul18.csv", row.names=F)
#write.csv(out_20180801, "unique_mrns_commercial_Aug18.csv", row.names=F)

out_all <- unique(r_all$patient_mrn)
#write.csv(out_all, "unique_mrns_commercial_prodFY18.csv", row.names=F)

out_Aug18 <- unique(r_all_Aug18$patient_mrn)
out_Jul18 <- unique(r_all_Jul18$patient_mrn)
#write.csv(out_Aug18, "unique_mrns_commercial_prodAug18.csv", row.names=F)
#write.csv(out_Jul18, "unique_mrns_commercial_prodJul18.csv", row.names=F)

out_ncommFY18_prodALL <- unique(r_ncommFY18_prodALL$patient_mrn)
out_ncommAug18_prodALL <- unique(r_ncommAug18_prodALL$patient_mrn)
out_ncommJul18_prodALL <- unique(r_ncommJul18_prodALL$patient_mrn)

#write.csv(out_ncommFY18_prodALL, "unique_mrns_ncommFY18_prodALL.csv", row.names=F)
#write.csv(out_ncommAug18_prodALL, "unique_mrns_ncommAug18_prodALL.csv", row.names=F)
#write.csv(out_ncommJul18_prodALL, "unique_mrns_ncommJul18_prodALL.csv", row.names=F)

fn_yo <- gsub(" .*", "", prod_ciam$firstname)

tr2 <- cbind(fn_yo, prod_ciam)

test_run <- data.frame(gsub("[[:punct:] ]+","", prod_ciam_nt_withoutdob_keyonly$ciam_name_key))
test_run <- cbind(test_run, prod_ciam_nt_withoutdob_keyonly, fn_yo)

tr_bids <- data.frame(gsub("[[:punct:] ]+", "", new_comm_df$patient_patientname))

try_review <- new_comm_df_withdate %>% group_by(yyyy_mm_dd) %>% summarise(n(), n_distinct(patient_mrn))
try_review2 <- new_comm_df_withdate %>% summarise(n_distinct(patient_mrn))


yo <- new_comm_df_withdate %>% filter(service_date>="2017-09-01")
range(yo$service_date)
length(unique(yo$patient_mrn))

yo2 <- yo %>% group_by(yyyy_mm_dd) %>% summarise(n_distinct(patient_mrn))
sum(yo2$`n_distinct(patient_mrn)`)
