
options(warn=-1)

library(data.table)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(rio)

setwd("F:/Thesis/Working_Data/Final")
getwd()

HAN_Dataset <- fread("F:/Thesis/Working_Data/Final\\HAN_Dataset.txt", header=T, sep="|",encoding = "UTF-8")
sapply(HAN_Dataset, class)

WIPO_Patents <- fread("F:/Thesis/Working_Data/Final\\WIPO_Patents.csv",stringsAsFactors = FALSE, na.strings="",encoding = "UTF-8")
sapply(WIPO_Patents, class)

head(HAN_Dataset[Publn_auth=="EP",],1)
head(HAN_Dataset[Publn_auth=="WO",],1)

head(WIPO_Patents[Country=="EP",], 1)
head(WIPO_Patents[Country=="WO",], 1)

WIPO_Patents[, Patent_number := ifelse(WIPO_Patents$Country=="EP", 
                                     paste0("EP", WIPO_Patents$Publication_Number),
                                     WIPO_Patents$Application_Id)]

#Check Last Column
head(WIPO_Patents[Country=="EP",], 1)
head(WIPO_Patents[Country=="WO",], 1)

setkey(WIPO_Patents, Patent_number)
setkey(HAN_Dataset, Patent_number)
Patent_HAN <- HAN_Dataset[WIPO_Patents]

head(Patent_HAN,5)

# of Patents without HAN identifier
sum(is.na(Patent_HAN$HAN_ID)) 

#By year
Year_NO_HAN <- table(year(Patent_HAN[is.na(Patent_HAN$HAN_ID),Application_Date]))
Year_NO_HAN

#Share of 2019 from all years
Year_NO_HAN[34]/sum(Year_NO_HAN)

Year_Patents <- table(year(Patent_HAN[!is.na(Patent_HAN$HAN_ID),Application_Date]))
Year_Patents

Patent_HAN <- Patent_HAN[year(Application_Date)<2019,]
Year_Patents <- table(year(Patent_HAN[!is.na(Patent_HAN$HAN_ID),Application_Date]))

Year_Patents

DropFeatures2 <- c("Application_Id","Application_Number", "Publication_Number", "Publication_Date",
                   "HARM_ID", "Appln_id", "Publn_auth")
Patent_HAN <- select(Patent_HAN, -!!DropFeatures2)

n_rows_HAN <- nrow(Patent_HAN)
dim(Patent_HAN)

#Identify Top 20 Countries
sort(table(Patent_HAN$Person_ctry_code)) 

#Subsitute Country code with Country name
Patent_HAN <- Patent_HAN[, Person_ctry_code := Person_ctry_code %>%
                               gsub("^US", "UNITED STATES",.) %>%
                               gsub("^JP", "JAPAN",.) %>%
                               gsub("^GB", "UNITED KINGDOM",.) %>%
                               gsub("^DE", "GERMANY",.) %>%
                               gsub("^CH", "SWITZERLAND",.) %>%
                               gsub("^CN", "CHINA",.) %>%
                               gsub("^CA", "CANADA",.) %>%
                               gsub("^NL", "NETHERLANDS",.) %>%
                               gsub("^IL", "ISRAEL",.) %>%
                               gsub("^KR", "SOUTH KOREA",.) %>%
                               gsub("^FR", "FRANCE",.) %>%
                               gsub("^AU", "AUSTRALIA",.) %>%
                               gsub("^FI", "FINLAND",.) %>%
                               gsub("^IT", "ITALY",.) %>%
                               gsub("^SE", "SWEDEN",.) %>%
                               gsub("^IN", "INDIA",.) %>%
                               gsub("^BE", "BELGIUM",.) %>%
                               gsub("^SG", "SINGAPORE",.) %>%
                               gsub("^ES", "SPAIN",.) %>%
                               gsub("^DK", "DENMARK",.)]

unique(Patent_HAN$Person_ctry_code)

#Select only patent applications related to top countries

Top_Countries <- c("UNITED STATES", "JAPAN","UNITED KINGDOM", "GERMANY", "CHINA", "CANADA", "NETHERLANDS",
                   "ISRAEL", "SOUTH KOREA", "FRANCE", "SWITZERLAND", "AUSTRALIA", "FINLAND", "ITALY",
                   "SWEDEN", "INDIA", "BELGIUM", "SINGAPORE", "SPAIN", "DENMARK")

Patent_HAN <- Patent_HAN[Person_ctry_code %in% Top_Countries,]

#Number of remaining patent applications
n_rows_HAN2 <- nrow(Patent_HAN)
n_rows_HAN2

#Number of dropped patent applications
n_rows_HAN - n_rows_HAN2

Country_Breakdown <- round(prop.table(sort(table(Patent_HAN$Person_ctry_code))),2)
Country_Breakdown

colnames(Patent_HAN)

Patent_HAN <- Patent_HAN[, Applicant_Type := 
                               ifelse(Patent_HAN$Inventors==Patent_HAN$Applicants, "Inventor",
                                      ifelse(grepl("UNIVERSITY|COLLEGE|INSTITUTE|FOUNDATION|ECOLE|FOUND
                                                   |UNIV|FUNDACIO|UNIVERSIDAD|UNIVERSITÄT|INSTITUT|
                                                   CONSERVAT|ACADEMY|HOCHSCHULE|STIFTUNG|UNIVERSITAT|
                                                   UNIVERSITAET|UNIVERSITIT|SCOLA",
                                                   Applicants) | grepl("UNIVERSITY|COLLEGE|INSTITUTE|FOUNDATION|
                                                                  ECOLE|FOUND|UNIV|FUNDACIO|UNIVERSIDAD|UNIVERSITÄT|
                                                                  INSTITUT|CONSERVAT|ACADEMY|HOCHSCHULE|STIFTUNG|
                                                                  UNIVERSITAT|UNIVERSITAET|UNIVERSITIT|SCOLA", Clean_name),
                                             "Research Institution","Enterprise"))]
#Applicant Type Breakdown
table(Patent_HAN[,Applicant_Type])

#Consistency Check
sum(table(Patent_HAN[,Applicant_Type]))-n_rows_HAN2

sum(is.na(Patent_HAN[,Applicant_Type]))

#These are assumed to be enterprise
Patent_HAN <- Patent_HAN[, Applicant_Type := ifelse(is.na(Applicant_Type),"Enterprise", Applicant_Type)]

table(Patent_HAN$Applicant_Type)
sum(is.na(Patent_HAN$Applicant_Type))

Patent_HAN <- Patent_HAN[, APPLICANT_MATCH_NAME := Clean_name]

length(unique(Patent_HAN$APPLICANT_MATCH_NAME))

Patent_HAN[, APPLICANT_MATCH_NAME:= APPLICANT_MATCH_NAME %>%
                 iconv(., from = "UTF-8", to="ASCII//TRANSLIT") %>%
                 gsub("\\.$","",.) %>%
                 gsub("[^[:alnum:][:blank:].,&]","",.) %>%
                 trimws()]

length(unique(Patent_HAN$APPLICANT_MATCH_NAME))

#United States
Patent_HAN[Person_ctry_code=="UNITED STATES", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("INCORPORATED$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(",INC$","",.) %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 gsub(" LLC$","",.) %>%
                 gsub(",LLC$","",.) %>%
                 gsub("L.L.C$","",.) %>%
                 gsub(" LLP$","",.) %>%
                 gsub(",LLP$","",.) %>%
                 gsub("CORPORATION$","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(",CORP$","",.) %>%
                 gsub(" CO INC$","",.) %>%
                 gsub(" CO LLC$","",.) %>%
                 gsub(" CO$","",.) %>%
                 gsub(",CO$","",.) %>%
                 gsub(" LP$","",.) %>%
                 gsub(",LP$","",.) %>%
                 trimws()]                 

#China
Patent_HAN[Person_ctry_code=="CHINA", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("CO.,LTD$","",.) %>%
                 gsub("CO., LTD$","",.) %>%
                 gsub("CO. LTD$","",.) %>%
                 gsub("CO.LTD$","",.) %>%
                 gsub("CORPORATION$","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(",CORP$","",.) %>%
                 gsub(" CO$","",.) %>%
                 gsub("CO$","",.) %>%
                 gsub(" CORP LTD$","",.) %>%
                 gsub(",CORP LTD$","",.) %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 gsub("INCORPORATED$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(",INC$","",.) %>%
                 trimws()]

#United Kingdom 
Patent_HAN[Person_ctry_code=="UNITED KINGDOM", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("LIMITED$", "",.) %>%
                 gsub(" LTD$", "",.) %>%
                 gsub(",LTD$", "",.) %>%
                 gsub(" LLC$", "",.) %>%
                 gsub(",LLC$", "",.) %>%
                 gsub(" PLC$", "",.) %>%
                 gsub(",PLC$", "",.) %>%
                 gsub(" INC$", "",.) %>%
                 gsub(",INC$", "",.) %>%
                 trimws()]

#France
Patent_HAN[Person_ctry_code=="FRANCE", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("SÀRL$","",.) %>%
                 gsub(" SARL$","",.) %>%
                 gsub(",SARL$","",.) %>%
                 gsub(" SASU$","",.) %>%
                 gsub(",SASU$","",.) %>%
                 gsub(" SRL$","",.) %>%
                 gsub(",SRL$","",.) %>%
                 gsub("S.A.S$","",.) %>%
                 gsub(" SAS$","",.) %>%
                 gsub(",SAS$","",.) %>%
                 gsub("S.A$","",.) %>%
                 gsub(" SA$","",.) %>%
                 gsub(",SA$","",.) %>%
                 gsub(" SE$","",.) %>%
                 gsub(",SE$","",.) %>%
                 gsub("SOCIÉTÉ ANONYME$","",.) %>%
                 gsub("^SAS ","",.) %>%
                 trimws()]

#Israel
Patent_HAN[Person_ctry_code=="ISRAEL", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$", "",.) %>%
                 gsub(",LTD$", "",.) %>%
                 gsub("INCOPORATED$","",.) %>%
                 gsub(" INC$", "",.) %>%
                 gsub(",INC$", "",.) %>%
                 gsub("CO LTD$","",.) %>%
                 trimws()]

#Canada
Patent_HAN[Person_ctry_code=="CANADA", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("INCORPORATED$","",.) %>%
                 gsub(" INC$", "",.) %>%
                 gsub(",INC$", "",.) %>%
                 gsub("CORPORATION$", "",.) %>%
                 gsub(" CORP$", "",.) %>%
                 gsub(",CORP$", "",.) %>%
                 gsub("LIMITED$", "",.) %>%
                 gsub(" LTD$", "",.) %>%
                 gsub(",LTD$", "",.) %>%
                 gsub(" ULC$","",.) %>%
                 gsub(",ULC$","",.) %>%
                 gsub(" LP$","",.) %>%
                 gsub(",LP$","",.) %>%
                 trimws()]

#Japan
Patent_HAN[Person_ctry_code=="JAPAN", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("CO.,LTD$","",.) %>%
                 gsub("CO., LTD$","",.) %>%
                 gsub("CO. LTD$","",.) %>%
                 gsub("CO.LTD$","",.) %>% 
                 gsub("CORPORATION$","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(",CORP$","",.) %>%
                 gsub("CO INC$","",.) %>%
                 gsub(" CO$","",.) %>%
                 gsub(",CO$","",.) %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 gsub("INCORPORATED$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(",INC$","",.) %>%
                 gsub("K.K$","",.) %>%
                 gsub(" KK$","",.) %>%
                 gsub(",KK$","",.) %>%
                 gsub("^CO LTD","",.) %>%
                 trimws()]

#India
Patent_HAN[Person_ctry_code=="INDIA", APPLICANT_MATCH_NAME :=  APPLICANT_MATCH_NAME %>%
                 gsub("PRIVATE LIMITED$","",.) %>%
                 gsub("PRIVATE LTD$","",.) %>%
                 gsub("PVT.LTD$","",.) %>%
                 gsub("PVT. LTD$","",.) %>%
                 gsub("PVT LTD$","",.) %>%
                 gsub(" PVT$","",.) %>%
                 gsub(",PVT$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(",INC$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 trimws()]

#Germany
Patent_HAN[Person_ctry_code=="GERMANY", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub(" SLU GMBH$", "",.) %>%
                 gsub("GMBH & CO KG$", "",.) %>%
                 gsub("GMBH & CO. KG$", "",.) %>%
                 gsub("GMBH & CO$", "",.) %>%
                 gsub(" KG$", "",.) %>%
                 gsub(",KG$", "",.) %>%
                 gsub("GMBH$", "",.) %>%
                 gsub("MBH$", "",.) %>%
                 gsub(" AG$", "",.) %>%
                 gsub(",AG$", "",.) %>%
                 gsub(" SE$", "",.) %>%
                 gsub(",SE$", "",.) %>%
                 trimws()]

#Singapore
Patent_HAN[Person_ctry_code=="SINGAPORE", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("PRIVATE LIMITED$", "",.) %>%
                 gsub("PTE. LTD$", "",.) %>%
                 gsub("PTE LTD$", "",.) %>%
                 gsub("LIMITED$", "",.) %>%
                 gsub(" LTD$", "",.) %>%
                 gsub(",LTD$", "",.) %>%
                 gsub("CORP LTD$","",.) %>%
                 trimws()]

#Australia
Patent_HAN[Person_ctry_code=="AUSTRALIA", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("PTY. LTD$","",.) %>%
                 gsub("PTY.LTD$","",.) %>%
                 gsub("PTY LTD$","",.) %>%
                 gsub("(PTY)$","",.) %>%
                 gsub("PTY LIMITED$","",.) %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 gsub("CORPORATION$","",.) %>%
                 gsub(" CROP$","",.) %>%
                 gsub(" CO$","",.) %>%
                 gsub(",CO$","",.) %>%
                 trimws()]

#Sweden
Patent_HAN[Person_ctry_code=="SWEDEN", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("\\(PUBL)$","",.) %>%
                 gsub("SE AB$","",.) %>% 
                 gsub(" AB$","",.) %>%
                 gsub(",AB$","",.) %>%
                 gsub("^AB ","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(",CORP$","",.) %>%
                 trimws()]

#Spain
Patent_HAN[Person_ctry_code=="SPAIN", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub(" SL$","",.) %>%
                 gsub(",SL$","",.) %>%
                 gsub("S.L$","",.) %>%
                 gsub(", S.L$","",.) %>%
                 gsub(" SA$","",.) %>%
                 gsub(",SA$","",.) %>%
                 gsub("S.A$","",.) %>%
                 gsub(", S.A$","",.) %>%
                 gsub("S.L.U$","",.) %>%
                 gsub(" SLU$","",.) %>%
                 gsub(",SLU$","",.) %>%
                 trimws()]

#Switzerland
Patent_HAN[Person_ctry_code=="SWITZERLAND", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("SÀRL$","",.) %>%
                 gsub(" AG$","",.) %>% 
                 gsub(",AG$","",.) %>% 
                 gsub("GMBH$","",.) %>%
                 gsub("S.A$","",.) %>%
                 gsub(" SA$","",.) %>%
                 gsub(",SA$","",.) %>%
                 gsub(" SARL$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 trimws()]

#Belgium
Patent_HAN[Person_ctry_code=="BELGIUM", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub(" NV SA$","",.) %>%
                 gsub(" NV$","",.) %>%
                 gsub(" SA$","",.) %>%
                 gsub(" VZW$","",.) %>%
                 gsub(" BVBA$","",.) %>%
                 gsub(" SPRL$","",.) %>%
                 trimws()]

#Netherlands
Patent_HAN[Person_ctry_code=="NETHERLANDS", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("N.V$","",.) %>%
                 gsub(" NV$","",.) %>%
                 gsub(",NV$","",.) %>%
                 gsub("B.V$","",.) %>%
                 gsub(" BV$","",.) %>%
                 gsub(",BV$","",.) %>%
                 trimws()]

#South Korea
Patent_HAN[Person_ctry_code=="SOUTH KOREA", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("CO.,LTD$","",.) %>%
                 gsub("CO., LTD$","",.) %>%
                 gsub("CO. LTD$","",.) %>%
                 gsub("CO.LTD$","",.) %>%
                 gsub("CORPORATION$","",.) %>%
                 gsub("CORP$","",.) %>%
                 gsub(" CO$","",.) %>%
                 gsub(",CO$","",.) %>%
                 gsub("LIMITED$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(",LTD$","",.) %>%
                 gsub("INCORPORATED$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(", INC$","",.) %>%
                 trimws()]

#Italy
Patent_HAN[Person_ctry_code=="ITALY", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("S.P.A$","",.) %>%
                 gsub(" SPA$","",.) %>%
                 gsub(",SPA$","",.) %>%
                 gsub("S.R.L$","",.) %>%
                 gsub(" SRL$","",.) %>%
                 gsub(",SRL$","",.) %>%
                 trimws()]

#Finland
Patent_HAN[Person_ctry_code=="FINLAND", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("OYJ$","",.) %>%
                 gsub("OY AB$","",.) %>%
                 gsub("OY$","",.) %>%
                 gsub(" INC$","",.) %>%
                 gsub(",INC$","",.) %>%
                 gsub(" LTD$","",.) %>%
                 gsub(", LTD$","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(" CORP$","",.) %>%
                 gsub(" AB$","",.) %>%
                 gsub(" AB LTD$","",.) %>%
                 gsub(" PLC$","",.) %>%
                 gsub("^OY ","",.) %>%
                 trimws()]

#Denmark
Patent_HAN[Person_ctry_code=="DENMARK", APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub(" APS$","",.) %>%
                 gsub(" AS$","",.) %>%
                trimws()]

#Remove commas and dots at end of names
Patent_HAN[, APPLICANT_MATCH_NAME := APPLICANT_MATCH_NAME %>%
                 gsub("\\.$","",.) %>%
                 gsub(",$","",.) %>%
                 gsub(" TECH$","",.) %>%      
                 trimws()]


head(Patent_HAN$APPLICANT_MATCH_NAME,20)

Patent_HAN[, Applicant_Key_Word := stringr::word(APPLICANT_MATCH_NAME,1)]
head(Patent_HAN$Applicant_Key_Word,20)

Duplicated_Applicant <- Patent_HAN[duplicated(Patent_HAN$Patent_number),Patent_number]
length(Duplicated_Applicant)

Patent_HAN[, Multiple_Applicants := Patent_number %in% Duplicated_Applicant]

#Overview
table(Patent_HAN[, Multiple_Applicants])

Patent_HAN <- Patent_HAN[, GPT_Scope:= as.factor(ifelse(Field_Applications==1, "Applied_AI", "Core_AI"))]
table(Patent_HAN$GPT_Scope)

head(Patent_HAN)

fwrite(Patent_HAN, file="F:/Thesis/Working_Data/Final\\Patent_Dataset.csv", col.names = TRUE)
