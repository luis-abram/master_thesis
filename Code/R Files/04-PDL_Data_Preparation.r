
library(data.table)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(rio)

setwd("F:/Thesis/Source_Data/People_Data_Lab")
getwd()

PDL_Raw <- fread("F:/Thesis/Source_Data/People_Data_Lab/companies_sorted\\companies_sorted.csv", 
                 stringsAsFactors = FALSE,
                 header = TRUE,
                 na.strings = "",
                 select = c("V1", "name", "year founded", "industry", "size range", "country"),
                 encoding = "UTF-8")

#Overview Statistics
dim(PDL_Raw)
n_rows0 <- nrow(PDL_Raw)

head(PDL_Raw,5)

colnames(PDL_Raw) <- toupper(colnames(PDL_Raw))
colnames(PDL_Raw)[1] <- "PDL_ID"
colnames(PDL_Raw)[2] <- "PDL_ORIGINAL_NAME"
colnames(PDL_Raw) <- gsub(" ","_",colnames(PDL_Raw))
colnames(PDL_Raw)

PDL_Raw <- PDL_Raw[, c("PDL_ORIGINAL_NAME", "INDUSTRY", "COUNTRY") := lapply(.SD, toupper),
                   .SDcols= c("PDL_ORIGINAL_NAME", "INDUSTRY", "COUNTRY")]
head(PDL_Raw,5)

#Identify original size ranges
unique(PDL_Raw$SIZE_RANGE)

#Clean classification
PDL_Raw <- PDL_Raw[,SIZE_RANGE := SIZE_RANGE %>% 
                         gsub("10001\\+",">10000",.) %>%
                         gsub("5001 - 10000","5001-10000",.) %>%
                         gsub("1001 - 5000","1001-5000",.) %>%
                         gsub("501 - 1000","501-1,000",.) %>%
                         gsub("201 - 500","201-500",.) %>%
                         gsub("51 - 200","51-200",.) %>%
                         gsub("11 - 50","11-50",.) %>%
                         gsub("1 - 10","1-10",.) %>%
                         trimws()]
#Overview
table(PDL_Raw[,SIZE_RANGE])

PDL_Raw <- na.omit(PDL_Raw, cols = c(2,3,4))

#Number remaining Organizations
n_rows1 <- nrow(PDL_Raw)
n_rows1

#Deleted entries
n_rows0-n_rows1

Top_Countries <- c("UNITED STATES", "JAPAN","UNITED KINGDOM", "GERMANY", "CHINA", "CANADA", "NETHERLANDS",
                   "ISRAEL", "SOUTH KOREA", "FRANCE", "SWITZERLAND", "AUSTRALIA", "FINLAND", "ITALY",
                   "SWEDEN", "INDIA", "BELGIUM", "SINGAPORE", "SPAIN", "DENMARK")

PDL_Raw <- PDL_Raw[COUNTRY %in% Top_Countries,]

#Number remaining Organizations
n_rows2 <- nrow(PDL_Raw)
n_rows2

#Deleted entries
n_rows1-n_rows2

PDL_Raw[PDL_ORIGINAL_NAME=="IBM",]

PDL_Raw[PDL_ORIGINAL_NAME=="APPLE",]

PDL_Raw[PDL_ORIGINAL_NAME=="ELEVATE",]

#Order dataset by fuondation year
PDL_Raw <-PDL_Raw[order(PDL_Raw[,"YEAR_FOUNDED"]),]

#Elimincate Duplicates
PDL_Raw <- unique(PDL_Raw, by= c("PDL_ORIGINAL_NAME", "COUNTRY"))

#Number remaining Organizations
n_rows3 <- nrow(PDL_Raw)
n_rows3

#Number organizations removed
n_rows2-n_rows3


PDL_Raw[PDL_ORIGINAL_NAME=="IBM",]

PDL_Raw[PDL_ORIGINAL_NAME=="APPLE",]

PDL_Raw[PDL_ORIGINAL_NAME=="ELEVATE",]

#Create new column to store edited names 
PDL_Raw <- PDL_Raw[, PDL_MATCH_NAME := PDL_ORIGINAL_NAME]
                   
#Remove non-alphanumeric charcters
PDL_Raw <- PDL_Raw[, PDL_MATCH_NAME := PDL_MATCH_NAME %>%
                         iconv(., from = "UTF-8", to="ASCII//TRANSLIT") %>%
                         gsub("[^[:alnum:][:blank:].,&]","",.)]

#Remove dots at the end of the name
PDL_Raw[, PDL_MATCH_NAME:= PDL_MATCH_NAME %>%
              gsub("\\.$","",.) %>%
              trimws()]
#Overview
length(unique(PDL_Raw$PDL_MATCH_NAME))

#United States
PDL_Raw[COUNTRY=="UNITED STATES", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="CHINA", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="UNITED KINGDOM", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="FRANCE", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="ISRAEL", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("LIMITED$","",.) %>%
              gsub(" LTD$", "",.) %>%
              gsub(",LTD$", "",.) %>%
              gsub("INCOPORATED$","",.) %>%
              gsub(" INC$", "",.) %>%
              gsub(",INC$", "",.) %>%
              gsub("CO LTD$","",.) %>%
              trimws()]

#Canada
PDL_Raw[COUNTRY=="CANADA", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="JAPAN", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="INDIA", PDL_MATCH_NAME :=  PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="GERMANY", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub(" SLU GMBH$", "",.) %>%
              gsub("GMBH & CO KG$", "",.) %>%
              gsub("GMBH$", "",.) %>%
              gsub("MBH$", "",.) %>%
              gsub(" AG$", "",.) %>%
              gsub(",AG$", "",.) %>%
              gsub(" SE$", "",.) %>%
              gsub(",SE$", "",.) %>%
              trimws()]

#Singapore
PDL_Raw[COUNTRY=="SINGAPORE", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("PRIVATE LIMITED$", "",.) %>%
              gsub("PTE. LTD$", "",.) %>%
              gsub("PTE LTD$", "",.) %>%
              gsub("LIMITED$", "",.) %>%
              gsub(" LTD$", "",.) %>%
              gsub(",LTD$", "",.) %>%
              gsub("CORP LTD$","",.) %>%
              trimws()]

#Australia
PDL_Raw[COUNTRY=="AUSTRALIA", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="SWEDEN", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("\\(PUBL)$","",.) %>%
              gsub("SE AB$","",.) %>% 
              gsub(" AB$","",.) %>%
              gsub(",AB$","",.) %>%
              gsub("AB $","",.) %>%
              gsub(" CORP$","",.) %>%
              gsub(",CORP$","",.) %>%
              trimws()]

#Spain
PDL_Raw[COUNTRY=="SPAIN", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="SWITZERLAND", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("SÀRL$","",.) %>%
              gsub(" AG$","",.) %>% 
              gsub(",AG$","",.) %>% 
              gsub("GMBH$","",.) %>%
              gsub("S.A$","",.) %>%
              gsub(" SA$","",.) %>%
              gsub(",SA$","",.) %>%
              gsub(" LTD$","",.) %>%
              gsub(",LTD$","",.) %>%
              trimws()]

#Belgium
PDL_Raw[COUNTRY=="BELGIUM", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub(" NVSA$","",.) %>%
              gsub(" NV$","",.) %>%
              gsub(" N.V$","",.) %>%
              gsub(" BVBA$","",.) %>%
              gsub(" BV$","",.) %>%
              gsub(" B.V$","",.) %>%
              gsub(" CVBA$","",.) %>%
              gsub(" SA$","",.) %>%
              gsub(" S.A$","",.) %>%
              gsub(" SPRL$","",.) %>%
              gsub(" S.P.R.L$","",.) %>%
              gsub(" ASBL$","",.) %>%
              gsub(" VZW$","",.) %>%
              gsub(" V.Z.W$","",.) %>%
              gsub(" SPA$","",.) %>%
              gsub(" S.P.A$","",.) %>%
              trimws()]

#Netherlands
PDL_Raw[COUNTRY=="NETHERLAND", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("N.V$","",.) %>%
              gsub(" NV$","",.) %>%
              gsub(",NV$","",.) %>%
              gsub("B.V$","",.) %>%
              gsub(" BV$","",.) %>%
              gsub(",BV$","",.) %>%
              trimws()]

#South Korea
PDL_Raw[COUNTRY=="SOUTH KOREA", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
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
PDL_Raw[COUNTRY=="ITALY", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("S.P.A$","",.) %>%
              gsub(" SPA$","",.) %>%
              gsub(",SPA$","",.) %>%
              gsub("S.R.L$","",.) %>%
              gsub(" SRL$","",.) %>%
              gsub(",SRL$","",.) %>%
              trimws()]


#Finland
PDL_Raw[COUNTRY=="FINLAND", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("OYJ$","",.) %>%
              gsub("OY AB$","",.) %>%
              gsub("OY$","",.) %>%
              gsub(" INC$","",.) %>%
              gsub(",INC$","",.) %>%
              gsub(" LTD$","",.) %>%
              gsub(", LTD$","",.) %>%
              gsub(" CORP$","",.) %>%
              gsub(" CORP$","",.) %>%
              trimws()]

#Denmark
PDL_Raw[COUNTRY=="DENMARK", PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub(" APS$","",.) %>%
              gsub(" AS$","",.) %>%
              gsub(" A.S$","",.) %>%
              trimws()]

#Remove commas and dots at end of names
PDL_Raw[, PDL_MATCH_NAME := PDL_MATCH_NAME %>%
              gsub("\\.$","",.) %>%
              gsub(",$","",.) %>%
              gsub("^\\....","",.) %>%
              gsub("^\\..","",.) %>%
              gsub("^\\.","",.) %>%
              gsub("^\\,","",.) %>%
              trimws()]

PDL_Raw[PDL_MATCH_NAME=="VESTA",]

Drop_Industries <- c ("GOVERNMENT ADMINISTRATION","NEWSPAPERS","MILITARY",
                      "NON-PROFIT ORGANIZATION MANAGEMENT","EDUCATION MANAGEMENT","LEGAL SERVICES",
                      "LAW ENFORCEMENT","RELIGIOUS INSTITUTIONS","ARCHITECTURE & PLANNING","LIBRARIES",
                      "LAW PRACTICE","MUSEUMS AND INSTITUTIONS","PRIMARY/SECONDARY EDUCATION",
                      "FINE ART","PUBLIC SAFETY","NONPROFIT ORGANIZATION MANAGEMENT","GOVERNMENT RELATIONS",
                      "CIVIC & SOCIAL ORGANIZATION","PERFORMING ARTS","RECREATIONAL FACILITIES AND SERVICES",
                      "MENTAL HEALTH CARE","MUSIC","PHILANTHROPY","INDIVIDUAL & FAMILY SERVICES",
                      "FUND-RAISING","LEGISLATIVE OFFICE","JUDICIARY","ENTERTAINMENT","EVENTS SERVICES",
                      "INTERNATIONAL AFFAIRS","POLITICAL ORGANIZATION","PUBLIC POLICY","RESTAURANTS",
                      "PROFESSIONAL TRAINING & COACHING","INTERNATIONAL TRADE AND DEVELOPMENT",
                      "EXECUTIVE OFFICE","GAMBLING & CASINOS","SPORTS","ARTS AND CRAFTS","VETERINARY",
                      "SUPERMARKETS","PHOTOGRAPHY","ALTERNATIVE MEDICINE","MOTION PICTURES AND FILM",
                      "THINK TANKS","ALTERNATIVE DISPUTE RESOLUTION", "HIGHER EDUCATION","CONSTRUCTION",
                      "FURNITURE","WINE AND SPIRITS", "MANAGEMENT CONSULTING", "FOOD & BEVERAGES", "DESIGN",
                      "PROGRAM DEVELOPMENT", "MEDICAL CARE", "DAIRY", "FACILITIES SERVICE",
                      "GLASS, CERAMICS & CONCRETE", "IMPORT AND EXPORT", "LUXURY GOODS & JEWELRY", "MARITIME",
                      "OUTSOURCING/OFFSHORING", "PAPER & FOREST PRODUCTS", "PUBLIC RELATIONS AND COMMUNICATIONS",
                      "RANCHING", "SHIPBUILDING", "TOBACCO", "WAREHOUSING", "WHOLESALE")

PDL_Raw <- PDL_Raw[!(INDUSTRY %in% Drop_Industries),]

#Remaining Industries
n_rows4 <- nrow(PDL_Raw)
n_rows4

#Dropped Organizations
n_rows3-n_rows4


PDL_Raw[, PDL_Key_Word := stringr::word(PDL_MATCH_NAME,1)]
head(PDL_Raw$PDL_Key_Word)

fwrite(PDL_Raw,"F:/Thesis/Working_Data/Final\\Industrial_Dataset.csv", col.names = TRUE)


