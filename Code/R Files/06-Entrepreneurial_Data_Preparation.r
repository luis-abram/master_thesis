
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
library(readxl)

setwd("F:/Thesis/Source_Data/AI_Index_2019")
getwd()

Venture_Raw <- data.table(readxl::read_excel("Chapter 4.2. CAPIQ, CB, Quid - Investment Activity.xlsx",
                                    sheet = "raw_event report"))
colnames(Venture_Raw)

DropFeatures <- c("...1","Date of Funding Event", "Event ID", "Target Company ID", "isProfileOrganization",
                  "Quarter of Funding Event")
Venture_Raw <-dplyr::select(Venture_Raw, -!!DropFeatures)
head(Venture_Raw)
n_rows0 <- nrow(Venture_Raw)

colnames(Venture_Raw) <- toupper(colnames(Venture_Raw))
colnames(Venture_Raw) <- gsub(" ","_", colnames(Venture_Raw))
colnames(Venture_Raw)[5] <- "Original_Venture_Name"

colnames(Venture_Raw)

Venture_Raw <- Venture_Raw[, c("CLUSTERS", "COUNTRIES") := lapply(.SD, toupper),
                               .SDcols = c("CLUSTERS", "COUNTRIES")]

head(Venture_Raw)

Venture_Raw <- Venture_Raw[,FUNDING_IN_USD:= FUNDING_IN_USD/1000000]

unique(Venture_Raw$EVENT_TYPE)

Venture_Raw <- Venture_Raw[EVENT_TYPE == "Private Investment",]
n_rows1 <- nrow(Venture_Raw)
length(unique((Venture_Raw$Original_Venture_Name)))

Top_Countries <- c("UNITED STATES", "JAPAN","UNITED KINGDOM", "GERMANY", "CHINA", "CANADA", "NETHERLANDS",
                   "ISRAEL", "SOUTH KOREA", "FRANCE", "SWITZERLAND", "AUSTRALIA", "FINLAND", "ITALY",
                   "SWEDEN", "INDIA", "BELGIUM", "SINGAPORE", "SPAIN", "DENMARK")

Venture_Raw <- Venture_Raw[COUNTRIES %in% Top_Countries,]
n_rows2 <- nrow(Venture_Raw)
length(unique((Venture_Raw$Original_Venture_Name)))

#Non-aplhanumeric characters
Venture_Raw[, Clean_Name := Original_Venture_Name %>%
                  iconv(., from = "UTF-8", to="ASCII//TRANSLIT") %>%
                  gsub("[^[:alnum:][:blank:].,]","",.)]

Venture_Raw[, Clean_Name:= Clean_Name %>%
                     gsub("\\.$","",.) %>%
                     trimws() %>%
                     toupper(.)]

head(Venture_Raw$Clean_Name,50)

#United States
Venture_Raw[COUNTRIES=="UNITED STATES", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="CHINA", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="UNITED KINGDOM", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="FRANCE", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="ISRAEL", Clean_Name := Clean_Name %>%
                  gsub("LIMITED$","",.) %>%
                  gsub(" LTD$", "",.) %>%
                  gsub(",LTD$", "",.) %>%
                  gsub("INCOPORATED$","",.) %>%
                  gsub(" INC$", "",.) %>%
                  gsub(",INC$", "",.) %>%
                  gsub("CO LTD$","",.) %>%
                  trimws()]

#Canada
Venture_Raw[COUNTRIES=="CANADA", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="JAPAN", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="INDIA", Clean_Name :=  Clean_Name %>%
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
Venture_Raw[COUNTRIES=="GERMANY", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="SINGAPORE", Clean_Name := Clean_Name %>%
                  gsub("PRIVATE LIMITED$", "",.) %>%
                  gsub("PTE. LTD$", "",.) %>%
                  gsub("PTE LTD$", "",.) %>%
                  gsub("LIMITED$", "",.) %>%
                  gsub(" LTD$", "",.) %>%
                  gsub(",LTD$", "",.) %>%
                  gsub("CORP LTD$","",.) %>%
                  trimws()]

#Australia
Venture_Raw[COUNTRIES=="AUSTRALIA", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="SWEDEN", Clean_Name := Clean_Name %>%
                  gsub("\\(PUBL)$","",.) %>%
                  gsub("SE AB$","",.) %>% 
                  gsub(" AB$","",.) %>%
                  gsub(",AB$","",.) %>%
                  gsub("^AB ","",.) %>%
                  gsub(" CORP$","",.) %>%
                  gsub(",CORP$","",.) %>%
                  trimws()]

#Spain
Venture_Raw[COUNTRIES=="SPAIN", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="SWITZERLAND", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="BELGIUM", Clean_Name := Clean_Name %>%
                  gsub(" NV SA$","",.) %>%
                  gsub(" NV$","",.) %>%
                  gsub(" SA$","",.) %>%
                  gsub(" VZW$","",.) %>%
                  gsub(" BVBA$","",.) %>%
                  gsub(" SPRL$","",.) %>%
                  trimws()]

#Netherlands
Venture_Raw[COUNTRIES=="NETHERLANDS", Clean_Name := Clean_Name %>%
                  gsub("N.V$","",.) %>%
                  gsub(" NV$","",.) %>%
                  gsub(",NV$","",.) %>%
                  gsub("B.V$","",.) %>%
                  gsub(" BV$","",.) %>%
                  gsub(",BV$","",.) %>%
                  trimws()]

#South Korea
Venture_Raw[COUNTRIES=="SOUTH KOREA", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="ITALY", Clean_Name := Clean_Name %>%
                  gsub("S.P.A$","",.) %>%
                  gsub(" SPA$","",.) %>%
                  gsub(",SPA$","",.) %>%
                  gsub("S.R.L$","",.) %>%
                  gsub(" SRL$","",.) %>%
                  gsub(",SRL$","",.) %>%
                  trimws()]

#Finland
Venture_Raw[COUNTRIES=="FINLAND", Clean_Name := Clean_Name %>%
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
Venture_Raw[COUNTRIES=="DEMARK", Clean_Name := Clean_Name %>%
                  gsub(" APS$","",.) %>%
                  gsub(" AS$","",.) %>%
                  trimws()]

#Remove commas and dots at end of names
Venture_Raw[, Clean_Name := Clean_Name %>%
                    gsub("\\.$","",.) %>%
                    gsub(",$","",.) %>%
                    trimws()]

Mannual_Venture_Data <- fread("F:/Thesis/Source_Data/AI_Index_2019\\Mannual_Venture_Data.csv",
                              stringsAsFactors = FALSE, na.strings = "")
head(Mannual_Venture_Data)

nrow(Mannual_Venture_Data)
sum(is.na(Mannual_Venture_Data$Year_Foundation))

setkey(Venture_Raw, Original_Venture_Name)
setkey(Mannual_Venture_Data, Original_Venture_Name)

Venture_Raw <- Mannual_Venture_Data[Venture_Raw]
head(Venture_Raw)

WIPO_Sector_Mapping <- data.table(readxl::read_excel("F:/Thesis/Source_Data/AI_Index_2019\\AI Index - WIPO Mapping.xlsx",
                              sheet = "AI Index - WIPO Mapping"))
head(WIPO_Sector_Mapping)

setkey(Venture_Raw, CLUSTERS)
setkey(WIPO_Sector_Mapping, Cluster)
Venture_Raw <- WIPO_Sector_Mapping[Venture_Raw]

head(Venture_Raw)

Field_Applications <- c("Agriculture","Arts and Humanities","Banking and Finance","Business","Cartography",
                        "Computing in Government","Document Management and Text Processing","Education",
                        "Energy Management","Entertainment","Industry and manufacturing",
                        "Law Social and Behavioral Sciences","Life and Medical Sciences", "Military","Networks",
                        "Personal Devices Computing and Hc","Physical Sciences and Engineering","Publishing",
                        "Security","Telecommunications","Transportation")

Venture_Raw <- Venture_Raw[,GPT_Scope := ifelse(Field=="Miscellaneous","Other",
                                                                    Field %in% Field_Applications)]
Venture_Raw <- Venture_Raw[,GPT_Scope := ifelse(GPT_Scope==TRUE,"Applied_AI",
                                                                    ifelse(GPT_Scope==FALSE,"Core_AI", "Other"))]
head(Venture_Raw)

length(unique(Venture_Raw$Original_Venture_Name))
sum(is.na(Venture_Raw$Year_Foundation))

sum(is.na(Venture_Raw$Year_Foundation))/length(unique(Venture_Raw$Original_Venture_Name))

fwrite(Venture_Raw, "F:/Thesis/Working_Data/Final\\Entrepreneurial_Dataset.csv", col.name=TRUE)
