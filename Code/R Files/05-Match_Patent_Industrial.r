
options(warn=-1)

library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringdist)

setwd("F:/Thesis/Working_Data/Final")
getwd()

#Patent Dataset
Patent_Dataset <- fread("Patent_Dataset.csv", stringsAsFactors = FALSE, na.strings="",encoding = "UTF-8")

dim(Patent_Dataset)

# Industrial Dataset
Industrial_Dataset <- fread("Industrial_Dataset.csv", stringsAsFactors = FALSE, na.strings="",encoding = "UTF-8")

dim(Industrial_Dataset)

Patents_Enterprise <- Patent_Dataset[Applicant_Type=="Enterprise",.(Patent_number, APPLICANT_MATCH_NAME,
                                                                 Person_ctry_code, Applicant_Key_Word)]
dim(Patents_Enterprise)
head(Patents_Enterprise,5)

head(Industrial_Dataset,5)

#Define keys for both dataset
setkey(Patents_Enterprise, APPLICANT_MATCH_NAME)
setkey(Industrial_Dataset, PDL_MATCH_NAME)

#Data Merger
Direct_Match <- Industrial_Dataset[Patents_Enterprise, .(PDL_ID, PDL_ORIGINAL_NAME, YEAR_FOUNDED, INDUSTRY, SIZE_RANGE,
                                                   COUNTRY, PDL_MATCH_NAME, APPLICANT_MATCH_NAME, Patent_number,
                                                   Person_ctry_code, Applicant_Key_Word)]
head(Direct_Match,100)

#Number of patent application without Indsutrial set correspondend
sum(is.na(Direct_Match$PDL_ID))

#Number matched patent applications
nrow(Direct_Match)-sum(is.na(Direct_Match$PDL_ID))

#Success Rate
DM_Success <- (nrow(Direct_Match)-sum(is.na(Direct_Match$PDL_ID)))/nrow(Direct_Match)
round(DM_Success,2)

Applicants_Fuzzy <- Direct_Match[is.na(PDL_ID)==TRUE,]

#Consistency Check
dim(Applicants_Fuzzy)

#Remove unmatched patent applications
Direct_Match <- na.omit(Direct_Match, col=1)

#Consistency Check
dim(Direct_Match)

head(Direct_Match,100)

Direct_Match[Patent_number=="WO2014048855",]

Direct_Match <- Direct_Match[, COUNTRY_MATCH := Direct_Match$COUNTRY==Direct_Match$Person_ctry_code]

table(Direct_Match$COUNTRY_MATCH)

Direct_Match <- Direct_Match[, METHOD := "Direct_Match"]

colnames(Direct_Match)

colnames(Applicants_Fuzzy)

#Remove unnecesary columns
Drop_Features <- c("PDL_ID", "PDL_ORIGINAL_NAME","YEAR_FOUNDED", "INDUSTRY","SIZE_RANGE", "COUNTRY","PDL_MATCH_NAME")
Applicants_Fuzzy <- dplyr::select(Applicants_Fuzzy, -!!Drop_Features)

colnames(Applicants_Fuzzy)
dim(Applicants_Fuzzy)

Industrial_Subset <- list()

Jaro_Winkler <- list()
Levenshtein <- list()
Damerau_Levenshtein <- list()

nchar_Applicant <- list()
nchar_Industrial_Subset <- list()

Convoluted_Distance <- list()

max_distance <- list()

candidate_list <- list()

for (i in 1:nrow(Applicants_Fuzzy)){
    
    #Step 1  
    Industrial_Subset[[i]]<- Industrial_Dataset[PDL_Key_Word==as.character(Applicants_Fuzzy[i,Applicant_Key_Word])
                                    & COUNTRY==as.character(Applicants_Fuzzy[i,Person_ctry_code]),
                                    PDL_MATCH_NAME]
    #Step 2
      Jaro_Winkler[[i]] <- 1-stringdist::stringdistmatrix(Industrial_Subset[[i]], Applicants_Fuzzy[i,APPLICANT_MATCH_NAME],
                                              method = "jw", useBytes = FALSE,
                                              weight = c(d = 1, i = 1, s = 1, t = 1),p = 0.1,
                                              bt = 0, useNames = c("string"))
    #Step 3
      nchar_Applicant[[i]] <- nchar(Applicants_Fuzzy[i,APPLICANT_MATCH_NAME], type = "chars", allowNA = FALSE)
    
    #Step 4
      nchar_Industrial_Subset[[i]] <- nchar(Industrial_Subset[[i]], type = "chars", allowNA = FALSE)
      
    #Step 5
      Levenshtein[[i]] <- stringdist::stringdistmatrix(Industrial_Subset[[i]], Applicants_Fuzzy[i,APPLICANT_MATCH_NAME],
                                           method = "lv")
    #Step 6
      for (n in 1:nrow(Levenshtein[[i]])) {
            Levenshtein[[i]][n] <- 1- (Levenshtein[[i]][n]/max((nchar_Industrial_Subset[[i]][n]),(nchar_Applicant[[i]])))
      }
      
    #Step 7
      Damerau_Levenshtein[[i]] <- stringdist::stringdistmatrix(Industrial_Subset[[i]],
                                                   Applicants_Fuzzy[i,APPLICANT_MATCH_NAME], method = "dl")
    
    #Step 8
      for (n in 1:nrow(Damerau_Levenshtein[[i]])) {
            Damerau_Levenshtein[[i]][n] <- 1- (Damerau_Levenshtein[[i]][n]/max(
                  (nchar_Industrial_Subset[[i]][n]),
                  (nchar_Applicant[[i]])))
      }
      
    #Step 9
      Jaro_Winkler[[i]][Jaro_Winkler[[i]]<0.85] <- 0
      Levenshtein[[i]][Levenshtein[[i]]<0.5] <- 0
      Damerau_Levenshtein[[i]][Damerau_Levenshtein[[i]]<0.5] <- 0
      
    #Step 10
      Convoluted_Distance[[i]] <- sqrt(Jaro_Winkler[[i]]^2 + Levenshtein[[i]]^2 + Damerau_Levenshtein[[i]]^2)/sqrt(3)
    
    #Step 11
      max_distance[[i]] <-ifelse(max(Convoluted_Distance[[i]])==0,NA,which.max(Convoluted_Distance[[i]]))
    
    #Step 12
      candidate_list[[i]]<- matrix(NA, nrow=1, ncol=3)
      candidate_list[[i]][1,1] <- as.character(Applicants_Fuzzy[i,APPLICANT_MATCH_NAME])
      candidate_list[[i]][1,2] <- ifelse(length(max_distance[[i]])==0,NA,
                                         as.character(Industrial_Subset[[i]][max_distance[[i]]]))
      candidate_list[[i]][1,3] <- as.character(Applicants_Fuzzy[i,Person_ctry_code])
       
}

Candidate_List <- data.table(do.call(rbind, candidate_list))
colnames(Candidate_List) <- c("Patent_Match_Name", "PDL_Match_Name", "Person_ctry_code")
#Consistency Check
dim(Candidate_List)

#Overview
head(Candidate_List,100)

#Estimation of completenesss of the Convoluted Fuzzy Match approach
#Number of patent applicatiosn matched via Fuzzy Matchin
nrow(Candidate_List)-sum(is.na(Candidate_List))

# Fail Rate
round(sum(is.na(Candidate_List))/nrow(Candidate_List),2)

#Combine the pre-existing Applicants_Fuzzy Table with the Candidate_List
Applicants_Fuzzy <- cbind(Applicants_Fuzzy, Candidate_List)
colnames(Applicants_Fuzzy)

#Eliminate entries for which there is no PDL match by means of PDL_Match_Name
Applicants_Fuzzy <- na.omit(Applicants_Fuzzy, col=6)

#Consistency Check
dim(Applicants_Fuzzy)

head(Applicants_Fuzzy)

#set key columns thorugh which to merge datasets: Match Name and Country
setkey(Industrial_Dataset, PDL_MATCH_NAME, COUNTRY)
setkey(Applicants_Fuzzy, PDL_Match_Name, Person_ctry_code)

#Merge datasets:
Applicants_Fuzzy <- Industrial_Dataset[Applicants_Fuzzy, .(PDL_ID, PDL_ORIGINAL_NAME, YEAR_FOUNDED, INDUSTRY, SIZE_RANGE,
                                                   COUNTRY, PDL_MATCH_NAME, APPLICANT_MATCH_NAME, Patent_number,
                                                   Person_ctry_code, Applicant_Key_Word)]
head(Applicants_Fuzzy)

# Add columns indicating the combination method and wether the Country in the Indsutrial_Dataset matched with the 'Person_ctry_code'
#in the patent datast
Applicants_Fuzzy <- Applicants_Fuzzy[, COUNTRY_MATCH := Applicants_Fuzzy$COUNTRY==Applicants_Fuzzy$Person_ctry_code]
Applicants_Fuzzy <- Applicants_Fuzzy[, METHOD := "Fuzzy_Match"]
head(Applicants_Fuzzy)

#check column consistency between tables
colnames(Direct_Match)
colnames(Applicants_Fuzzy)
colnames(Direct_Match)==colnames(Applicants_Fuzzy)

#Matched Applicats in one table
Matched_Applicants <- rbind(Direct_Match,Applicants_Fuzzy)
dim(Matched_Applicants)

#set column keys
setkey(Patent_Dataset, Patent_number, APPLICANT_MATCH_NAME, Person_ctry_code)
setkey(Matched_Applicants, Patent_number, APPLICANT_MATCH_NAME, Person_ctry_code)

#Merge tables
Industrial_Patent_Dataset <- Matched_Applicants[Patent_Dataset]

dim(Industrial_Patent_Dataset)
head(Industrial_Patent_Dataset)

Industrial_Patent_Dataset[Patent_number=="EP0039393",]

Industrial_Patent_Dataset[Patent_number=="EP3457324",]

#Order dataset so that the entry with COUNTRY_MATCH==TRUE and oldest Year of foundation comes last 
Industrial_Patent_Dataset <- Industrial_Patent_Dataset[order(Patent_number,-YEAR_FOUNDED, -COUNTRY_MATCH)]

Industrial_Patent_Dataset[Patent_number=="EP0039393",]

#Group patents based on Patent_number, Clean_name and number them wihtin the group 
Industrial_Patent_Dataset <- Industrial_Patent_Dataset[, Grp := .GRP, by = .(Patent_number, Clean_name)][]
Industrial_Patent_Dataset[Patent_number=="EP0039393",]

# Create column where is indicated which rows ought to be dropped from the table.
#The only entry for a given group to be kept is the first from below
Industrial_Patent_Dataset <- Industrial_Patent_Dataset[, DROP := duplicated(Grp, fromLast = TRUE)]
Industrial_Patent_Dataset[Patent_number=="EP0039393",]

#Keep only rows where DROP==FALSE
Industrial_Patent_Dataset <- Industrial_Patent_Dataset[DROP==FALSE,]

Industrial_Patent_Dataset[Patent_number=="EP0039393",]

dim(Industrial_Patent_Dataset)

#Number of enterprise applications wth no match
No_Match_Applications <- sum(is.na(Industrial_Patent_Dataset[Applicant_Type=="Enterprise",PDL_ORIGINAL_NAME]))
No_Match_Applications

#Total number of enterprise applications
Total_Applications <- nrow(Industrial_Patent_Dataset[Applicant_Type=="Enterprise",])
Total_Applications

#Fail rate enterprise applications
Fail_Rate_Applications <- No_Match_Applications/Total_Applications
Fail_Rate_Applications

#Number enterprise applicants with no match
No_Match_Applicants <- length(unique(Industrial_Patent_Dataset[Applicant_Type=="Enterprise" & 
                                                               is.na(PDL_ORIGINAL_NAME),Clean_name]))
No_Match_Applicants

#Total number of enterprise applications
Total_Applicants <-length(unique(Industrial_Patent_Dataset[Applicant_Type=="Enterprise",Clean_name]))
Total_Applicants

#Fail rate enterprise applicats
Fail_Rate_Applicants <- No_Match_Applicants/Total_Applicants
Fail_Rate_Applicants


table(Industrial_Patent_Dataset$METHOD)



Industrial_Patent_Dataset <- Industrial_Patent_Dataset[, Year:= lubridate::year(Application_Date)]

Industrial_Patent_Dataset <- Industrial_Patent_Dataset[, Applicant_Age:= Year-YEAR_FOUNDED]
head(Industrial_Patent_Dataset)

#Drop irrelevant features
Drop_Features_Final <- c("PDL_ID", "Applicant_Key_Word", "HAN_ID", "Country", "Title", "Applicants", 
                    "Inventors", "i.Applicant_Key_Word", "COUNTRY_MATCH","Multiple_Applicants","Grp", "DROP")

Industrial_Patent_Dataset <- dplyr::select(Industrial_Patent_Dataset, -!!Drop_Features_Final)
dim(Industrial_Patent_Dataset)

#reorder columns
setcolorder(Industrial_Patent_Dataset, c("Patent_number", "Application_Date", "Clean_name", "APPLICANT_MATCH_NAME", 
                          "PDL_ORIGINAL_NAME","PDL_MATCH_NAME","METHOD", "Person_ctry_code", "COUNTRY",
                          "Applicant_Type", "YEAR_FOUNDED","SIZE_RANGE", "INDUSTRY","GPT_Scope", "Year","Applicant_Age"))
colnames(Industrial_Patent_Dataset)

#Breakdown Method
#Applications 
sum(table(Industrial_Patent_Dataset$METHOD))
table(Industrial_Patent_Dataset$METHOD)

#Applicants
Method_Analysis <- unique(Industrial_Patent_Dataset, by=c("APPLICANT_MATCH_NAME"))
sum(table(Method_Analysis$METHOD))
table(Method_Analysis$METHOD)

fwrite(Industrial_Patent_Dataset, "F:/Thesis/Working_Data/Final\\Industrial_Patent_Dataset.csv", col.names = TRUE )



