
library(data.table)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(rio)

setwd("F:/Thesis/Source_Data/WIPO_AI_Patents/WIPO_Patenscope-Transnational_Patents")
getwd()

Directory

#xls <- list.files(pattern = "*.xls", recursive = TRUE, full.names = TRUE)
#created <- mapply(convert, xls, gsub("xls", "csv", xls))

DirectoryFiles <- list.files(pattern="*.csv", recursive = TRUE)

#Number of Queries: 66
length(DirectoryFiles)
head(DirectoryFiles,3)
tail(DirectoryFiles,3)

RawPatents <- data_frame(File = DirectoryFiles) %>%
      mutate(FileContent = 
                   map(DirectoryFiles, ~ fread(., header=TRUE, skip=5, na.string=c(""),
                                               encoding = "UTF-8", stringsAsFactors=FALSE)))
RawPatents<- data.table(unnest(RawPatents))

#Dimension of combined dataset
dim(RawPatents)
n_rows0 <- nrow(RawPatents)

colnames(RawPatents)

colnames(RawPatents) <- gsub(" ","_", names(RawPatents))
colnames(RawPatents)
dim(RawPatents)

head(RawPatents$File)

RawPatents <- separate(RawPatents, File,
                       into = c("Category","Field", "Subfield", "Query_Period", NA), sep="([\\/\\-\\-\\.])")

colnames(RawPatents)
dim(RawPatents)

head(RawPatents,1)
tail(RawPatents,1)

DropFeatures <- c("Subfield","Query_Period", "I_P_C", "Priorities_Data", "National_Phase_Entries", "Abstract")
RawPatents <-select(RawPatents, -!!DropFeatures)
colnames(RawPatents)
dim(RawPatents)

sapply(RawPatents, class)

RawPatents$Application_Date <- dmy(RawPatents$Application_Date)

sapply(RawPatents, class)

head(RawPatents,5)

#Category
table((RawPatents$Category))

#Consistency Check
sum(table((RawPatents$Category)))- n_rows0


#Field
table((RawPatents$Field))

#Consistency Check
sum(table((RawPatents$Field)))- n_rows0

RawPatents <- unique(RawPatents, by=c("Field", "Application_Id"))
n_rows2 <- nrow(RawPatents)
n_rows2

#Number entries removed
n_rows0-n_rows2 

dim(RawPatents)

#Number of entries in the dataset
n_rows2

#Number of unique patent applications
length(unique(RawPatents$Application_Id))


## Map field and category into dummy features
RawPatents$Dummy <- as.integer(1)
RawPatents$Dummy1 <- as.integer(1) 

RawPatents <- dcast(RawPatents, ... ~ Field, value.var =  "Dummy", fill=0)
RawPatents <- dcast(RawPatents, ... ~ Category, value.var = "Dummy1", fill=0 )
n_rows3 <- nrow(RawPatents)

#Output
n_rows3
dim(RawPatents)
colnames(RawPatents)

head(RawPatents,5)

## Transform data type of dummy variables
RawPatents[,10:ncol(RawPatents):= lapply(.SD, as.integer),
           .SDcols = 10:ncol(RawPatents)]
sapply(RawPatents, class)

# Agregate all dummy values per unique Application_Id
RawPatents <-RawPatents[, 10:ncol(RawPatents) := lapply(.SD, sum),
                        by = Application_Id,
                        .SDcols = 10:ncol(RawPatents)]
dim(RawPatents)

#Drop duplicates
RawPatents <- unique(RawPatents, by=c("Application_Id"))
n_rows4 <- nrow(RawPatents)
n_rows4

#Splt Columns into Groups
Features <- colnames(RawPatents)
GeneralFeatures <- Features[1:9]
Categories <- Features[(length(Features)-2):length(Features)]
Fields <- Features[10:(length(Features)-3)]

#Output
Categories
Fields

length(Categories)
length(Fields)

# Patent count by Field
Count_Fields <- RawPatents[, lapply(.SD, sum),
                           .SDcols= Fields]
#Consistency Check
sum(Count_Fields)
sum(Count_Fields) - n_rows2

# Patent count by Category
Count_Categories <- RawPatents[, lapply(.SD, sum),
                               .SDcols= Categories]
#Consistency Check
sum(Count_Categories)
sum(Count_Categories) - n_rows3

fwrite(RawPatents, file="F:/Thesis/Working_Data/Final\\WIPO_Patents.csv", col.names = TRUE)


