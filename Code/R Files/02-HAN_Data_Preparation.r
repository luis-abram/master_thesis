
library(data.table)

setwd("F:/Thesis/Source_Data/OECD_HAN")
getwd()

HARM_NAMES <- fread("202001_HARM_NAMES.txt", header=TRUE, sep="|", encoding="UTF-8")
HAN_PATENTS <-fread("202001_HAN_PATENTS.txt", header=TRUE, sep = "|", encoding="UTF-8")

dim(HARM_NAMES)
dim(HAN_PATENTS)

colnames(HARM_NAMES)
colnames(HAN_PATENTS)

Match by setting 'HARM_ID' as identification key

setkey(HARM_NAMES,HARM_ID)
setkey(HAN_PATENTS,HARM_ID)

HAN_TABLE <- merge(HAN_PATENTS,HARM_NAMES)
head(HAN_TABLE,5)

fwrite(HAN_TABLE, file="F:/Thesis/Working_Data/Final\\HAN_Dataset.txt", sep="|", col.names = TRUE)
