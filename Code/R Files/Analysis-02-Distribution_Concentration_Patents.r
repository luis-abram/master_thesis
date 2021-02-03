options(warn=-1)

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(VennDiagram))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(wesanderson))
suppressPackageStartupMessages(library(ggridges))

setwd("F:/Thesis/Working_Data/Final")
getwd()

Industrial_Patent_Dataset <- fread("Industrial_Patent_Dataset.csv", stringsAsFactors = FALSE, na.strings="")
dim(Industrial_Patent_Dataset)
sapply(Industrial_Patent_Dataset,class)

Features <- colnames(Industrial_Patent_Dataset)
General_Features <- Features[1:16]
Category_Features <- Features[(length(Features)-2):(length(Features))]
Field_Features <- Features[17:(length(Features)-3)]

General_Features
Category_Features
Field_Features

GPT_ByYear <- unique(Industrial_Patent_Dataset, by=c("Patent_number"))
GPT_ByYear <- GPT_ByYear[, .N, by = c("Year", "GPT_Scope")]
colnames(GPT_ByYear)[3] <- "Number_Patents"
GPT_ByYear <- GPT_ByYear[order(GPT_ByYear[,Year])]
head(GPT_ByYear,10)

#Consistency Check
sum(GPT_ByYear[GPT_Scope=="Applied_AI",Number_Patents])
sum(GPT_ByYear[GPT_Scope=="Core_AI",Number_Patents])
sum(GPT_ByYear[,Number_Patents])

summary(GPT_ByYear[GPT_Scope=="Core_AI",Year])
summary(GPT_ByYear[GPT_Scope=="Applied_AI",Year])

GPT_Scope_Trend <- ggplot(GPT_ByYear, aes(x = Year, y = Number_Patents,fill=GPT_Scope)) + 
      labs(caption="Number of patents:16,971") +
      geom_bar(stat="identity") +    
      scale_fill_manual(values=c("deepskyblue4", "darkgrey"), name = "GPT Scope", labels = c("Applied AI", "Core AI")) +
      geom_segment(aes(x = 1978, y = 0, xend = 1978, yend = 1000), color = "black", size=0.25) + 
      geom_segment(aes(x = 1985, y = 0, xend = 1985, yend = 1500), color = "black", size=0.25) + 
      geom_segment(aes(x = 1988, y = 0, xend = 1988, yend = 1000), color = "black", size=0.5, linetype="dashed") +
      geom_segment(aes(x = 1998, y = 0, xend = 1998, yend = 1000), color = "black", size=0.5, linetype="dashed") +
      geom_segment(aes(x = 2012, y = 0, xend = 2012, yend = 1000), color = "black", size=0.5, linetype="dashed") +
      geom_segment(aes(x = 2016, y = 0, xend = 2016, yend = 500), color = "black", size=0.5, linetype="dashed") +
      scale_x_continuous(name=NULL, limits = c(1977,2019), breaks=seq(1978,2019,10)) + 
      scale_y_continuous(name="Patent Applications per Year", breaks = seq(0,3000,500),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=10, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=10, color="black")) +
      theme(axis.title.y=element_text(size=8)) +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) +
      annotate(geom="text", x=1978, y=1100, label="1st Applied AI Patent",
               color="black", size=2.5,family="sans", hjust="left") +
      annotate(geom="text", x=1985, y=1500, label="1st Core AI Patent",
               color="black", size=2.5, family="sans", hjust="left")
GPT_Scope_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine.
ggsave(plot=GPT_Scope_Trend,
       filename="GPT_Scope_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

GPT_ByYear_AS <- Industrial_Patent_Dataset[GPT_Scope=="Applied_AI"]
GPT_ByYear_AS <- unique(GPT_ByYear_AS, by=c("Patent_number"))

#Consistency Check
nrow(GPT_ByYear_AS)

Field_Applications <- c("Agriculture","Arts and Humanities","Banking and Finance","Business","Cartography",
                        "Computing in Government","Document Management and Text Processing","Education",
                        "Energy Management","Entertainment","Law Social and Behavioral Sciences",
                        "Life and Medical Sciences", "Military","Networks","Personal Devices Computing and Hc",
                        "Physical Sciences and Engineering","Publishing","Security","Telecommunications",
                        "Transportation")

GPT_ByYear_AS <- data.table(GPT_ByYear_AS[, by = Year,
                                          lapply(.SD, sum),
                                          .SDcols = as.character(Field_Applications)])
head(GPT_ByYear_AS)

#Adjust table format for graph
GPT_ByYear_AS <- melt(GPT_ByYear_AS, 
                            id.vars = "Year",
                            measure.vars = as.character(Field_Applications),
                            variable.name = "Field",
                            value.name = "Total_AS_Patents")
head(GPT_ByYear_AS,20)

GPT_ByYear_AS_Chart <- GPT_ByYear_AS %>%
      mutate(Field=fct_reorder(Field,Total_AS_Patents)) %>%
      ggplot(aes(x= Year, y=Field)) +
      labs(caption="\nNumber of Applied AI patents:16,184\nPatents can be allocated to multiple Application Sectors\nColor intensity and size of squares represent the number of patents\nSectors listed in decreasing order by total number of patents") +
      geom_point(aes(colour=Total_AS_Patents, size=Total_AS_Patents), shape=15) +
      geom_segment(aes(x=1988, xend=1988, y=0, yend=20), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=1998, xend=1998, y=0, yend=20), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=2012, xend=2012, y=0, yend=20), color = "black", size=0.25, linetype="dashed") +
      scale_x_continuous(name=NULL,limits = c(1977,2019), breaks=seq(1978,2018,5)) +
      scale_y_discrete(name=NULL) + 
      theme_bw() +
      theme(legend.position = "none",legend.title=element_blank()) + 
      theme( plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) +
      scale_color_gradient(low="khaki", high="darkred")
GPT_ByYear_AS_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=GPT_ByYear_AS_Chart,
       filename="GPT_ByYear_AS_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

ByDate_Applicant <- Industrial_Patent_Dataset[,.N, by=c("Year","APPLICANT_MATCH_NAME", "GPT_Scope")]
head(ByDate_Applicant,20)

#Order by Date
ByDate_Applicant <- ByDate_Applicant[order(ByDate_Applicant[,Year]),]

colnames(ByDate_Applicant)[4] <- "Number_Patents"

#Consistency Check
sum(ByDate_Applicant$Number_Patents)

head(ByDate_Applicant)

ByDate_Applicant <- ByDate_Applicant[,.N, by=c("Year","GPT_Scope")]
colnames(ByDate_Applicant)[3] <- "Number_Applicants"

head(ByDate_Applicant,10)

ByDate_Applicant[Year>1988 & Year<=1998 & GPT_Scope=="Applied_AI",]

ByDate_Applicant[Year>1998 & Year<=2012 & GPT_Scope=="Applied_AI",]

ByDate_Applicant[Year>2012 & Year<2019 & GPT_Scope=="Applied_AI",]

Applicants_per_Year<- ggplot(ByDate_Applicant, aes(x=Year, y=Number_Applicants, fill=GPT_Scope)) +
      labs(caption="Number of applicants: 6,807") +
      geom_bar(stat="identity") +
      geom_segment(aes(x=1988, xend=1988, y=0, yend=750), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=1998, xend=1998, y=0, yend=750), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=2012, xend=2012, y=0, yend=750), color = "black", size=0.25, linetype="dashed") +
      theme_bw() +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.y=element_text(size=10)) + 
      scale_fill_manual(values=c("deepskyblue4", "darkgrey"), name = "GPT Scope", labels = c("Applied AI", "Core AI")) +
      scale_x_continuous(name=NULL, breaks=seq(1978,2018,5), limits = c(1977,2019)) +
      scale_y_continuous(name="Number of Patent Applicants per Year", breaks=seq(0,1750,250), limits=c(0,1750),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))
Applicants_per_Year

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Applicants_per_Year,
       filename="Applicants_per_Year.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Avg_Application <- Industrial_Patent_Dataset[, keyby = .(APPLICANT_MATCH_NAME, Year, GPT_Scope),
                                        .(Number_Patents = .N)]
#Consistency Check
length(unique(Avg_Application$APPLICANT_MATCH_NAME))
sum(Avg_Application$Number_Patents)

head(Avg_Application)

Avg_Application <- Avg_Application[, keyby= c("Year", "GPT_Scope"),
                                   .(Average_Patent = mean(Number_Patents, na.rm = TRUE))]
head(Avg_Application,20)

Avg_Application_Trend <- ggplot(Avg_Application, aes(x = Year, y = Average_Patent)) + 
      labs(caption="Number of applicants: 6,807\nNumber of patents: 19,602\nIncludes patents with multiple applicants\nDashed line: LOESS") +
      geom_line(aes(color="Core_AI"), Avg_Application%>%filter(GPT_Scope=="Core_AI"), size=1) +
      geom_line(aes(color="Applied_AI"),Avg_Application %>%filter(GPT_Scope=="Applied_AI"), size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.3, color="black", linetype="dashed", size=0.5) + 
      scale_color_manual(name = "GPT Scope", 
                         values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4"),
                         labels=c("Applied AI", "Core AI")) +
      scale_x_continuous(name=NULL, breaks=seq(1978,2018,5), limits=c(1977,2019)) +
      scale_y_continuous(name="Average Number of\n Patent Applications", breaks=seq(0,3,0.5), limits=c(0,3)) +
      theme_bw()+
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10),legend.key = element_blank()) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) +
      theme(axis.title.y=element_text(size=10))+
      scale_linetype_manual(labels=c("Applied AI", "Core AI"),
                            values = c("Applied AI"=2, "Core AI"=2))
Avg_Application_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Avg_Application_Trend,
       filename="Avg_Application_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

setorder(Industrial_Patent_Dataset,Year)
New_Applicant <- Industrial_Patent_Dataset[, .SD[1], by = APPLICANT_MATCH_NAME]

#Consistency Check should match the number of unique applicants
nrow(New_Applicant)

head(New_Applicant)

New_Applicant <- New_Applicant[,.N, by=c("Year","GPT_Scope")]
colnames(New_Applicant)[3] <- "New_Applicant"

head(New_Applicant)

mean(New_Applicant[Year<=1988 ,New_Applicant])
mean(New_Applicant[Year>1988 & Year<=1998,New_Applicant])
mean(New_Applicant[Year>1998 & Year<=2012,New_Applicant])
New_Applicant[Year==2018,]

New_Applicants_per_Year<- ggplot(New_Applicant, aes(x=Year, y=New_Applicant, fill=GPT_Scope)) +
      labs(caption="Number of applicants: 6,807") +
      geom_bar(stat="identity") +
      geom_segment(aes(x=1988, xend=1988, y=0, yend=500), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=1998, xend=1998, y=0, yend=500), color = "black", size=0.25, linetype="dashed") +
      geom_segment(aes(x=2012, xend=2012, y=0, yend=500), color = "black", size=0.25, linetype="dashed") +
      theme_bw() +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.y=element_text(size=10)) + 
      scale_fill_manual(values=c("deepskyblue4", "darkgrey"), name = "GPT Scope", labels = c("Applied AI", "Core AI")) +
      scale_x_continuous(name=NULL, breaks=seq(1978,2018,5), limits = c(1977,2019)) +
      scale_y_continuous(name="Number of New Applicants\n per Year", breaks=seq(0,1000,250), limits=c(0,1000),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))
New_Applicants_per_Year

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=New_Applicants_per_Year,
       filename="New_Applicants_per_Year.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

setkey(New_Applicant, Year, GPT_Scope)
setkey(ByDate_Applicant, Year, GPT_Scope)

Share_New_Applicants <- ByDate_Applicant[New_Applicant]

head(Share_New_Applicants)

Share_New_Applicants <- Share_New_Applicants[, Share_New := New_Applicant/Number_Applicants]

head(Share_New_Applicants)

summary(Share_New_Applicants[GPT_Scope=="Applied_AI",Share_New])
summary(Share_New_Applicants[GPT_Scope=="Core_AI",Share_New])

Share_New_Applicants_Chart<- ggplot(Share_New_Applicants, aes(x=Year, y=Share_New, color=GPT_Scope)) +
      labs(caption="Dashed line: LOESS") +
      geom_line(size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.3, color="black", linetype="dashed", size=0.5) + 
      theme_bw() +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.y=element_text(size=10)) + 
      scale_color_manual(values=c("deepskyblue4", "darkgrey"), name = "GPT Scope", labels = c("Applied AI", "Core AI")) +
      scale_x_continuous(name=NULL, breaks=seq(1978,2018,5), limits = c(1977,2019)) +
      scale_y_continuous(name="Share of New Applicants per Year",
                         labels = percent_format(), limits=c(0,1), breaks=seq(0,1,0.2))
Share_New_Applicants_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine.
ggsave(plot=Share_New_Applicants_Chart,
       filename="Share_New_Applicants_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

setkey(New_Applicant, Year, GPT_Scope)
setkey(GPT_ByYear, Year, GPT_Scope)
First_Application_Rate <- GPT_ByYear[New_Applicant]

head(First_Application_Rate)

First_Application_Rate <- First_Application_Rate[, First_Patent_Rate := New_Applicant/Number_Patents]
head(First_Application_Rate)

First_Application_Trend <- ggplot(First_Application_Rate, aes(x = Year, y = First_Patent_Rate)) + 
      labs(caption="Number of 1st-time applications: 6,807\nNumber of patents:19,602\nIncludes patents with multiple applicants\nDashed line: LOESS") +
      geom_line(aes(color="Core AI"), First_Application_Rate %>% filter(GPT_Scope=="Core_AI"), size=1) +
      geom_line(aes(color="Applied AI"), First_Application_Rate %>% filter(GPT_Scope=="Applied_AI"), size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.4, color="black", linetype="dashed", size=0.5) + 
      scale_color_manual(name = "GPT Scope", 
                         values = c("Core AI" = "darkgrey", "Applied AI" = "deepskyblue4")) +
      scale_x_continuous(name=NULL, breaks=seq(1978,2018,5), limits=c(1977,2019)) +
      scale_y_continuous(name="Share of First-time Applications\n per Year",
                         labels = percent_format(), limits=c(0,1), breaks=seq(0,1,0.2))+
      theme_bw() +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10))  +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10))
First_Application_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=First_Application_Trend,
       filename="First_Application_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

Patenting_Distribution <- Industrial_Patent_Dataset[, .N, by=c("APPLICANT_MATCH_NAME")]
colnames(Patenting_Distribution)[2] <- "Number_Patents"

#Consistency Check
nrow(Patenting_Distribution)

head(Patenting_Distribution)

summary(Patenting_Distribution$Number_Patents)

Patenting_Distribution <- Patenting_Distribution[, .N, by=c("Number_Patents")]
colnames(Patenting_Distribution)[2] <- "Number_Applicants"
setorder(Patenting_Distribution,Number_Patents)
head(Patenting_Distribution)
tail(Patenting_Distribution)

Patenting_Distribution <- Patenting_Distribution[, Total_Patents_Cohort:= Number_Patents*Number_Applicants]

#Consistency Check
sum(Patenting_Distribution$Number_Applicants)
sum(Patenting_Distribution$Total_Patents)
sum(Patenting_Distribution$Total_Patents_Cohort)

head(Patenting_Distribution)
tail(Patenting_Distribution)

Patent_Distr_Chart <- ggplot(Patenting_Distribution, aes(x=Number_Patents, y=Number_Applicants)) +
      labs(caption="Number of applicants: 6,807\nNumber of patent applications: 16,971") +
      geom_bar(stat="identity") + 
      scale_x_continuous(name="Number of Patents per Applicant",breaks=seq(0,75,5), limits = c(0,76)) +
      scale_y_continuous(name="Number of Applicants", breaks=seq(0,5000,1000), limits=c(0,5000),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) +
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.x=element_text(size=10)) +
      theme(axis.title.y=element_text(size=10))
Patent_Distr_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Patent_Distr_Chart,
       filename="Patent_Distr_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

#Number of Applicants
Small_Applicants <- sum(Patenting_Distribution[Number_Patents<=12,Number_Applicants])
Small_Applicants

#Share of patent applicants
round(Small_Applicants/sum(Patenting_Distribution[,Number_Applicants]),3)

#Number Applications
Small_Applications <- sum(Patenting_Distribution[Number_Patents<=12,Total_Patents_Cohort])
Small_Applications

#Share of patent applications
round(Small_Applications/sum(Patenting_Distribution[,Total_Patents_Cohort]),3)

#Number of Applicants
Large_Applicants <- sum(Patenting_Distribution[Number_Patents>=75,Number_Applicants])
Large_Applicants

#Share of patent applicants
round(Large_Applicants/sum(Patenting_Distribution[,Number_Applicants]),3)

#Number Applications
Large_Applications <- sum(Patenting_Distribution[Number_Patents>=75,Total_Patents_Cohort])
Large_Applications

#Share of patent applications
round(Large_Applications/sum(Patenting_Distribution[,Total_Patents_Cohort]),3)

head(Patenting_Distribution)

Distribution_Curve <- ggplot(Patenting_Distribution, aes(x=Number_Patents,
                                                             y=cumsum(Total_Patents_Cohort)/sum(Total_Patents_Cohort))) +
      labs(caption="Number of applicants: 6,807\n Number of patent applications: 16,971") +
      geom_line(colour="gray8", size=1) + 
      geom_rug(sides="b") + 
      geom_segment(aes(x = 12, y = 0.2, xend = 12, yend = 1), size=0.25, linetype= "dashed") + 
      geom_segment(aes(x = 75, y = 0.2, xend = 75, yend = 1), size=0.25,  linetype= "dashed") + 
      scale_x_continuous(name="Number of Patents per Applicant", breaks = seq(0,500,50))+
      scale_y_continuous(name="Cumulative Share of Patents",
                         breaks=seq(0,1,0.1),labels = percent_format(accuracy = 5L)) +
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) +
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.x=element_text(size=10)) +
      theme(axis.title.y=element_text(size=10))
Distribution_Curve

ggsave(plot=Distribution_Curve,
       filename="Distribution_Curve.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_I",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Top_Owners <- Industrial_Patent_Dataset[, .N, by=c("APPLICANT_MATCH_NAME")]
colnames(Top_Owners)[2] <-
"Number_Patents"
head(Top_Owners)

Top_Owners <- Top_Owners[Number_Patents>=75,]
setorder(Top_Owners,-Number_Patents )
Top_Owners

Top_Owners$APPLICANT_MATCH_NAME
