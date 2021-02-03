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

Field_Applications <- c("Agriculture","Arts and Humanities","Banking and Finance","Business","Cartography",
                        "Computing in Government","Document Management and Text Processing","Education",
                        "Energy Management","Entertainment",
                        "Law Social and Behavioral Sciences","Life and Medical Sciences", "Military",
                        "Networks","Personal Devices Computing and Hc","Physical Sciences and Engineering",
                        "Publishing","Security","Telecommunications","Transportation")

Entrepreneurial_Dataset <- fread("Entrepreneurial_Dataset.csv", stringsAsFactors = FALSE, na.strings="")
dim(Entrepreneurial_Dataset)
sapply(Entrepreneurial_Dataset,class)

setorder(Entrepreneurial_Dataset,-Year_Foundation)

Startup_List <-unique(Entrepreneurial_Dataset, by=c("Original_Venture_Name"))
nrow(Startup_List)

Startup_List <- Startup_List[!is.na(Year_Foundation),]
nrow(Startup_List)

GPT_Founding_Yearly <- Startup_List[, .N, by=c("Year_Foundation", "GPT_Scope")]
colnames(GPT_Founding_Yearly)[3] <- "Number_Startups"

#Consistency Check
sum(GPT_Founding_Yearly$Number_Startups)

head(GPT_Founding_Yearly)


GPT_Yearly_Founding_Trend <- ggplot(GPT_Founding_Yearly, aes(x=Year_Foundation, y=Number_Startups, fill=GPT_Scope)) +
      labs(caption="Number of startups: 5,719") +
      geom_bar(stat="identity") +
      geom_segment(aes(x = 2006, y = 0, xend = 2006, yend =400), linetype="dashed", color="black", size=0.5) + 
      geom_segment(aes(x = 2010, y = 0, xend = 2010, yend =400), linetype="dashed", color="black", size=0.5) + 
      scale_fill_manual(values=c("deepskyblue4", "darkgrey","black"), name = "GPT Scope", 
                        labels = c("Applied AI", "Core AI", "Other")) +
      scale_x_continuous(name=NULL, breaks=seq(1979,2019,5), limits = c(1978,2020)) +
      scale_y_continuous(name="Number of New AI Startups per Year", breaks = seq(0,1000,200),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_bw() +
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
GPT_Yearly_Founding_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=GPT_Yearly_Founding_Trend,
       filename="GPT_Yearly_Founding_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Startups_Development_AS <- Startup_List[GPT_Scope=="Applied_AI",]
nrow(Startups_Development_AS)

Startups_Development_AS <- Startups_Development_AS[, .N, by=c("Year_Foundation", "Field")]
colnames(Startups_Development_AS)[3] <- "Number_Startups"

head(Startups_Development_AS)

Total_startups_Field <- Startup_List[GPT_Scope=="Applied_AI", .N, by=c("Field")]
colnames(Total_startups_Field)[2] <- "Total_Startups_Field"

#Consistency check
sum(Total_startups_Field$Total_Field)

Total_startups_Field

Startups_Development_AS <- Total_startups_Field[Startups_Development_AS, on="Field"]
head(Startups_Development_AS)

Startups_Development_AS_Chart <- Startups_Development_AS %>%
      mutate(Field=fct_reorder(Field,Total_Startups_Field)) %>%
      ggplot(aes(x= Year_Foundation, y=Field)) +
      geom_point(aes(colour=Number_Startups, size=Number_Startups),shape=15) +
      geom_segment(aes(x = 2007, y = 0, xend = 2007, yend =13), linetype="dashed", color="black", size=0.5) + 
      geom_segment(aes(x = 2011, y = 0, xend = 2011, yend =12), linetype="dashed", color="black", size=0.5) + 
      geom_segment(aes(x = 2012, y = 0, xend = 2012, yend =11), linetype="dashed", color="black", size=0.5) + 
      labs(caption="Number of startups: 4,422\n Color intensity and size of squares represent the number of startups\nSectors listed in decreasing order by total number of startups") +
      theme_bw() +
      theme(legend.position="none") + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) + 
      theme(axis.title.y=element_text(size=10)) + 
      scale_y_discrete(name=NULL) +
      scale_x_continuous(name=NULL,limits = c(1978.75,2019.25), breaks=seq(1979,2019,5)) +
      scale_color_gradient(low="khaki", high="darkred")
Startups_Development_AS_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine.
ggsave(plot=Startups_Development_AS_Chart,
       filename="Startups_Development_AS_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

Total_startups_Field

cols_selection <- c("Patent_number", as.vector(Field_Applications))

Patent_AS <- Industrial_Patent_Dataset[GPT_Scope=="Applied_AI",..cols_selection]
nrow(Patent_AS)

head(Patent_AS)

Patent_AS <- melt(Patent_AS,
                  id.vars = c("Patent_number"),
                  measure.vars = as.character(Field_Applications),
                  variable.name = "Field",
                  value.name = "Dummy")
head(Patent_AS,10)
tail(Patent_AS,10)

Patent_AS <- Patent_AS[Dummy==1,]

#number patents
length(unique(Patent_AS$Patent_number))
       
Patent_AS <- Patent_AS[, .N, by=Field]
colnames(Patent_AS)[2] <- "Number_Patents"

head(Patent_AS,20)

Startup_Patent_corr <- Patent_AS[Total_startups_Field, on="Field", nomatch=0]
Startup_Patent_corr

#numer startups
sum(Startup_Patent_corr$Total_Startups_Field)

Scatterplot_Startup_Patent_corr <- Startup_Patent_corr %>%
      mutate(Field = fct_reorder(Field, -Total_Startups_Field)) %>%
      ggplot(aes(x=Total_Startups_Field, y=Number_Patents)) + 
      geom_point(aes(color=Field, size=1)) +
      geom_smooth(method="lm" , formula= y ~ x,  color="black", se=FALSE, linetype="dashed", alpha=0.2 ,size=0.5) + 
      labs(caption="Number of patents: 16,184. Number of startups: 4,422\n Patents can be assigend to multiple Application Sectors\nSectors listed in decreasing order by total number of startups\nDashed line: Linear Fit Model") + 
      theme_bw() +
      theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 1), legend.margin=margin(0,0,0,0),legend.key.size=unit(0.1,"cm"),
            legend.box.margin=margin(-5,-5,-5,-5)) +
      scale_x_continuous(name="Number Startups", breaks=seq(0,1500,250), limits=c(0,1500),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      scale_y_continuous(name="Number Patents",
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      scale_colour_manual(values=rev(wes_palette(n=13, name="Rushmore", type = "continuous"))) +
      guides(col = guide_legend(ncol = 4)) +
      theme() + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.title.x=element_text(size=10)) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Scatterplot_Startup_Patent_corr

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Scatterplot_Startup_Patent_corr,
       filename="Scatterplot_Startup_Patent_corr.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)

setkey(Startup_List, Clean_Name)
setkey(Industrial_Patent_Dataset, APPLICANT_MATCH_NAME)

Startup_Patents <- Startup_List[Industrial_Patent_Dataset, nomatch = 0,
                                         .(Field, Year_Foundation,GPT_Scope, Clean_Name, Patent_number,Application_Date,
                                           APPLICANT_MATCH_NAME, Year)]

head(Startup_Patents)

#Number Patent Applications
nrow(Startup_Patents)

#Number Startup Applicants
length(unique(Startup_Patents$Clean_Name))

#Share of Startups with Patent Application
length(unique(Startup_Patents$Clean_Name))/nrow(Startup_List)

table(Startup_Patents$GPT_Scope)
prop.table(table(Startup_Patents$GPT_Scope))

summary(Startup_Patents[GPT_Scope=="Applied_AI",Year])
summary(Startup_Patents[GPT_Scope=="Core_AI",Year])


Startup_Patents_GPT <- Startup_Patents[, .N, by=c("Year","GPT_Scope")]
colnames(Startup_Patents_GPT)[3] <- "Number_Patents"
head(Startup_Patents_GPT)

Startup_Patents_GPT_Trend <- ggplot(Startup_Patents_GPT, aes(x = Year, y = Number_Patents, fill=GPT_Scope)) + 
      labs(caption="Number of startups: 266\n Number of patents: 615") +
      geom_bar(stat="identity") + 
      scale_fill_manual(name = "GPT Scope", 
                         values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4", "Other"="black"),
                         labels = c("Applied AI", "Core AI", "Other")) +
      scale_y_continuous(name="Number of Patents by\n Startups per Year", limits=c(0,150)) +
      scale_x_continuous(name= NULL, breaks=seq(1978,2018,10), limits = c(1977,2019))+
      theme_bw()+
      theme(legend.position = "top", legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),legend.key.size=unit("0.25","cm")) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Startup_Patents_GPT_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Startup_Patents_GPT_Trend,
       filename="Startup_Patents_GPT_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Young_Enterprise_Patents <- Industrial_Patent_Dataset[Year>=2009 &
                                             Applicant_Type=="Enterprise" &
                                                is.na(PDL_ORIGINAL_NAME)==FALSE,]
nrow(Young_Enterprise_Patents)
head(Young_Enterprise_Patents)

Young_Enterprise_Patents <- Young_Enterprise_Patents[, Founding_Year_Cluster:= ifelse(YEAR_FOUNDED<2009,"Before","After")]

table(Young_Enterprise_Patents$Founding_Year_Cluster,Young_Enterprise_Patents$GPT_Scope )
round(prop.table(table(Young_Enterprise_Patents$Founding_Year_Cluster)),2)

Young_Enterprise_Patents_Year <- Young_Enterprise_Patents[, .N, by=c("Year", "GPT_Scope")]
colnames(Young_Enterprise_Patents_Year)[3] <- "Total_Patents_GPT"

head(Young_Enterprise_Patents_Year)

Young_Enterprise_Patents_After <- Young_Enterprise_Patents[Founding_Year_Cluster=="After",
                                                         .N, by=c("Year", "GPT_Scope")]
colnames(Young_Enterprise_Patents_After)[3] <- "Number_Patents_After" 

head(Young_Enterprise_Patents_After)

setkey(Young_Enterprise_Patents_Year, GPT_Scope, Year)
setkey(Young_Enterprise_Patents_After, GPT_Scope, Year)
Young_Enterprise_Patents_After <- Young_Enterprise_Patents_Year[Young_Enterprise_Patents_After]
head(Young_Enterprise_Patents_After)
tail(Young_Enterprise_Patents_After)

Young_Enterprise_Patents_After <- Young_Enterprise_Patents_After[, Post_09_Rate := Number_Patents_After/Total_Patents_GPT]
head(Young_Enterprise_Patents_After)
tail(Young_Enterprise_Patents_After)

Young_Enterprise_Patents_After_Trend <- ggplot(Young_Enterprise_Patents_After, aes(x = Year, y = Post_09_Rate)) + 
      labs(caption="Number of applicants: 2,215\nNumber of patents: 7,833\n Includes patents with multiple applicants\nDashed line: LOESS") +
      geom_line(aes(color="Core_AI"),Young_Enterprise_Patents_After %>%filter(GPT_Scope=="Core_AI"),size=1) +
      geom_line(aes(color="Applied_AI"),Young_Enterprise_Patents_After %>%filter(GPT_Scope=="Applied_AI"), size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.2, color="black", linetype="dashed", size=0.25) + 
      scale_color_manual(name = "GPT Scope", 
                         values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4"),
                         labels=c("Applied AI", "Core AI")) +
      scale_y_continuous(name="% of Patent Applications by \nEnterprises Founded in\n 2009-2018",
                         labels = percent_format(accuracy = 5L), limits=c(0,0.5), breaks = seq(0,0.5,0.1)) +
      scale_x_continuous(name=NULL, breaks=seq(2009,2018,3), limits = c(2008,2019))+
            theme_bw()+
      theme(legend.position = "top", legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10))+
      theme(axis.title.y=element_text(size=10)) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Young_Enterprise_Patents_After_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Young_Enterprise_Patents_After_Trend,
       filename="Young_Enterprise_Patents_After_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Young_Enterprise_Patents_DirectM <- Industrial_Patent_Dataset[Year>=2009 &
                                             Applicant_Type=="Enterprise" &
                                                is.na(PDL_ORIGINAL_NAME)==FALSE &
                                                      METHOD=="Direct_Match",]

Young_Enterprise_Patents_DirectM <- Young_Enterprise_Patents_DirectM[, Founding_Year_Cluster:= ifelse(YEAR_FOUNDED<2009,
                                                                                                      "Before","After")]

Young_Enterprise_Patents_Year_DirectM  <- Young_Enterprise_Patents_DirectM [, .N, by=c("Year", "GPT_Scope")]
colnames(Young_Enterprise_Patents_Year_DirectM )[3] <- "Total_Patents_GPT"

Young_Enterprise_Patents_After_DirectM <- Young_Enterprise_Patents_DirectM[Founding_Year_Cluster=="After",
                                                         .N, by=c("Year", "GPT_Scope")]
colnames(Young_Enterprise_Patents_After_DirectM)[3] <- "Number_Patents_After" 

setkey(Young_Enterprise_Patents_Year_DirectM, GPT_Scope, Year)
setkey(Young_Enterprise_Patents_After_DirectM, GPT_Scope, Year)
Young_Enterprise_Patents_After_DirectM <- Young_Enterprise_Patents_Year_DirectM[Young_Enterprise_Patents_After_DirectM]

Young_Enterprise_Patents_After_DirectM <- Young_Enterprise_Patents_After_DirectM[,
                                                            Post_09_Rate := Number_Patents_After/Total_Patents_GPT]
nrow(Young_Enterprise_Patents_DirectM)
length(unique(Young_Enterprise_Patents_DirectM$APPLICANT_MATCH_NAME))

head(Young_Enterprise_Patents_After_DirectM)
tail(Young_Enterprise_Patents_After_DirectM)

Young_Enterprise_Patents_After_DirectM_Trend <- ggplot(Young_Enterprise_Patents_After_DirectM, aes(x = Year, y = Post_09_Rate)) + 
      geom_line(aes(color="Core_AI"),Young_Enterprise_Patents_After_DirectM %>%filter(GPT_Scope=="Core_AI"),size=1) +
      geom_line(aes(color="Applied_AI"),Young_Enterprise_Patents_After_DirectM %>%filter(GPT_Scope=="Applied_AI"), size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.2, color="black", linetype="dashed", size=0.25) + 
      labs(caption="Number of applicants: 1,181\nNumber of patents: 4,426\n Includes patents with multiple applicants\nDirect and Convoluted Fuzzy Match\nDashed line: LOESS") +
      scale_color_manual(name = "GPT Scope", 
                         values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4"),
                         labels=c("Applied AI", "Core AI")) +
      scale_y_continuous(name="% of Patents by Enterprises\n Founded in 2009-2018\nDirect Match",
                         labels = percent_format(accuracy = 5L), limits=c(0,0.5), breaks = seq(0,0.5,0.1)) +
      scale_x_continuous(name=NULL, breaks=seq(2009,2018,3), limits = c(2008,2019))+
            theme_bw()+
      theme(legend.position = "top", legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10))+
      theme(axis.title.y=element_text(size=10)) + 
      theme(plot.caption=element_text(family="sans",size=6)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Young_Enterprise_Patents_After_DirectM_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Young_Enterprise_Patents_After_DirectM_Trend,
       filename="Young_Enterprise_Patents_After_DirectM_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

summary(Industrial_Patent_Dataset$Applicant_Age)

Applicant_Age <- Industrial_Patent_Dataset[Applicant_Age>-1 & Applicant_Type=="Enterprise",]

#Nunber Applications
length(unique(Applicant_Age$Patent_number))

nrow(Applicant_Age)
#Number unique Applicants
length(unique(Applicant_Age$APPLICANT_MATCH_NAME))

summary(Applicant_Age[GPT_Scope=="Core_AI",Applicant_Age])
summary(Applicant_Age[GPT_Scope=="Applied_AI",Applicant_Age])

Boxplot_Age_GPT<- ggplot(Applicant_Age, aes(x=GPT_Scope, y=Applicant_Age, fill=GPT_Scope)) + 
      geom_boxplot(width=0.5) +
      labs(caption="Number of applicants: 2,862\n Number of patents: 10,442") +
      theme_bw() + 
      theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 1)) + 
      scale_x_discrete(name=NULL,labels=c("Applied AI","Core AI")) + 
      scale_y_continuous(name="Enterprise Age when Submitting Applications", breaks=seq(0,200,20)) + 
      scale_fill_manual(values=c("deepskyblue4","darkgrey")) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=10, color="black")) +
      theme(axis.title.y=element_text(size=10)) + 
      coord_flip()
Boxplot_Age_GPT

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Boxplot_Age_GPT,
       filename="Boxplot_Age_GPT.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Applicant_Age_AS <- Applicant_Age[GPT_Scope=="Applied_AI",]

#Nunber Applications
length(unique(Applicant_Age_AS$Patent_number))
#Number unique Applicants
length(unique(Applicant_Age_AS$APPLICANT_MATCH_NAME))

head(Applicant_Age_AS)

cols_selction <- c("Patent_number", "Year", "Applicant_Age", as.vector(Field_Applications))
Applicant_Age_AS <- Applicant_Age_AS[, ..cols_selction]

head(Applicant_Age_AS)

Applicant_Age_AS <- melt(Applicant_Age_AS,
                  id.vars=c("Year", "Patent_number","Applicant_Age"),
                  measure.vars = as.character(Field_Applications),
                  variable.name = "Field",
                  value.name = "Dummy")

head(Applicant_Age_AS)

Applicant_Age_AS <- Applicant_Age_AS[Dummy==1,]

head(Applicant_Age_AS)

Applicant_Age_AS_Field <- Applicant_Age_AS[, .N, by=Field]
colnames(Applicant_Age_AS_Field)[2] <- "Total_Field"

head(Applicant_Age_AS_Field)

Applicant_Age_AS <- Applicant_Age_AS_Field[Applicant_Age_AS, on="Field"]
head(Applicant_Age_AS)

Boxplot_Age_AS<- Applicant_Age_AS %>%
      mutate(Field =fct_reorder(Field,Total_Field)) %>%
      ggplot(aes(x=Field, y=Applicant_Age, fill=Field)) + 
      geom_boxplot() +
      labs(caption="Number of Applied AI applicants: 2,820\nNumber of Applied AI patents: 9,583\nPatents can be assigend to multiple Application Sectors\nSectors listed in decreasing order by total number of patents") +
      geom_segment(aes(x=0, xend=20, y=50, yend=50), linetype="dashed", color="grey", size=0.75) + 
      theme_bw() + 
      theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 1)) + 
      guides(col = guide_legend(ncol = 4)) +
      scale_x_discrete(name=NULL) + 
      scale_y_continuous(name="Enterprise Age when Submitting Patent Applicantion", breaks=seq(0,200,10)) + 
      scale_fill_manual(values=wes_palette(n=20, name="Rushmore", type = "continuous")) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.y=element_text(size=10)) + 
      coord_flip()
Boxplot_Age_AS

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Boxplot_Age_AS,
       filename="Boxplot_Age_AS.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)

Median_Applicant_Age <- Industrial_Patent_Dataset[Applicant_Age>-1 & Applicant_Type=="Enterprise",]

length(unique(Median_Applicant_Age$Patent_number))
nrow(Median_Applicant_Age)
length(unique(Median_Applicant_Age$APPLICANT_MATCH_NAME))

Median_Applicant_Age <- Median_Applicant_Age[, keyby=.(GPT_Scope, Year),
                                             .(Median_Age = median(Applicant_Age))]
setorder(Median_Applicant_Age,Year)
head(Median_Applicant_Age)

summary(Median_Applicant_Age[GPT_Scope=="Core_AI",Median_Age])
summary(Median_Applicant_Age[GPT_Scope=="Applied_AI",Median_Age])

Median_Applicant_Age_Trend <- ggplot(Median_Applicant_Age, aes(x = Year, y = Median_Age)) + 
      labs(caption="Number of applicants: 2,862\nNumber of patents: 10,442\n Includes patents with multiple applicants\n Dashed line: LOESS") +
      geom_line(aes(color="Core_AI"),Median_Applicant_Age %>%filter(GPT_Scope=="Core_AI"),size=1) +
      geom_line(aes(color="Applied_AI"),Median_Applicant_Age %>%filter(GPT_Scope=="Applied_AI"),size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.2, color="black", linetype="dashed", size=0.25) + 
      scale_color_manual(name = "GPT Scope", values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4"),
                         labels=c("Applied AI", "Core AI")) +
      theme_bw()+
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10)) +
      scale_y_continuous(name="Median Age of Applicants\nper Year", breaks=seq(0,150,10), limits = c(0,150)) +
      scale_x_continuous(name= NULL, breaks=seq(1981,2018,5), limits = c(1980.75,2018.25)) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black"))
Median_Applicant_Age_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Median_Applicant_Age_Trend,
       filename="Median_Applicant_Age_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

New_Applicant_Age <- Industrial_Patent_Dataset[Applicant_Age>-1 & 
                                                Applicant_Type=="Enterprise" &
                                                is.na(YEAR_FOUNDED)==FALSE,]

length(unique(New_Applicant_Age$APPLICANT_MATCH_NAME))
nrow(New_Applicant_Age)
length(unique(New_Applicant_Age$Patent_number))

setorder(New_Applicant_Age,Year)
New_Applicant_Age <- New_Applicant_Age[, .SD[1], by = APPLICANT_MATCH_NAME]

#Consistency Check
nrow(New_Applicant_Age)
head(New_Applicant_Age)

New_Applicant_Age <- New_Applicant_Age[,keyby=.(GPT_Scope, Year),
                                             .(Yearly_Median=median(Applicant_Age))]
head(New_Applicant_Age)

New_Applicant_Age_Trend <- ggplot(New_Applicant_Age, aes(x = Year, y = Yearly_Median)) + 
      labs(caption="Number of applicants: 2,862\nNumber of patent applications: 10,442\n Includes patents with multiple applicants\nDashed line:LOESS") +
      geom_line(aes(color="Core_AI"),New_Applicant_Age %>%filter(GPT_Scope=="Core_AI"),size=1) +
      geom_line(aes(color="Applied_AI"),New_Applicant_Age %>%filter(GPT_Scope=="Applied_AI"),size=1) +
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.2, color="black", linetype="dashed", size=0.25) + 
      scale_color_manual(name = "GPT Scope", values = c("Core_AI" = "darkgrey", "Applied_AI" = "deepskyblue4"),
                         labels=c("Applied AI", "Core AI")) +
      theme_bw()+
      theme(legend.position = "top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10)) +
      scale_y_continuous(name="Median Age of New Applicants\n per Year", breaks=seq(0,150,10), limits = c(0,150)) +
      scale_x_continuous(name= NULL, breaks=seq(1981,2018,5), limits = c(1980.75,2018.25)) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black"))
New_Applicant_Age_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=New_Applicant_Age_Trend,
       filename="New_Applicant_Age_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

New_Applicant_Age_AS <- Industrial_Patent_Dataset[Applicant_Age>-1 &
                                                      Applicant_Type=="Enterprise" &
                                                      is.na(PDL_ORIGINAL_NAME)==FALSE &
                                                      GPT_Scope=="Applied_AI",]
#Number Applicants
length(unique(New_Applicant_Age_AS$APPLICANT_MATCH_NAME))

#Number Applications
length(unique(New_Applicant_Age_AS$Patent_number))

setorder(New_Applicant_Age_AS,Year)
New_Applicant_Age_AS <- New_Applicant_Age_AS[, .SD[1], by = APPLICANT_MATCH_NAME]

#Consistency Check
nrow(New_Applicant_Age_AS)

head(New_Applicant_Age_AS)

cols_selction <- c("Patent_number", "Year", "Applicant_Age", as.vector(Field_Applications))
New_Applicant_Age_AS <- New_Applicant_Age_AS[, ..cols_selction]

New_Applicant_Age_AS <- melt(New_Applicant_Age_AS,
                                id.vars=c("Year", "Patent_number","Applicant_Age"),
                                measure.vars = as.character(Field_Applications),
                                variable.name = "Field",
                                value.name = "Dummy")

head(New_Applicant_Age_AS)

New_Applicant_Age_AS <- New_Applicant_Age_AS[Dummy==1,]

head(New_Applicant_Age_AS)

Median_New_Applicant_Age_AS_Field <- New_Applicant_Age_AS[, by = Field,
                                                         .(Field_Median = median(Applicant_Age),
                                                           Total_Field_Patens=sum(Dummy))]

head(Median_New_Applicant_Age_AS_Field)

New_Applicant_Age_AS <- New_Applicant_Age_AS[, keyby=.(Field, Year),
                                                   .(Yearly_Median=median(Applicant_Age))]

head(New_Applicant_Age_AS)

New_Applicant_Age_AS <- Median_New_Applicant_Age_AS_Field[New_Applicant_Age_AS, on="Field"]
head(New_Applicant_Age_AS)

New_Applicant_Age_AS_Trend <- New_Applicant_Age_AS%>%
      mutate(Field = fct_reorder(Field, -Total_Field_Patens)) %>%
      ggplot(aes(x= Year, y=Yearly_Median, color=Field)) +
      geom_line() + 
      stat_smooth(method = "loess", formula = y ~ x, se = FALSE, alpha=0.25, color="black", linetype="dashed", size=0.5) + 
      labs(caption="Number of Applied AI applicants: 2,820\nNumber of Applied AI patent applications: 9,583\nPatents can have multiple applicants\nSectors listed in decreasing order by total number of patents\nDashed line: LOESS") + 
      theme_bw() +
      theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(size=7),
            legend.key = element_rect(size = 1)) + 
      scale_color_manual(values=rev(wes_palette(n=20, name="Rushmore", type = "continuous"))) + 
      scale_x_continuous(name= NULL, limits = c(1981,2018)) +
      scale_y_continuous(name="Median Age of New Applicants per Year", limits= c(0,126), breaks=seq(0,125,25)) + 
      theme(plot.caption=element_text(family="sans",size=6)) + 
      theme(axis.text.x = element_text(family="sans",size=6, color="black")) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) +
      facet_wrap(. ~Field) + 
      theme(strip.text.x = element_text(size = 5))
New_Applicant_Age_AS_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=New_Applicant_Age_AS_Trend,
       filename="New_Applicant_Age_AS_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300) 


Size_Analysis_All <- Industrial_Patent_Dataset[is.na(SIZE_RANGE)==FALSE,]

#Number unique applicants
length(unique(Size_Analysis_All$APPLICANT_MATCH_NAME))

#Number unique patent applications
length(unique(Size_Analysis_All$Patent_number))

#Number entries
nrow(Size_Analysis_All)

round(prop.table(table(Size_Analysis_All[GPT_Scope=="Applied_AI",SIZE_RANGE])),3)
round(prop.table(table(Size_Analysis_All[GPT_Scope=="Core_AI",SIZE_RANGE])),3)

Size_Analysis_All <- Size_Analysis_All[, .N, by=c("Year", "SIZE_RANGE", "GPT_Scope")]
colnames(Size_Analysis_All)[4] <- "Patent_per_Size"

#Consistency Check
sum(Size_Analysis_All$Patent_per_Size)

head(Size_Analysis_All)


Size_Analysis_Year <- Size_Analysis_All[,keyby=.(Year,GPT_Scope),
                                        .(Total_Year=sum(Patent_per_Size))]

#Consistency check
sum(Size_Analysis_Year$Total_Year)

head(Size_Analysis_Year)
tail(Size_Analysis_Year)

setkey(Size_Analysis_All,Year,GPT_Scope)
setkey(Size_Analysis_Year,Year,GPT_Scope)

Size_Analysis_All <- Size_Analysis_Year[Size_Analysis_All]
head(Size_Analysis_All)

Size_Analysis_All <- Size_Analysis_All[, Patent_per_Size_Rate:= Patent_per_Size/Total_Year]
head(Size_Analysis_All)

Size_Analysis_All$SIZE_RANGE <- factor(Size_Analysis_All$SIZE_RANGE, 
                                       levels = c(">10000","5001-10000", "1001-5000","501-1,000",
                                                       "201-500","51-200", "11-50","1-10"))
setorder(Size_Analysis_All,Year, SIZE_RANGE)
head(Size_Analysis_All)
sapply(Size_Analysis_All,class)

Size_Analysis_All_Trend <- ggplot(Size_Analysis_All, aes(x=Year, y=Patent_per_Size_Rate, fill=SIZE_RANGE)) +
      geom_area(alpha=1) +
      geom_segment(aes(x = 1988, y = 0, xend = 1988, yend =1), linetype="dashed", color="black", size=0.5) +
      geom_segment(aes(x = 1998, y = 0, xend = 1998, yend =1), linetype="dashed", color="black", size=0.5) +
      geom_segment(aes(x = 2012, y = 0, xend = 2012, yend =1), linetype="dashed", color="black", size=0.5) +
      labs(caption="Number of applicants: 2,862\nNumber of patents: 10,442\n Patents can have multiple applicants") + 
      theme_bw() +
      theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(size=8),
            legend.key = element_rect(size = 1), legend.margin=margin(0,0,0,0),legend.key.size=unit(0.2,"cm"),
            legend.box.margin=margin(-5,-5,-5,-5)) + 
      scale_fill_manual(values=wes_palette(n=8, name="Moonrise1", type = "continuous"),
                        labels=c(">10,000","10,000-5,001", "5,000-1,001","1,000-501","500-201","200-51",
                                   "50-11","10-1")) +
      scale_x_continuous(name= NULL, limits = c(1979,2019),breaks=seq(1978,2018,5)) +
      scale_y_continuous(name="Share of Patent Applications by Size Cluster",labels = percent_format()) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10))+
      facet_grid(GPT_Scope~.)
Size_Analysis_All_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Size_Analysis_All_Trend,
       filename="Size_Analysis_All_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

Size_Analysis_All_DirectM <- Industrial_Patent_Dataset[is.na(SIZE_RANGE)==FALSE & METHOD=="Direct_Match",]
nrow(Size_Analysis_All_DirectM)
length(unique(Size_Analysis_All_DirectM$APPLICANT_MATCH_NAME))

Size_Analysis_All_DirectM <- Size_Analysis_All_DirectM[, .N, by=c("Year", "SIZE_RANGE","GPT_Scope")]
colnames(Size_Analysis_All_DirectM)[4] <- "Patent_per_Size"

Size_Analysis_Year_DirectM <- Size_Analysis_All_DirectM[,keyby=.(Year,GPT_Scope), .(Total_Year=sum(Patent_per_Size))]

setkey(Size_Analysis_All_DirectM, Year, GPT_Scope)
setkey(Size_Analysis_Year_DirectM, Year, GPT_Scope)
Size_Analysis_All_DirectM <- Size_Analysis_Year_DirectM[Size_Analysis_All_DirectM]

Size_Analysis_All_DirectM <- Size_Analysis_All_DirectM[, Patent_per_Size_Rate:= Patent_per_Size/Total_Year]
Size_Analysis_All_DirectM$SIZE_RANGE <- factor(Size_Analysis_All_DirectM$SIZE_RANGE, 
                                       levels = c(">10000","5001-10000", "1001-5000","501-1,000",
                                                       "201-500","51-200", "11-50","1-10"))
setorder(Size_Analysis_All_DirectM,Year, SIZE_RANGE)

Size_Analysis_All_DirectM_Trend <- ggplot(Size_Analysis_All_DirectM, aes(x=Year, y=Patent_per_Size_Rate, fill=SIZE_RANGE)) +
      geom_area(alpha=1) +
      geom_segment(aes(x = 1988, y = 0, xend = 1988, yend =1), linetype="dashed", color="black", size=0.5) +
      geom_segment(aes(x = 1998, y = 0, xend = 1998, yend =1), linetype="dashed", color="black", size=0.5) +
      geom_segment(aes(x = 2012, y = 0, xend = 2012, yend =1), linetype="dashed", color="black", size=0.5) +
      labs(caption="Number of applicants: 1,508. Number of patents: 6,232\n Patents can have multiple applicants") + 
      theme_bw() +
      theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(size=8),
            legend.key = element_rect(size = 1), legend.margin=margin(0,0,0,0),legend.key.size=unit(0.2,"cm"),
            legend.box.margin=margin(-5,-5,-5,-5)) + 
      scale_fill_manual(values=wes_palette(n=8, name="Moonrise1", type = "continuous"),
                        labels=c(">10,000","10,000-5,001", "5,000-1,001","1,000-501","500-201","200-51",
                                   "50-11","10-1")) +
      scale_x_continuous(name= NULL, limits = c(1979,2019),breaks=seq(1978,2018,5)) +
      scale_y_continuous(name="Share of Patent Applications by Size Cluster - Direct Match",labels = percent_format()) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10))+
      facet_grid(GPT_Scope~.)
Size_Analysis_All_DirectM_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Size_Analysis_All_DirectM_Trend,
       filename="Size_Analysis_All_DirectM_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)

Size_Analysis_AS <- Industrial_Patent_Dataset[is.na(SIZE_RANGE)==FALSE & GPT_Scope=="Applied_AI",]

#Number unique Applicants
length(unique(Size_Analysis_AS$APPLICANT_MATCH_NAME)) 

#Number unique Patent Applications
length(unique(Size_Analysis_AS$Patent_number))

#Number Entries
nrow(Size_Analysis_AS)

head(Size_Analysis_AS)

cols_selection2 <- c("Patent_number", "Year", "SIZE_RANGE", as.vector(Field_Applications))
Size_Analysis_AS <- Size_Analysis_AS[, ..cols_selection2]
head(Size_Analysis_AS)

Size_Analysis_AS <- melt(Size_Analysis_AS,
                         id.vars=c("Year", "Patent_number","SIZE_RANGE"),
                         measure.vars = as.character(Field_Applications),
                         variable.name = "Field",
                         value.name = "Dummy")

head(Size_Analysis_AS)

Size_Analysis_AS <- Size_Analysis_AS[Dummy==1,]
head(Size_Analysis_AS)

Size_Analysis_AS <- Size_Analysis_AS[, keyby=.(Year,Field, SIZE_RANGE),
                                     .(Total_Size_Range=sum(Dummy))]

#Consistency Check
sum(Size_Analysis_AS$Total_Size_Range)

head(Size_Analysis_AS)

Size_Analysis_AS_Year <- Size_Analysis_AS[, by=.(Year,Field), .(Total_Year=sum(Total_Size_Range))]

#Consistency Check
sum(Size_Analysis_AS_Year$Total_Year)

head(Size_Analysis_AS_Year)

Size_Analysis_AS_Field <- Size_Analysis_AS_Year[, by=Field,
                                     .(Total_Field=sum(Total_Year))]
#Consistency Check
sum(Size_Analysis_AS_Field$Total_Field)

head(Size_Analysis_AS_Field)

Size_Analysis_AS_Year <- Size_Analysis_AS_Field[Size_Analysis_AS_Year, on="Field"]

head(Size_Analysis_AS_Year)

setkey(Size_Analysis_AS, Year, Field)
setkey(Size_Analysis_AS_Year, Year, Field)
Size_Analysis_AS <- Size_Analysis_AS_Year[Size_Analysis_AS]

head(Size_Analysis_AS)

Size_Analysis_AS<- Size_Analysis_AS[, Size_Share:= Total_Size_Range/Total_Year]

tail(Size_Analysis_AS)

Size_Analysis_AS$SIZE_RANGE <- factor(Size_Analysis_AS$SIZE_RANGE,
                                      levels = c(">10000","5001-10000", "1001-5000","501-1,000",
                                                       "201-500","51-200", "11-50","1-10"))

setorder(Size_Analysis_AS,Year, SIZE_RANGE)
sapply(Size_Analysis_AS,class)

Size_Analysis_AS_Chart <- Size_Analysis_AS %>%
      mutate(Field = fct_reorder(Field,-Total_Field)) %>%
      ggplot(aes(x= Year, y=Size_Share)) +
      geom_bar(aes(fill=SIZE_RANGE), postition="dodge", stat="identity") + 
      labs(caption="Number of Applied AI applicants: 2,820\nNumber of Applie AI patents: 9,583\nPatents can have multiple applicants\nSectors listed in decreasing order by total number of patents") + 
      theme_bw() +
      theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 0.5),legend.margin=margin(0,0,0,0),legend.key.size=unit("0.3","cm"),
            legend.box.margin=margin(-5,-5,-5,-5) ) +
      scale_fill_manual(values=wes_palette(n=8, name="Moonrise1", type="continuous"),
                          labels=c(">10,000","10,000-5,001", "5,000-1,001","1,000-501","500-201","200-51",
                                   "50-11","10-1")) +
      scale_x_continuous(name= NULL, limits = c(1981,2018)) +
      scale_y_continuous(name="Share of Patent Applications by Size Cluster", labels = percent_format()) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      facet_wrap(. ~Field) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(strip.text.x = element_text(size = 6))
Size_Analysis_AS_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine.
ggsave(plot=Size_Analysis_AS_Chart,
       filename="Size_Analysis_AS_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)

Large_Applicants <- Industrial_Patent_Dataset[is.na(SIZE_RANGE)==FALSE & GPT_Scope=="Applied_AI",] 
head(Large_Applicants)

Large_Applicants <- Large_Applicants[SIZE_RANGE==">10000" | SIZE_RANGE=="5001-10000",]

#Number unique Applicants
length(unique(Large_Applicants$APPLICANT_MATCH_NAME)) 

#Number unique Patent Applications
length(unique(Large_Applicants$Patent_number))

#Number Entries
nrow(Large_Applicants)

head(Large_Applicants)

cols_selection3 <- c("Patent_number", "Year", "SIZE_RANGE", as.vector(Field_Applications))
Large_Applicants <- Large_Applicants[, ..cols_selection3]
head(Large_Applicants)

Large_Applicants <- melt(Large_Applicants,
                         id.vars=c("Year", "Patent_number","SIZE_RANGE"),
                         measure.vars = as.character(Field_Applications),
                         variable.name = "Field",
                         value.name = "Dummy")

head(Large_Applicants)

Large_Applicants <- Large_Applicants[Dummy==1,]
head(Large_Applicants)

Large_Applicants <- Large_Applicants[, keyby=.(Year,Field, SIZE_RANGE),
                                     .(Total_Size_Range=sum(Dummy))]

#Consistency Check
sum(Large_Applicants$Total_Size_Range)

head(Large_Applicants)

Large_Applicants <- Large_Applicants[, keyby=.(Year, Field),
                                    .(Large_Applicants=sum(Total_Size_Range))]

tail(Large_Applicants)

head(Size_Analysis_AS_Year)

setkey(Large_Applicants, Year, Field)
setkey(Size_Analysis_AS_Year, Year, Field)
Large_Applicants <- Size_Analysis_AS_Year[Large_Applicants]
tail(Large_Applicants)

Large_Applicants <- Large_Applicants[, Share_Large:= Large_Applicants/Total_Year]
tail(Large_Applicants)

Large_Applicants_Trend <- Large_Applicants %>%
      mutate(Field =fct_reorder(Field,-Total_Field)) %>%
      ggplot(aes(x=Year, y=Share_Large, group = Field, colour = Field)) + 
      labs(caption="Number of Applied AI applicants: 2,820\nNumber of Applied AI patents: 9,583\n Patents can have multiple applicants\nSectors listed in decreasing order by total number of patents\nMedian:32.7%") + 
      geom_line() +
      geom_segment(aes(x=1978, xend=2019, y=0.327, yend=0.327),
                  linetype="dashed", color="black", size=0.75) + 
      theme_bw() + 
      theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 1)) + 
      guides(col = guide_legend(ncol = 4)) +
      scale_x_continuous(name=NULL, limits=c(1977,2019), breaks=seq(1978,2018, 20)) + 
      scale_y_continuous(name="Share of Application by Enterprises with >5,000 Employees",
                         labels = percent_format(accuracy = 5L), limits=c(0,1), breaks = seq(0,1,0.1)) + 
      scale_color_manual(values=rev(wes_palette(n=20, name="Rushmore", type = "continuous"))) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=6, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.y=element_text(size=10)) +
      theme(strip.text.x = element_text(size = 6)) + 
      facet_wrap(Field~.)
Large_Applicants_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Large_Applicants_Trend,
       filename="Large_Applicants_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Research_Question_II",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)
