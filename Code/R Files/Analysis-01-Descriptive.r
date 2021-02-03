options(warn=-1)

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(VennDiagram))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggridges))
suppressPackageStartupMessages(library(wesanderson))
library(Cairo)

setwd("F:/Thesis/Working_Data/Final")
getwd()

Industrial_Patent_Dataset <- fread("Industrial_Patent_Dataset.csv", stringsAsFactors = FALSE, na.strings="")
dim(Industrial_Patent_Dataset)

sapply(Industrial_Patent_Dataset,class)

Industrial_Patent_Dataset$Application_Date <- lubridate::ymd(Industrial_Patent_Dataset$Application_Date)

range(Industrial_Patent_Dataset$Application_Date) 
range(Industrial_Patent_Dataset$Year)

Features <- colnames(Industrial_Patent_Dataset)
General_Features <- Features[1:16]
Category_Features <- Features[(length(Features)-2):(length(Features))]
Field_Features <- Features[17:(length(Features)-3)]

General_Features
Category_Features
Field_Features

Total_Applications <- length(unique(Industrial_Patent_Dataset$Patent_number))
Total_Applications

#Group patent applications by year
ByYear <- unique(Industrial_Patent_Dataset, by=c("Patent_number"))
ByYear <- ByYear[, .N, by = "Year"]
ByYear <-ByYear[order(ByYear[,"Year"]),]
colnames(ByYear)[2] <- "Number_Patents"

head(ByYear)
sapply(ByYear, class)

Yearly_Applications <- ggplot(ByYear, aes(x=Year, y=Number_Patents)) +
      labs(caption="Total number of patents: 16,971") + 
      geom_bar(stat="identity", colour="gray8", fill="gray48") +
      geom_segment(aes(x=1988, xend=1988, y=0, yend=1500), color="black", linetype="dashed", size=0.5) + 
      geom_segment(aes(x=1998, xend=1998, y=0, yend=1500), color="black", linetype="dashed", size=0.5) + 
      geom_segment(aes(x=2012, xend=2012, y=0, yend=1500), color="black", linetype="dashed", size=0.5) + 
      scale_x_continuous(name=NULL, limits = c(1977.75,2019), breaks=seq(1978,2018,10)) + 
      scale_y_continuous(name="Patents per Year", limits=c(0,3000), breaks=seq(0,3000,250),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_bw()+
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Yearly_Applications

#In Jupyter Notebook code gives error but in RSutdio it works fine
ggsave(plot=Yearly_Applications,
       filename="Yearly_Applications.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Category_Overlap <- unique(Industrial_Patent_Dataset, by=c("Patent_number"))
Tech <- Category_Overlap[Techniques=="1", Patent_number]
FuA <- Category_Overlap[Functional_Applications=="1", Patent_number]
FiA <- Category_Overlap[Field_Applications=="1", Patent_number]

venn.diagram(
      x=list(Tech, FuA, FiA),
      cex=0.75,
      category.names = c("Technique", "Functional Application", "Field Application"),
      cat.default.pos = "outer",
      cat.cex  = c(0.85, 0.85, 0.85),
      cat.pos = c(-20, 10, 180),
      cat.fontfamily=c("sans","sans","sans"),
      print.mode=c("raw","percent"),
      sigdigs=2,
      width=3.54,
      height=3.54,
      units=c("in"),
      imagetype = "png",
      filename = "F:/Thesis/Charts/Final/Descriptive_Analysis\\Category_VennDiagram.png",
      output = TRUE)

#subset patents per field by patent number
Patent_Field <- replicate(length(Field_Features),list())

for (i in 1:length(Field_Features)){
      Patent_Field[[i]] <- Industrial_Patent_Dataset[get(Field_Features[i])==1,Patent_number]
}

Patent_Field_Headers <- paste("Unique_List", Field_Features)
names(Patent_Field) <- Patent_Field_Headers

#co-ocurrence table
list2env(Patent_Field, environment())
Patent_Field_Table <- crossprod(table(stack(mget(ls(pattern="Unique_List")))))


colnames(Patent_Field_Table)<- Field_Features
rownames(Patent_Field_Table)<- Field_Features

#because it is a symmetric table, upper part is zeroed
Patent_Field_Table[upper.tri(Patent_Field_Table)]<-0

Patent_Field_Table

Patent_Field_Table <- data.table(Patent_Field_Table)
Patent_Field_Table <- Patent_Field_Table[,Field := Field_Features]

Patent_Field_Table <- melt(Patent_Field_Table,
                           id.vars=c("Field"),
                           measure.vars = as.character(Field_Features),
                           variable.name = "H_Field",
                           value.name = "Number_Patents")


Field_Distribution_Graph <- ggplot(Patent_Field_Table, aes(x= H_Field, y=Field)) +
      labs(caption="Total number of patents: 16,971. Patents can be assigend to multiple fields\n Color intensity and size of circles represent the number of patents") + 
      geom_point(aes(colour=Number_Patents, size=Number_Patents)) +
      theme_bw() +
      theme(legend.position="none") + 
      scale_x_discrete(name=NULL) + 
      scale_y_discrete(name=NULL) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=6, color="black",angle=45, hjust=1)) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) +
      scale_color_gradient(low="khaki", high="darkred")
Field_Distribution_Graph

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Field_Distribution_Graph,
       filename="Field_Distribution_Graph.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=7.25,
       height=(7.25*0.75),
       units=c("in"),
       dpi=300)

Industrial_Patent_Dataset <- Industrial_Patent_Dataset[,Core_AI:=
                                                   ifelse(Industrial_Patent_Dataset[,Techniques] +
                                                                Industrial_Patent_Dataset[,Functional_Applications]>0,1,0)]

GPT_Scope_Analysis <- unique(Industrial_Patent_Dataset, by=c("Patent_number"))
GPT_Scope_Analysis <- table(GPT_Scope_Analysis$GPT_Scope)

GPT_Scope_Analysis

Core_AI_Patents <- Industrial_Patent_Dataset[Core_AI==1,Patent_number]
Applied_AI_Patents <- Industrial_Patent_Dataset[Field_Applications==1,Patent_number]
length(Core_AI_Patents)
length(Applied_AI_Patents)

venn.diagram(
      x=list(Core_AI_Patents,Applied_AI_Patents),
      fill=c("gray88", "deepskyblue4"),
      cex=0.75,
      alpha=c(1,1),
      category.names = c("Core AI", "Applied AI"),
      cat.default.pos ="outer",
      ext.length = rep(0.7,2),
      cat.pos = c(-150, 150),
      cat.distance = c(-10,-10),
      print.mode=c("raw","percent"),
      sigdigs=2,
      cat.col = c("black", "black"),
      cat.cex= c(0.8,0.8),
      width=5,
      height=5,
      units=c("in"),
      imagetype = "png",
      filename = "F:/Thesis/Charts/Final/Descriptive_Analysis\\Core-Applied_VennDiagramm.png",
      output = TRUE)

GPT_AS <- Industrial_Patent_Dataset[GPT_Scope=="Applied_AI"]
GPT_AS <- unique(GPT_AS, by=c("Patent_number"))
nrow(GPT_AS)

Field_Applications <- c("Agriculture","Arts and Humanities","Banking and Finance","Business","Cartography",
                        "Computing in Government","Document Management and Text Processing","Education",
                        "Energy Management","Entertainment","Law Social and Behavioral Sciences",
                        "Life and Medical Sciences", "Military","Networks","Personal Devices Computing and Hc",
                        "Physical Sciences and Engineering","Publishing","Security","Telecommunications",
                        "Transportation")

GPT_AS <- data.table(GPT_AS[, by = Year,
                            lapply(.SD, sum),
                            .SDcols = as.character(Field_Applications)])
head(GPT_AS,20)
dim(GPT_AS)

GPT_AS <- data.table(GPT_AS[, lapply(.SD, sum),
                            .SDcols= as.character(Field_Applications)])

GPT_AS <- GPT_AS[, Id:= ""]

GPT_AS <- melt(GPT_AS,
               id.vars = "Id",
               measure.vars = as.character(Field_Applications),
               variable.name = "Field",
               value.name = "Total_AS_Patents")

GPT_AS <- GPT_AS[,!"Id"]

GPT_AS_Graph <- GPT_AS %>%
      mutate(Field=fct_reorder(Field,Total_AS_Patents)) %>%
      ggplot(aes(x=Field, y=Total_AS_Patents)) +
      geom_segment( aes(x=Field, xend=Field, y=0, yend=Total_AS_Patents), color="deepskyblue4") +
      geom_point(color="deepskyblue4", size=2, alpha=1) +
      labs(caption="Total number of Applied AI patents: 16,184") +
      scale_x_discrete(name=NULL) + 
      scale_y_continuous(name="Patents", breaks = seq(0,15000,3000),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) +
      theme(axis.title.x=element_text(size=10)) + 
      coord_flip()
GPT_AS_Graph

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=GPT_AS_Graph,
       filename="GPT_AS_Graph.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=5,
       height=(5*0.75),
       units=c("in"),
       dpi=300)

N_Applicants <- length(unique(Industrial_Patent_Dataset$APPLICANT_MATCH_NAME))
N_Applicants

Distr_App_Type <- table((unique(Industrial_Patent_Dataset, by = "APPLICANT_MATCH_NAME"))$Applicant_Type)
Distr_App_Type

#Proportional
Distr_App_Type_Relative <- round(prop.table(table((unique(Industrial_Patent_Dataset, by = "APPLICANT_MATCH_NAME")$Applicant_Type))),2)
Distr_App_Type_Relative

#Consistency check
sum(Distr_App_Type)-N_Applicants

Distr_App_Type2 <- table(Industrial_Patent_Dataset$Applicant_Type)
Distr_App_Type2

#Proportional
Distr_App_Type2_Relative <- round(prop.table(table(Industrial_Patent_Dataset$Applicant_Type)),2)
Distr_App_Type2_Relative

#Consistency Check
sum(Distr_App_Type2)-nrow(Industrial_Patent_Dataset)


ByYear_App_Type <- Industrial_Patent_Dataset[, .N, by = c("Year", "Applicant_Type")]
ByYear_App_Type <-ByYear_App_Type[order(ByYear_App_Type[,"Year"]),]
colnames(ByYear_App_Type)[3] <- "Number_Applicants"
head(ByYear_App_Type)

App_Type_Trend <- ggplot(ByYear_App_Type, aes(x = Year, y = cumsum(Number_Applicants))) + 
      labs(caption="Number of patent applications:19,602. Includes patents with multiple applicants") + 
      geom_line(aes(color="Inventor"),ByYear_App_Type %>% filter(Applicant_Type=="Inventor")) +
      geom_line(aes(color="Research Institution"),ByYear_App_Type %>% filter(Applicant_Type=="Research Institution")) +
      geom_line(aes(color="Enterprise"),ByYear_App_Type %>% filter(Applicant_Type=="Enterprise")) +
      theme_bw() +
      theme(legend.position="top",legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10)) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(legend.text = element_text(size=10)) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) +
      scale_x_continuous(name=NULL, limits = c(1977.75,2018.25), breaks=seq(1978,2018,10)) + 
      scale_y_continuous(name="Number of Patent Applications", 
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE),
                         breaks = seq(0,17000,2000)) +
      scale_color_manual(values=wes_palette(n=3, name="BottleRocket2"))
App_Type_Trend  

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=App_Type_Trend,
       filename="App_Type_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1.25,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Founding_Year_Analysis <- Industrial_Patent_Dataset[Applicant_Type=="Enterprise" & !is.na(YEAR_FOUNDED),]
Founding_Year_Analysis <- unique(Founding_Year_Analysis, by=c("APPLICANT_MATCH_NAME"))

# Number of Enteprise Applicants for which Industrial chatacteritics was found
nrow(Founding_Year_Analysis) 

summary(Founding_Year_Analysis$YEAR_FOUNDED)

Distribution_Year_Founded <- ggplot(Founding_Year_Analysis, aes(x=YEAR_FOUNDED)) +
      labs(caption="Number of enterprises: 2,862\n Median=2000") + 
      geom_density(alpha=1,colour="gray8", fill="gray48") + 
      geom_vline(aes(xintercept = median(YEAR_FOUNDED)), color = "#FC4E08", linetype = "dashed", size = 1) + 
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans", size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans", color="black")) + 
      theme(axis.title.y=element_text(size=10)) +
      theme(axis.title.x=element_text(size=10)) + 
      scale_x_continuous(name="Year of Foundation", limits = c(1811,2018), breaks=seq(1810,2020,20)) + 
      scale_y_continuous(name="Density Distribution",  breaks = seq(0,0.04,0.01))
Distribution_Year_Founded

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Distribution_Year_Founded,
       filename="Distribution_Year_Founded.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1.25,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Applications_w_Age <- nrow(Industrial_Patent_Dataset[is.na(Applicant_Age)==FALSE,])
Applications_w_Age

summary(Industrial_Patent_Dataset$Applicant_Age)

Applicant_Age_Analysis <- Industrial_Patent_Dataset[is.na(Applicant_Age)==FALSE]
#Number of Applications
nrow(Applicant_Age_Analysis)

#Number of Applicants
length(unique(Applicant_Age_Analysis$APPLICANT_MATCH_NAME))

summary(Applicant_Age_Analysis$Applicant_Age)

Applicant_Age_Distribution <- ggplot(Applicant_Age_Analysis, aes(x=Applicant_Age)) +
      labs(caption="Number of enterprises: 2,862\n Median=24 Years") + 
      geom_density(alpha=1,colour="gray8", fill="gray48") + 
      geom_vline(aes(xintercept = median(Applicant_Age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans", size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans", color="black")) + 
      scale_x_continuous(name="Applicant Age at Time of Patenting", breaks=seq(0,200,25)) + 
      scale_y_continuous(name="Density Distribution",  breaks = seq(0,0.04,0.01))+
      theme(axis.title.y=element_text(size=10)) +
      theme(axis.title.x=element_text(size=10))
Applicant_Age_Distribution

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Applicant_Age_Distribution,
       filename="Applicant_Age_Distribution.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1.25,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Applicant_Age_Distribution_Method <- ggplot(Applicant_Age_Analysis, aes(x=Applicant_Age, fill=METHOD)) +
      geom_density(alpha=0.5) + 
      labs(caption="Number of enterprises: 2,862") + 
      scale_fill_grey()  +
      theme_bw() +
      theme(axis.text.x = element_text(family="sans", size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans", color="black")) + 
      scale_x_continuous(name="Enterprise Age") + 
      scale_y_continuous(name="Density Distribution",  breaks = seq(0,0.04,0.01))+
      theme(axis.title.y=element_text(size=10)) +
      theme(axis.title.x=element_text(size=10))  
Applicant_Age_Distribution_Method

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Applicant_Age_Distribution_Method,
       filename="Applicant_Age_Distribution_Method.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1.25,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Size_Range_Distribution <- unique(Founding_Year_Analysis, by=c("APPLICANT_MATCH_NAME"))
Size_Range_Distribution <- data.table(table(Size_Range_Distribution$SIZE_RANGE))
colnames(Size_Range_Distribution) <- c("SIZE_RANGE", "Number_Enterprises")

#Adjust Factor and Level
Size_Range_Distribution$SIZE_RANGE <- factor(Size_Range_Distribution$SIZE_RANGE,
                                             levels = c(">10000","5001-10000", "1001-5000","501-1,000",
                                                       "201-500","51-200", "11-50","1-10"))

setorder(Size_Range_Distribution,SIZE_RANGE)
Size_Range_Distribution

#Consistency Check
sum(Size_Range_Distribution$Number_Enterprises)

Size_Range_Distribution <- Size_Range_Distribution[,Share:= Number_Enterprises/sum(Number_Enterprises)]

head(Size_Range_Distribution,10)

sum(Size_Range_Distribution$Share)

Size_Range_Distribution_Boxes <-(Size_Range_Distribution$Number_Enterprises/sum(Size_Range_Distribution$Number_Enterprises))*100

Chart_names <- sprintf("%s (%s)", c(">10,000","10,000-5,001", "5,000-1,001","1,000-501",
                                                       "500-201","200-51", "50-11","10-1"),
                      scales::percent(Size_Range_Distribution_Boxes/sum(Size_Range_Distribution_Boxes)))
names(Size_Range_Distribution_Boxes) <- Chart_names

Size_Range_Distribution_Chart <- waffle::waffle(Size_Range_Distribution_Boxes,
                                                colors = (wes_palette(n=8, name="Moonrise1",type = "continuous")))
Size_Range_Distribution_Chart


#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Size_Range_Distribution_Chart,
       filename="Size_Range_Distribution_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)  

Age_Size <- Industrial_Patent_Dataset[is.na(SIZE_RANGE)==FALSE & 
                                       Applicant_Age>-1,]
unique(Age_Size$SIZE_RANGE)

Age_Size <- Age_Size[, .(SIZE_RANGE, Applicant_Age)]

Age_Size$SIZE_RANGE <- factor(Age_Size$SIZE_RANGE, levels =c("1-10","11-50","51-200","201-500",
                                                             "501-1,000","1001-5000","5001-10000",">10000"))
                                                              
table(Age_Size$SIZE_RANGE)

Boxplot_Age_Size<- ggplot(Age_Size, aes(x=SIZE_RANGE, y=Applicant_Age, fill=SIZE_RANGE)) + 
      geom_boxplot() +
     labs(caption="Number of enterprises: 2,862\n Number of patents:10,442") +
      theme_bw() + 
      theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(size=6),
            legend.key = element_rect(size = 1)) + 
      guides(col = guide_legend(ncol = 4)) +
      scale_x_discrete(name="Size Cohort",labels=c("10-1","50-11","200-51","500-201",
                                         "1,000-501","5,000-1,001","10,000-5,001",">10,000")) + 
      scale_y_continuous(name="Applicant Age at Time of Patenting", breaks=seq(0,200,20)) + 
      scale_fill_manual(values=rev(wes_palette(n=8, name="Moonrise1",type = "continuous"))) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black")) +
      theme(axis.title.x=element_text(size=10)) + 
      theme(axis.title.y=element_text(size=10)) + 
      coord_flip()
Boxplot_Age_Size

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Boxplot_Age_Size,
       filename="Boxplot_Age_Size.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1.25,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Entrepreneurial_Dataset <- fread("Entrepreneurial_Dataset.csv", stringsAsFactors = FALSE, na.strings="")
sapply(Entrepreneurial_Dataset,class)

summary(Entrepreneurial_Dataset$Year_Foundation)

setorder(Entrepreneurial_Dataset,Year_Foundation)
Foundation_Year_Analysis <-unique(Entrepreneurial_Dataset, by=c("Original_Venture_Name"))

#Total Number of startups
Total_Startups<- nrow(Foundation_Year_Analysis)
Total_Startups

#Number Startups with Founding Year available
Startups_w_FY <- Foundation_Year_Analysis[!is.na(Year_Foundation)]
nrow(Startups_w_FY)

Foundation_Year_Analysis <- Startups_w_FY[, .N, by=c("Year_Foundation")]
colnames(Foundation_Year_Analysis)[2] <- "Number_Startups"
tail(Foundation_Year_Analysis,10)

Foundation_Distribution <- ggplot(Foundation_Year_Analysis, aes(x=Year_Foundation, y=Number_Startups)) +
      labs(caption="Total number of startups: 5,719") +
      geom_bar(stat="identity", colour="gray8", fill="gray48") +
      scale_x_continuous(name=NULL, limits = c(1977,2020), breaks=seq(1978,2019,10)) + 
      scale_y_continuous(name="Number of new AI Startups",
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
      theme_bw() +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Foundation_Distribution

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Foundation_Distribution,
       filename="Foundation_Distribution.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Venture_Age_Funding <- Entrepreneurial_Dataset[, keyby= .(YEAR_OF_FUNDING_EVENT), 
                                               .(Median_Age= median(Venture_Age, na.rm=TRUE ))]

Venture_Age_Funding

setorder(Venture_Age_Funding, YEAR_OF_FUNDING_EVENT)

Venture_Age_Funding_Trend <- ggplot(Venture_Age_Funding, aes(x = YEAR_OF_FUNDING_EVENT, y = Median_Age)) + 
      geom_line(position=position_jitter(w=0.1, h=0.1), size=1) +
      labs(caption="Number of startups: 5,719") +
      scale_y_continuous(name="Age (Years)", breaks=seq(0,4,0.5)) +
      scale_x_continuous(name=NULL, breaks=seq(2009,2019,2), limits = c(2008.75,2019.25))+
      theme_bw()+
      theme(legend.position = "top", legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10)) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.title.y=element_text(size=10)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=8, color="black"))
Venture_Age_Funding_Trend

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Venture_Age_Funding_Trend,
       filename="Venture_Age_Funding_Trend.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

Field_Distribution <- unique(Entrepreneurial_Dataset, by=c("Original_Venture_Name"))
nrow(Field_Distribution)

Field_Distribution <- Field_Distribution[,.N, by=c("Field")]
colnames(Field_Distribution)[2] <- "Number_Startups"

head(Field_Distribution)

setorder(Field_Distribution, -Number_Startups)

Top_3_Rate <- sum(Field_Distribution[1:3,Number_Startups])/sum(Field_Distribution$Number_Startups)
Top_3_Rate

Field_Distr_Graph <- Field_Distribution %>%
      mutate(Field=fct_reorder(Field,Number_Startups)) %>%
      ggplot(aes(x=Field, y=Number_Startups)) +
      geom_bar(stat="identity", colour="gray8", fill="gray48") +
      labs(caption="Total number of startups: 5,719") +
      theme_bw() +
      scale_x_discrete(name=NULL) + 
      scale_y_continuous(name="Number of Startups", limits=c(0,1500), breaks = seq(0,1500,300),
                         labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
      theme(plot.caption=element_text(family="sans",size=8)) + 
      theme(axis.text.x = element_text(family="sans",size=8, color="black")) + 
      theme(axis.text.y = element_text(family="sans",size=6, color="black")) + 
      theme(axis.title.x=element_text(size=10)) +
      coord_flip()
Field_Distr_Graph

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=Field_Distr_Graph,
       filename="Field_Distr_Graph.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)

GPT_Distr_Found <- unique(Entrepreneurial_Dataset, by=c("Original_Venture_Name"))
GPT_Distr_Found <- GPT_Distr_Found[, .N, by=c("GPT_Scope")]
colnames(GPT_Distr_Found)[2] <- "Number_Startups"

head(GPT_Distr_Found)

GPT_Distr_Found <- GPT_Distr_Found[, Share := Number_Startups/sum(Number_Startups)]
head(GPT_Distr_Found)

GPT_Distr_Found_Chart <- ggplot(GPT_Distr_Found, aes(x="", y=Number_Startups, fill=GPT_Scope)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      labs(caption="Total number of startups: 5,719") +
      theme_void() +
      theme(legend.position="top", legend.title=element_blank(),legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),legend.key.size=unit("0.25","cm")) +
      theme(plot.caption=element_text(family="sans",size=8)) + 
      geom_text(aes(label = round(Share,2)), color = "black", size=3, fontface ="bold") +
      scale_fill_manual(values = c("deepskyblue4", "gray88", "gray24"), 
                        labels = c("Applied AI", "Core AI", "Other"))
GPT_Distr_Found_Chart

#In Jupyter Notebook code gives error but in RSutdio it works fine. 
ggsave(plot=GPT_Distr_Found_Chart,
       filename="GPT_Distr_Found_Chart.png",
       type = "cairo-png",
       path="F:/Thesis/Charts/Final/Descriptive_Analysis",
       scale=1,
       width=3.54,
       height=(3.54*0.75),
       units=c("in"),
       dpi=300)
