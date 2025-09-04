library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(netrankr) #library for calculating pagerank related indicators (i.e. centrality_closeness_harmonic and centrality_closeness_residual)

library(dplyr)
library(readxl) #for reading the xlsx files
library(tidyr)
library(ggrepel)
library(scales) #for scaling without cutting data out
library(patchwork) #for cutting out the X labs while keeping the legend
library(RColorBrewer)

library(ggforce) #for using geom_mark_hull
library(stringr) #for applying str_match

library(readxl)
library(openxlsx)
library(zoo) #for some reason the function na.locf is just in the zoo package for me now
library(psych) #for descriptives

library(MatchIt) 
library(ggplot2)
#library(lmtest) #for coeftest
#library(sandwich) #for vcovCL

library(did) #for treatment effect over distinct periods of time
library(vtable) #for functions st and sumtable in the statistics
#library(readr) #for reading text files;
library(ggcorrplot) #for correlation table

#note that most used codes, specializations, RCAs and others are in another code, because they need the complete set of 
#patents to work

#First part - Matching companies-----

#1.Pre-matching General ####
###1.1.Select possible dataset ----
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
#pick GUOs that appear in the filtered dataset
FinalDataset <- read.csv("files_created_code2/FinalDataset_allYears_FullGUOs_filtered_w2000_2010.csv", sep = ";", header = TRUE, dec = ",")
length(unique(FinalDataset$Company)) #28,216 GUOs
#select and organize the columns we want
FinalDataset <- FinalDataset[,c(26,1,3:7,28,29,31,38,41)]

#exclude all companies that adopted AI before 2011;
CompWithAIB4_test2 <- FinalDataset[(FinalDataset$No_AI_PatentsYearGUOtotal>0 & FinalDataset$CurrentYear < 2011),]
length(unique(CompWithAIB4_test2$Company)) #1244 #thus, the second option is slightly more strict (2% less companies), and is the one we want

'%notin%' <- Negate('%in%')
Test<-FinalDataset[FinalDataset$Company %notin% CompWithAIB4_test2$Company,]
length(unique(CompWithAIB4_test2$Company)) #1244
length(unique(FinalDataset$Company)) #28216
length(unique(Test$Company)) #26972+1244 =28216, so we managed to clean companies that had AI patents before 2010 out
#of the dataset
FinalDataset <-Test
rm(Test, CompWithAIB4_test2)

#Create a variable that shows the first year that a company adopted AI
FinalDataset %<>% 
  group_by(Company) %>% 
  mutate(YearFirstAdoption = min(CurrentYear[which(No_AI_PatentsYearGUOtotal > 0)])) %>% 
  ungroup()

#replace infinite values by a factor named "Control";
FinalDataset$YearFirstAdoption[which(!is.finite(FinalDataset$YearFirstAdoption))] <- "Control"
table(FinalDataset$YearFirstAdoption)

#read relatedness values as Y;
#starting with the Relatedness of GUOs alone:

#read (old) indicators of relatedness for GUOs (these wrong indicators will be fixed on Section 3.), when I'll read them from "/Relatedness_corrected/" instead of /Relatedness/:
Relatedness_2010_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2010_Guos.csv", sep = ";", header = TRUE, dec=",")

Relatedness_2011_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2011_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2012_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2012_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2013_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2013_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2014_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2014_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2015_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2015_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2016_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2016_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2017_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2017_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2018_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2018_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2019_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2019_Guos.csv", sep = ";", header = TRUE, dec=",")

RelatednessGUOs <- rbind(Relatedness_2010_GUOs, Relatedness_2011_GUOs, Relatedness_2012_GUOs, Relatedness_2013_GUOs, Relatedness_2014_GUOs,
                         Relatedness_2015_GUOs, Relatedness_2016_GUOs, Relatedness_2017_GUOs, Relatedness_2018_GUOs,
                         Relatedness_2019_GUOs)
rm(Relatedness_2010_GUOs, Relatedness_2011_GUOs, Relatedness_2012_GUOs, Relatedness_2013_GUOs, Relatedness_2014_GUOs,
   Relatedness_2015_GUOs, Relatedness_2016_GUOs, Relatedness_2017_GUOs, Relatedness_2018_GUOs, Relatedness_2019_GUOs)
RelatednessGUOs <- RelatednessGUOs[,c(-1)]
names(RelatednessGUOs) <- c("Company", "Relatedness_GUOs","CurrentYear")
FinalDataset <- left_join(FinalDataset,RelatednessGUOs, by=c("Company","CurrentYear"),na_matches="never")
rm(RelatednessGUOs)

#Relatedness GUOs + subs:
Relatedness_2010_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2010_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")

Relatedness_2011_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2011_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2012_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2012_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2013_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2013_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2014_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2014_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2015_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2015_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2016_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2016_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2017_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2017_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2018_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2018_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2019_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2019_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")

RelatednessbothGuoAndSubs <- rbind(Relatedness_2010_bothGuoAndSubs, Relatedness_2011_bothGuoAndSubs, Relatedness_2012_bothGuoAndSubs, Relatedness_2013_bothGuoAndSubs, Relatedness_2014_bothGuoAndSubs,
                                   Relatedness_2015_bothGuoAndSubs, Relatedness_2016_bothGuoAndSubs, Relatedness_2017_bothGuoAndSubs, Relatedness_2018_bothGuoAndSubs,
                                   Relatedness_2019_bothGuoAndSubs)
rm(Relatedness_2010_bothGuoAndSubs, Relatedness_2011_bothGuoAndSubs, Relatedness_2012_bothGuoAndSubs, Relatedness_2013_bothGuoAndSubs, Relatedness_2014_bothGuoAndSubs,
   Relatedness_2015_bothGuoAndSubs, Relatedness_2016_bothGuoAndSubs, Relatedness_2017_bothGuoAndSubs, Relatedness_2018_bothGuoAndSubs, Relatedness_2019_bothGuoAndSubs)
RelatednessbothGuoAndSubs <- RelatednessbothGuoAndSubs[,c(-1)]
names(RelatednessbothGuoAndSubs) <- c("Company", "Relatedness_bothGuoAndSubs","CurrentYear")
FinalDataset <- left_join(FinalDataset,RelatednessbothGuoAndSubs, by=c("Company","CurrentYear"),na_matches="never")
rm(RelatednessbothGuoAndSubs)

write.csv2(FinalDataset, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_new_stock2000_2010.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
#2010 is included; + the addition of _stock2000_2010 means the stock also from 2000 to 2010

##1.2.Convert to wide format (which is separated later into 4 datasets, one for each complexity and relatedness) ----
rm(list=ls())
FinalDataset <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_new_stock2000_2010.csv", sep = ";", header = TRUE, dec=",")

#4.convert to wide format
FinalDataset <- FinalDataset[,c((-3),(-4), (-11))]
Companies <- distinct(FinalDataset, Company, .keep_all = TRUE)[,c((1),(2))] #26,972 companies
Companies1 <- Companies[c(1:5400),]
Companies2 <- Companies[c(5401:10800),]
Companies3 <- Companies[c(10801:16200),]
Companies4 <- Companies[c(16201:21600),]
Companies5 <- Companies[c(21601:26972),]

FinalDataset1<-FinalDataset[FinalDataset$Company %in% Companies1$Company,]
DataWide1 <- reshape(FinalDataset1, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies1,FinalDataset1)

FinalDataset2<-FinalDataset[FinalDataset$Company %in% Companies2$Company,]
DataWide2 <- reshape(FinalDataset2, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies2,FinalDataset2)

FinalDataset3<-FinalDataset[FinalDataset$Company %in% Companies3$Company,]
DataWide3 <- reshape(FinalDataset3, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies3,FinalDataset3)

FinalDataset4<-FinalDataset[FinalDataset$Company %in% Companies4$Company,]
DataWide4 <- reshape(FinalDataset4, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies4,FinalDataset4)

FinalDataset5<-FinalDataset[FinalDataset$Company %in% Companies5$Company,]
DataWide5 <- reshape(FinalDataset5, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies5,FinalDataset5)

DataWide <- rbind(DataWide1, DataWide2, DataWide3, DataWide4, DataWide5)
length(unique(DataWide$Company)) #26650
length(unique(FinalDataset$Company)) #26972, so the transformation from long to wide format lost 322 GUOs
write.csv2(DataWide, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_new.csv", row.names = F) 

#2.Matching -----
##2.1.Pre-matching ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#open directory above
setwd('..')
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_new.csv", sep = ";", header = TRUE, dec=",")

#Drop companies that don't have the variable Complexity_Subs in ANY (i.e., Complexity = NA for all periods) of 
#the considered periods;
Exclude <- DataWide[is.na(DataWide$Relatedness_bothGuoAndSubs.2010) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2011) == T &
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2012) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2013) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2014) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2015) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2016) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2017) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2018) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2019) == T,] 

#no company will be excluded in this analysis (i.e., all of them present relatedness for GUOs and Subs in at least one period)
#but, just for the sake of standarization:
'%notin%' <- Negate('%in%')
DataWide<-DataWide[DataWide$Company %notin% Exclude$Company,] #26,650 Subs left;
rm(Exclude)

table(DataWide$YearFirstAdoption != "Control") #1,757 GUOs with data available;

#check the most used Nace codes in control
AIcompanies <- DataWide[DataWide$YearFirstAdoption != "Control",]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] #Nace code 6201 leads, with 124 companies, followed by 5829 (99), 2611 (96), 
#6420 (87);6209 (59); 2120 (46); 2899 (46); 2630 (44); 2651 (44)
rm(FreqNacecodes, AIcompanies)

#drop missing data of any variables used (matching doesn't allow any missing variables)
DataWide <- DataWide[complete.cases(DataWide$Date_Incorporation), ] #just this line drop 5 Subs, the rest is ok
DataWide <- DataWide[complete.cases(DataWide$Size_class), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2010), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2011), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2012), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2013), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2014), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2015), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2016), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2017), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2018), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2019), ]
DataWide <- DataWide[complete.cases(DataWide$Nace_4d), ]

write.csv2(DataWide, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub.csv", row.names = F) 

##2.2.Matching ----
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub.csv", sep = ";", header = TRUE, dec=",")

#create category age 
Age<-as.data.frame(table(DataWide$Date_Incorporation))
DataWide$Agegroup = cut(DataWide$Date_Incorporation,c(0,1980,2000,2005,2010,2021))
#DataWide(data$Agegroup) = c("0-5","6-10",">10")
Age<-as.data.frame(table(DataWide$Agegroup))
rm(Age)

#create category other nace;
AIcompanies <- DataWide[DataWide$YearFirstAdoption != "Control",]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] 
FreqNacecodes_top <- FreqNacecodes[FreqNacecodes$Freq>=5,]
DataWide$CategoryNace <-ifelse(DataWide$Nace_4d %in% FreqNacecodes_top$Var1,DataWide$Nace_4d,"Less_used_codes")
rm(FreqNacecodes_top,AIcompanies,FreqNacecodes)

#2.4.2.3.1.Year 2011
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2011 <- DataWide[DataWide$YearFirstAdoption == "2011",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2011$treat <- 1

#rbind both
DataWide2011 <- rbind(DataWide2011,DataWideControl)

#build group variable:
DataWide2011$Group <- "2011"
#calculate the mean number of patents:
DataWide2011 <- mutate(DataWide2011, Mean_patents = 
                         rowMeans(select(DataWide2011, c(NoPatentsYearGUOtotal.2006, NoPatentsYearGUOtotal.2007,NoPatentsYearGUOtotal.2008, NoPatentsYearGUOtotal.2009, 
                                                         NoPatentsYearGUOtotal.2010)), na.rm = TRUE)) 
m.out1_data_caliper2011 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2011,
                                   data = DataWide2011, method = "genetic", caliper = .07, distance = "cbps", #.005 also seems good, 107 matched
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)
summary(m.out1_data_caliper2011, un = T)
#caliper .001 with no distance seems to work better (although just 30 companies are matched then)
#.005 works even better

m.data1_data_caliper2011 <- match.data(data= DataWide2011, m.out1_data_caliper2011)
rm(DataWide2011, DataWideControl)

test <- m.data1_data_caliper2011[m.data1_data_caliper2011$subclass == 20,c("Company","Mean_patents", "NoPatentsYearGUOtotal.2011", "subclass")]
test5 <- m.data1_data_caliper2011[,c("Company","treat","Mean_patents", "NoPatentsYearGUOtotal.2011", "subclass", "Nace_4d")]
rm(test, test5)

#2.4.2.3.2. Year 2012
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2012 <- DataWide[DataWide$YearFirstAdoption == "2012",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2012$treat <- 1

#rbind both
DataWide2012 <- rbind(DataWide2012,DataWideControl)

#build group variable:
DataWide2012$Group <- "2012"

#calculate the mean number of patents:
DataWide2012 <- mutate(DataWide2012, Mean_patents = 
                         rowMeans(select(DataWide2012, c(NoPatentsYearGUOtotal.2007,NoPatentsYearGUOtotal.2008, NoPatentsYearGUOtotal.2009, 
                                                         NoPatentsYearGUOtotal.2010, NoPatentsYearGUOtotal.2011)), na.rm = TRUE)) 
m.out1_data_caliper2012 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2012,
                                   data = DataWide2012, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2012, un = T)
m.data1_data_caliper2012 <- match.data(data= DataWide2012, m.out1_data_caliper2012)
rm(DataWide2012, DataWideControl)

#2.4.2.3.3. Year 2013
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2013 <- DataWide[DataWide$YearFirstAdoption == "2013",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2013$treat <- 1

#rbind both
DataWide2013 <- rbind(DataWide2013,DataWideControl)

#build group variable:
DataWide2013$Group <- "2013"
#calculate the mean number of patents:
DataWide2013 <- mutate(DataWide2013, Mean_patents = 
                         rowMeans(select(DataWide2013, c(NoPatentsYearGUOtotal.2008, NoPatentsYearGUOtotal.2009, NoPatentsYearGUOtotal.2010,
                                                         NoPatentsYearGUOtotal.2011,NoPatentsYearGUOtotal.2012)), na.rm = TRUE)) 
m.out1_data_caliper2013 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2013,
                                   data = DataWide2013, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2013, un = T)
m.data1_data_caliper2013 <- match.data(data= DataWide2013, m.out1_data_caliper2013)
rm(DataWide2013, DataWideControl)

#2.4.2.3.4. Year 2014
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2014 <- DataWide[DataWide$YearFirstAdoption == "2014",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2014$treat <- 1

#rbind both
DataWide2014 <- rbind(DataWide2014,DataWideControl)

#build group variable:
DataWide2014$Group <- "2014"
#calculate the mean number of patents:
DataWide2014 <- mutate(DataWide2014, Mean_patents = 
                         rowMeans(select(DataWide2014, c(NoPatentsYearGUOtotal.2009, NoPatentsYearGUOtotal.2010,NoPatentsYearGUOtotal.2011,
                                                         NoPatentsYearGUOtotal.2012, NoPatentsYearGUOtotal.2013)), na.rm = TRUE)) 
m.out1_data_caliper2014 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2014,
                                   data = DataWide2014, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2014, un = T)
m.data1_data_caliper2014 <- match.data(data= DataWide2014, m.out1_data_caliper2014)
rm(DataWide2014, DataWideControl)

#2.4.2.3.5. Year 2015
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2015 <- DataWide[DataWide$YearFirstAdoption == "2015",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2015$treat <- 1

#rbind both
DataWide2015 <- rbind(DataWide2015,DataWideControl)

#build group variable:
DataWide2015$Group <- "2015"
#calculate the mean number of patents:
DataWide2015 <- mutate(DataWide2015, Mean_patents = 
                         rowMeans(select(DataWide2015, c(NoPatentsYearGUOtotal.2010,NoPatentsYearGUOtotal.2011, NoPatentsYearGUOtotal.2012,
                                                         NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014)), na.rm = TRUE)) 
m.out1_data_caliper2015 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2015,
                                   data = DataWide2015, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2015, un = T)
m.data1_data_caliper2015 <- match.data(data= DataWide2015, m.out1_data_caliper2015)
rm(DataWide2015, DataWideControl)

#2.4.2.3.6. Year 2016
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2016 <- DataWide[DataWide$YearFirstAdoption == "2016",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2016$treat <- 1

#rbind both
DataWide2016 <- rbind(DataWide2016,DataWideControl)

#build group variable:
DataWide2016$Group <- "2016"
#calculate the mean number of patents:
DataWide2016 <- mutate(DataWide2016, Mean_patents = 
                         rowMeans(select(DataWide2016, c(NoPatentsYearGUOtotal.2011, NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, 
                                                         NoPatentsYearGUOtotal.2014, NoPatentsYearGUOtotal.2015)), na.rm = TRUE)) 
m.out1_data_caliper2016 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2016,
                                   data = DataWide2016, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2016, un = T)
m.data1_data_caliper2016 <- match.data(data= DataWide2016, m.out1_data_caliper2016)
rm(DataWide2016, DataWideControl)

#2.4.2.3.7. Year 2017
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2017 <- DataWide[DataWide$YearFirstAdoption == "2017",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2017$treat <- 1

#rbind both
DataWide2017 <- rbind(DataWide2017,DataWideControl)

#build group variable:
DataWide2017$Group <- "2017"
#calculate the mean number of patents:
DataWide2017 <- mutate(DataWide2017, Mean_patents = 
                         rowMeans(select(DataWide2017, c(NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,
                                                         NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016)), na.rm = TRUE)) 
m.out1_data_caliper2017<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2017,
                                  data = DataWide2017, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2017, un = T)
m.data1_data_caliper2017 <- match.data(data= DataWide2017, m.out1_data_caliper2017)
rm(DataWide2017, DataWideControl)

#2.4.2.3.8. Year 2018
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2018 <- DataWide[DataWide$YearFirstAdoption == "2018",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2018$treat <- 1

#rbind both
DataWide2018 <- rbind(DataWide2018,DataWideControl)

#build group variable:
DataWide2018$Group <- "2018"
#calculate the mean number of patents:
DataWide2018 <- mutate(DataWide2018, Mean_patents = 
                         rowMeans(select(DataWide2018, c(NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, 
                                                         NoPatentsYearGUOtotal.2016, NoPatentsYearGUOtotal.2017)), na.rm = TRUE)) 
m.out1_data_caliper2018<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2018,
                                  data = DataWide2018, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2018, un = T)
m.data1_data_caliper2018 <- match.data(data= DataWide2018, m.out1_data_caliper2018)
rm(DataWide2018, DataWideControl)

#2.4.2.3.9. Year 2019
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2019 <- DataWide[DataWide$YearFirstAdoption == "2019",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2019$treat <- 1

#rbind both
DataWide2019 <- rbind(DataWide2019,DataWideControl)

#build group variable:
DataWide2019$Group <- "2019"
#calculate the mean number of patents:
DataWide2019 <- mutate(DataWide2019, Mean_patents = 
                         rowMeans(select(DataWide2019, c(NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016,
                                                         NoPatentsYearGUOtotal.2017, NoPatentsYearGUOtotal.2018)), na.rm = TRUE)) 
m.out1_data_caliper2019<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2019,
                                  data = DataWide2019, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2019, un = T)
m.data1_data_caliper2019 <- match.data(data= DataWide2019, m.out1_data_caliper2019)
rm(DataWide2019, DataWideControl)

#2.4.2.3.10. Put everything together and save
m.data1_data_caliperMatch <- rbind(m.data1_data_caliper2011, m.data1_data_caliper2012, m.data1_data_caliper2013,
                                   m.data1_data_caliper2014, m.data1_data_caliper2015, m.data1_data_caliper2016,
                                   m.data1_data_caliper2017, m.data1_data_caliper2018, m.data1_data_caliper2019)
write.csv2(m.data1_data_caliperMatch, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS.csv", row.names = F)
table(m.data1_data_caliperMatch$treat == 1) #1441 companies matched (before it was 1155 companies, but there were 5 times more control companies)
m.out0_data <- matchit(treat ~ Agegroup + Size_class + NoPatentsYearGUOtotal.2010 + NoPatentsYearGUOtotal.2011 + 
                         NoPatentsYearGUOtotal.2012 + NoPatentsYearGUOtotal.2013 + NoPatentsYearGUOtotal.2014 + NoPatentsYearGUOtotal.2015 +
                         NoPatentsYearGUOtotal.2016 + NoPatentsYearGUOtotal.2017 + NoPatentsYearGUOtotal.2018 + NoPatentsYearGUOtotal.2019 +
                         Nace_4d, data = m.data1_data_caliperMatch, method = NULL, distance = "cbps") 
# Check balance of this match
summary(m.out0_data)

####2.2.1.Transform data from wide back into long format----
rm(list=ls())
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS.csv", sep = ";", header = TRUE, dec=",")
#drop mean column, so that we don't have to change anything in the command after:
DataWide <- subset(DataWide, select = -c(Mean_patents) )

DataLong <- melt(setDT(DataWide), 
                 measure.vars=list(c(8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84), 
                                   c(9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85),
                                   c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86), 
                                   c(11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79, 83, 87)),
                 variable.name='LongIdentifier', value.name=c('NoPatentsYearGUOtotal', 'No_AI_PatentsYearGUOtotal',
                                                              'Relatedness_GUOs','RelatednessGUosandSubs'))[,LongIdentifier:= paste0('f',LongIdentifier)][order(Company)]


DataLong$CurrentYear<-rep(c(2000:2019),times=nrow(DataLong)/20)
table(DataLong$CurrentYear)

length(unique(DataWide$Company)) #2475 (before it was 2732 with the CategoryNace variable instead of the Nace_4d)
length(unique(DataLong$Company)) #2475

#and save it
write.csv2(DataLong, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS.csv", row.names = F)

####2.2.2.Prepare data ----
rm(list=ls())
DataLong_Subs <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS.csv", sep = ";", header = TRUE, dec=",")
DataLong_Subs$first.treat <- as.numeric(ifelse(DataLong_Subs$YearFirstAdoption=="Control",0,DataLong_Subs$YearFirstAdoption))

#fix current year: pick the last digit after fff and add 1999 to it:
DataLong_Subs$CurrentYear <- as.numeric(str_extract(DataLong_Subs$LongIdentifier, "\\-*\\d+\\.*\\d*")) + 1999

table(DataLong_Subs$CurrentYear)

#produce some short descriptives about nace codes:
DataLong_Subs_desc <- DataLong_Subs[DataLong_Subs$CurrentYear==2019,]
AIcompanies <- DataLong_Subs_desc[DataLong_Subs_desc$treat == 1,]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
rm(FreqNacecodes, DataLong_Subs_desc, AIcompanies)

#exclude data before 2006:
DataLong_Subs <- DataLong_Subs[DataLong_Subs$CurrentYear>2005,]
table(DataLong_Subs$CurrentYear)
table(DataLong_Subs$LongIdentifier)

DataLong_Subs %<>%
  group_by(Company, CurrentYear) %>%
  mutate(SeqOwner = seq_along(Company))

#create unique id for every Company;
DataLong_Subs$id <- paste0(DataLong_Subs$Company,DataLong_Subs$Group,DataLong_Subs$SeqOwner)
length(unique(DataLong_Subs$id)) #2606 

DataLong_Subs$id <- as.numeric(factor(DataLong_Subs$id, 
                                      levels=unique(DataLong_Subs$id)))
length(unique(DataLong_Subs$id)) #2606
table(DataLong_Subs$id)

DataLong_Subs_sim <- DataLong_Subs

#read new indicators of relatedness:
DataRel_2006 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2006_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2007 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2007_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2008 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2008_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2009 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2009_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2010 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2010_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2011 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2011_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2012 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2012_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2013 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2013_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2014 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2014_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2015 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2015_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2016 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2016_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2017 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2017_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2018 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2018_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2019 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2019_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2006$CurrentYear <- 2006
DataRel_2007$CurrentYear <- 2007
DataRel_2008$CurrentYear <- 2008
DataRel_2009$CurrentYear <- 2009

DataRel_2010$CurrentYear <- 2010
DataRel_2011$CurrentYear <- 2011
DataRel_2012$CurrentYear <- 2012
DataRel_2013$CurrentYear <- 2013
DataRel_2014$CurrentYear <- 2014
DataRel_2015$CurrentYear <- 2015
DataRel_2016$CurrentYear <- 2016
DataRel_2017$CurrentYear <- 2017
DataRel_2018$CurrentYear <- 2018
DataRel_2019$CurrentYear <- 2019

DataRel <- rbind(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009,
                 DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, DataRel_2015, DataRel_2016, DataRel_2017,
                 DataRel_2018, DataRel_2019)
rm(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009, DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, 
   DataRel_2015, DataRel_2016, DataRel_2017, DataRel_2018, DataRel_2019)

DataRel %<>% 
  rename(Company = X)

#replace NAs by 0 (they refer to NaN values that were generated for firms that had no specialization)
table(is.na(DataRel$Relatedness_Association))
DataRel[is.na(DataRel)] <- 0

#merge data to the current dataset
DataLong_Subs_sim <- left_join(DataLong_Subs_sim,DataRel, by=c("Company","CurrentYear"),na_matches="never")

#fill missing data with the relatedness from the previous year;
DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Association)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jaccard)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cosine)) %>% 
  ungroup

DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Assoc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jacc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cos2, fromLast = TRUE)) %>% 
  ungroup
table(is.na(DataLong_Subs_sim$Relatedness_Assoc2)) #all False, as expected;

test<-DataLong_Subs_sim[DataLong_Subs_sim$Group == 2011,]
test2<-test[test$treat == 0,]
test3<-test[test$treat == 1,]
mean(test2$Relatedness_Assoc2) #nontreated; 6.417819
mean(test3$Relatedness_Assoc2) #treated; 8.142857
rm(test,test2,test3)

#calculate the mean relatedness of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Relatedness_Cos2), by = treat ] #or Relatedness_Assoc2 or Relatedness_Jacc2
#11.04720 for 0 and 11.78155 for 1
rm(DataLong_Subs_sim3)

TreatedUnits <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
length(unique(TreatedUnits$id)) #1303 
rm(TreatedUnits)

#the code below could overwrite the original one, where I excluded Companies ids, so I'll save it twice
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(DataLong_Subs_sim, file = "Input_code/Matched_companies.csv", row.names = F)

#Second part - New match for AI acquisition -----
#3.Pre-matching General ####
###3.1.Select possible dataset ----
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
#pick GUOs that appear in the filtered dataset
FinalDataset <- read.csv("files_created_code2/FinalDataset_allYears_FullGUOs_filtered_w2000_2010.csv", sep = ";", header = TRUE, dec = ",")
length(unique(FinalDataset$Company)) #28,216 GUOs
#select and organize the columns we want
FinalDataset <- FinalDataset[,c(26,1,3:7,28,29,31,38,41)]

#exclude all companies that adopted AI before 2011;
CompWithAIB4_test2 <- FinalDataset[(FinalDataset$No_AI_PatentsYearGUOtotal>0 & FinalDataset$CurrentYear < 2011),]
length(unique(CompWithAIB4_test2$Company)) #1244 #thus, the second option is slightly more strict (2% less companies), and is the one we want

'%notin%' <- Negate('%in%')
Test<-FinalDataset[FinalDataset$Company %notin% CompWithAIB4_test2$Company,]
length(unique(CompWithAIB4_test2$Company)) #1244
length(unique(FinalDataset$Company)) #28216
length(unique(Test$Company)) #26972+1244 =28216, so we managed to clean companies that had AI patents before 2010 out
#of the dataset
FinalDataset <-Test
rm(Test, CompWithAIB4_test2)

##3.2.Add acquisition data-----
#let's read the acquisition data:
All_2011 <-fread("files_created_code2/Data_patent_acquis_2011.csv")
All_2011$CurrentYear <- 2011

All_2012 <-fread("files_created_code2/Data_patent_acquis_2012.csv")
All_2012$CurrentYear <- 2012

All_2013 <-fread("files_created_code2/Data_patent_acquis_2013.csv")
All_2013$CurrentYear <- 2013

All_2014 <-fread("files_created_code2/Data_patent_acquis_2014.csv")
All_2014$CurrentYear <- 2014

All_2015 <-fread("files_created_code2/Data_patent_acquis_2015.csv")
All_2015$CurrentYear <- 2015

All_2016 <-fread("files_created_code2/Data_patent_acquis_2016.csv")
All_2016$CurrentYear <- 2016

All_2017 <-fread("files_created_code2/Data_patent_acquis_2017.csv")
All_2017$CurrentYear <- 2017

All_2018 <-fread("files_created_code2/Data_patent_acquis_2018.csv")
All_2018$CurrentYear <- 2018

All_2019 <-fread("files_created_code2/Data_patent_acquis_2019.csv")
All_2019$CurrentYear <- 2019

All_acquisition <- rbind(All_2011,All_2012,All_2013,All_2014,All_2015,All_2016,All_2017,All_2018,All_2019)
rm(All_2011,All_2012,All_2013,All_2014,All_2015,All_2016,All_2017,All_2018,All_2019)

colnames(All_acquisition)[which(names(All_acquisition) == "Owner")] <- "Company"

#now we merge it with the dataset:
FinalDataset <- left_join(FinalDataset, All_acquisition, by = c("Company", "CurrentYear"))
#replace NA values by 0 for these 4 new columns
FinalDataset <- FinalDataset %>%
  mutate(N_acquisitions_headquarters = replace_na(N_acquisitions_headquarters, 0),
    N_acquisitions_subsidiaries = replace_na(N_acquisitions_subsidiaries, 0),
    N_AI_patents_acquired_headquarters = replace_na(N_AI_patents_acquired_headquarters, 0),
    N_AI_patents_acquired_subsidiaries = replace_na(N_AI_patents_acquired_subsidiaries, 0))

#look at the data and define what will be the treatment. For now: companies that bought an AI patent, but never developed one;
#one alternative could be companies that bought one but never got any granted, but this one makes less sense because it would 
#basically lead to comparing again a granted (the one bought) against non granted
FinalDataset <- FinalDataset %>% group_by(Company, CurrentYear) %>% 
  mutate(Self_dev_min_acquis = No_AI_PatentsYearGUOtotal - N_AI_patents_acquired_headquarters - N_AI_patents_acquired_subsidiaries)
  #mutate(Acquisition_but_no_self_dev = replace_na(N_acquisitions_headquarters, 0))
test <-FinalDataset[FinalDataset$Self_dev_min_acquis <=0,]
test <- test[test$No_AI_PatentsYearGUOtotal>0,]
test2 <- FinalDataset[FinalDataset$Company == "CA192628338L",]
test3 <- FinalDataset[FinalDataset$N_AI_patents_acquired_headquarters >0,]
test4 <- FinalDataset[FinalDataset$N_AI_patents_acquired_subsidiaries >0,]
length(unique(test3$Company)) #38 hq acquisitions
length(unique(test4$Company)) #1 subsidiary acquisition

test5 <- FinalDataset[FinalDataset$Self_dev_min_acquis > 0,]
length(unique(test5$Company))
length(unique(FinalDataset$Company)) #out of 26972 companies, just 1791 acquired AI patents;

#create a new variable
FinalDataset <- FinalDataset %>%
  arrange(Company, CurrentYear) %>%  # Ensure data is sorted correctly
  group_by(Company) %>%
  mutate(Cum_N_AI_patents_headquarters = cumsum(N_AI_patents_acquired_headquarters),
    Cum_N_AI_patents_subsidiaries = cumsum(N_AI_patents_acquired_subsidiaries)) %>%
  ungroup()

test3 <- FinalDataset[FinalDataset$N_AI_patents_acquired_headquarters >0,] #US812983623
test1 <- FinalDataset[FinalDataset$Company == "US163795503L",]

#create another variable
FinalDataset$Diff_AI_Self_dev_min_acquis <- (FinalDataset$No_AI_PatentsYearGUOtotal) - 
  (FinalDataset$Cum_N_AI_patents_headquarters + FinalDataset$Cum_N_AI_patents_subsidiaries) #GB02415211

test1 <- FinalDataset[FinalDataset$Company == "GB02415211",]  #probably is negative because the company sold the patent, or it expired;
test2 <- All_acquisition[All_acquisition$Company == "GB02415211",] 
ChangeInOwnership_clean <- fread("files_created_code2/ChangeInOwnership_clean.csv")
ChangeInOwnership_clean <- ChangeInOwnership_clean[complete.cases(ChangeInOwnership_clean$Owner), ]
length(unique(ChangeInOwnership_clean$Publication_number)) #356,246 patents with identifiable owner;
test7 <- ChangeInOwnership_clean[ChangeInOwnership_clean$Owner %in% FinalDataset$Company,]
length(unique(test7$Publication_number)) #in the best case, I'd have 24358 patents linked to my dataset
test8 <- distinct(test7, Publication_number, .keep_all = TRUE)
table(test8$CurrentYear)
table(test8$Transaction_year)
test4 <- ChangeInOwnership_clean[ChangeInOwnership_clean$Owner == "GB02415211",] 
length(unique(test4$Publication_number)) #11 patents total acquired
length(unique(test4$Publication_number[test4$Transaction_year == 2016])) #7 patents acquired HQ in 2016
length(unique(test4$Publication_number[test4$Transaction_year == 2018])) #11 patents acquired in 2018 (which indicates a double transaction registered by orbis for at least 4 patents)
length(unique(test4$Publication_number[test4$AI_patent == "Yes"])) #3 AI patents

test5 <- FinalDataset[FinalDataset$N_acquisitions_headquarters >0,]
length(unique(test5$Company)) #2607 companies  that did some acquisition at the HQ
test6 <- FinalDataset[FinalDataset$N_acquisitions_subsidiaries >0,]
length(unique(test6$Company))  #18 companies only that did some acquisition at the subsidiary level;

rm(test, test1, test2, test3, test4, test5, test6, test7, test8)
#test3<- ChangeInOwnership_clean[ChangeInOwnership_clean$Owner == "CA192628338L",] #it's correct now;

#attention here------
#now 
test3 <- FinalDataset[FinalDataset$Cum_N_AI_patents_headquarters >0 | FinalDataset$Cum_N_AI_patents_subsidiaries >0,] 
length(unique(test3$Company)) #39 companies total
test3 <- FinalDataset[FinalDataset$Company %in% test3$Company,]
test3 <- test3[,c("Company", "CurrentYear", "No_AI_PatentsYearGUOtotal", "N_AI_patents_acquired_headquarters",
                  "N_AI_patents_acquired_subsidiaries", "Diff_AI_Self_dev_min_acquis","Cum_N_AI_patents_headquarters", "Cum_N_AI_patents_subsidiaries")]

#filter out companies where No_AI_PatentsYearGUOtotal was greater than 0 before they ever 
#acquired an AI patent (N_AI_patents_acquired_headquarters or N_AI_patents_acquired_subsidiaries)

# Step 1: Identify the first year each company acquired an AI patent
company_first_acquisition <- test3 %>%
  group_by(Company) %>%
  summarise(
    first_acquisition_year = min(CurrentYear[N_AI_patents_acquired_headquarters > 0 | N_AI_patents_acquired_subsidiaries > 0], na.rm = TRUE)
  )

# Step 2: Identify companies that self-developed AI patents before acquiring any
companies_to_exclude <- test3 %>%
  inner_join(company_first_acquisition, by = "Company") %>%
  filter(CurrentYear < first_acquisition_year & No_AI_PatentsYearGUOtotal > 0) %>%
  pull(Company) %>% unique()

# Step 3: Filter out the identified companies
filtered_test3 <- test3 %>%
  filter(!Company %in% companies_to_exclude)

test<- test3[test3$Company == "GB02415211",]
test<- filtered_test3[filtered_test3$Company == "CA30195NC",] #it works!
length(unique(filtered_test3$Company)) #just 10 left that bought before developing
#CA30195NC, GB02415211 acquired before

#so it seems that the treatment is buying an AI patent before developing one, with only 10 companies doing it  
#Create a variable that shows the first year that a company adopted AI
FinalDataset %<>% 
  group_by(Company) %>% 
  mutate(YearFirstAdoption = min(CurrentYear[N_AI_patents_acquired_headquarters > 0 | N_AI_patents_acquired_subsidiaries > 0], na.rm = TRUE)) %>% 
  ungroup()

#replace infinite values by a factor named "Control";
FinalDataset$YearFirstAdoption[which(!is.finite(FinalDataset$YearFirstAdoption))] <- "Control"
table(FinalDataset$YearFirstAdoption)

#now now "flag" the companies that can't be used, i.e., the ones that bought AI patents, but also developed:
FinalDataset <- FinalDataset %>%
  mutate(YearFirstAdoption = ifelse(Company %in% companies_to_exclude, "Control2", YearFirstAdoption))

table(FinalDataset$YearFirstAdoption)

#read relatedness values as Y;
#starting with the Relatedness of GUOs alone:

#read (old) indicators of relatedness for GUOs (these wrong indicators will be fixed on Section 3.), when I'll read them from "/Relatedness_corrected/" instead of /Relatedness/:
Relatedness_2010_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2010_Guos.csv", sep = ";", header = TRUE, dec=",")

Relatedness_2011_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2011_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2012_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2012_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2013_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2013_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2014_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2014_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2015_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2015_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2016_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2016_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2017_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2017_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2018_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2018_Guos.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2019_GUOs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2019_Guos.csv", sep = ";", header = TRUE, dec=",")

RelatednessGUOs <- rbind(Relatedness_2010_GUOs, Relatedness_2011_GUOs, Relatedness_2012_GUOs, Relatedness_2013_GUOs, Relatedness_2014_GUOs,
                         Relatedness_2015_GUOs, Relatedness_2016_GUOs, Relatedness_2017_GUOs, Relatedness_2018_GUOs,
                         Relatedness_2019_GUOs)
rm(Relatedness_2010_GUOs, Relatedness_2011_GUOs, Relatedness_2012_GUOs, Relatedness_2013_GUOs, Relatedness_2014_GUOs,
   Relatedness_2015_GUOs, Relatedness_2016_GUOs, Relatedness_2017_GUOs, Relatedness_2018_GUOs, Relatedness_2019_GUOs)
RelatednessGUOs <- RelatednessGUOs[,c(-1)]
names(RelatednessGUOs) <- c("Company", "Relatedness_GUOs","CurrentYear")
FinalDataset <- left_join(FinalDataset,RelatednessGUOs, by=c("Company","CurrentYear"),na_matches="never")
rm(RelatednessGUOs)

#Relatedness GUOs + subs:
Relatedness_2010_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2010_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")

Relatedness_2011_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2011_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2012_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2012_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2013_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2013_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2014_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2014_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2015_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2015_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2016_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2016_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2017_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2017_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2018_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2018_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2019_bothGuoAndSubs <- read.csv("Matheus/DataGenerated/Relatedness/Relatedness_2019_bothGuoAndSubs.csv", sep = ";", header = TRUE, dec=",")

RelatednessbothGuoAndSubs <- rbind(Relatedness_2010_bothGuoAndSubs, Relatedness_2011_bothGuoAndSubs, Relatedness_2012_bothGuoAndSubs, Relatedness_2013_bothGuoAndSubs, Relatedness_2014_bothGuoAndSubs,
                                   Relatedness_2015_bothGuoAndSubs, Relatedness_2016_bothGuoAndSubs, Relatedness_2017_bothGuoAndSubs, Relatedness_2018_bothGuoAndSubs,
                                   Relatedness_2019_bothGuoAndSubs)
rm(Relatedness_2010_bothGuoAndSubs, Relatedness_2011_bothGuoAndSubs, Relatedness_2012_bothGuoAndSubs, Relatedness_2013_bothGuoAndSubs, Relatedness_2014_bothGuoAndSubs,
   Relatedness_2015_bothGuoAndSubs, Relatedness_2016_bothGuoAndSubs, Relatedness_2017_bothGuoAndSubs, Relatedness_2018_bothGuoAndSubs, Relatedness_2019_bothGuoAndSubs)
RelatednessbothGuoAndSubs <- RelatednessbothGuoAndSubs[,c(-1)]
names(RelatednessbothGuoAndSubs) <- c("Company", "Relatedness_bothGuoAndSubs","CurrentYear")
FinalDataset <- left_join(FinalDataset,RelatednessbothGuoAndSubs, by=c("Company","CurrentYear"),na_matches="never")
rm(RelatednessbothGuoAndSubs)
write.csv2(FinalDataset, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_new_stock2000_2010_Acquis_match.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
#2010 is included; + the addition of _stock2000_2010 means the stock also from 2000 to 2010

##3.3.Convert to wide format (which is separated later into 4 datasets, one for each complexity and relatedness) ----
rm(list=ls())
FinalDataset <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_new_stock2000_2010_Acquis_match.csv", sep = ";", header = TRUE, dec=",")
#exclude new variables, that we can pick back later
FinalDataset <- subset(FinalDataset, select = -c(N_acquisitions_subsidiaries, N_AI_patents_acquired_subsidiaries, N_acquisitions_headquarters,
                                             N_AI_patents_acquired_headquarters, Self_dev_min_acquis, Cum_N_AI_patents_headquarters,
                                             Cum_N_AI_patents_subsidiaries, Diff_AI_Self_dev_min_acquis) )
#4.convert to wide format
FinalDataset <- FinalDataset[,c((-3),(-4), (-11))]
Companies <- distinct(FinalDataset, Company, .keep_all = TRUE)[,c((1),(2))] #26,972 companies
Companies1 <- Companies[c(1:5400),]
Companies2 <- Companies[c(5401:10800),]
Companies3 <- Companies[c(10801:16200),]
Companies4 <- Companies[c(16201:21600),]
Companies5 <- Companies[c(21601:26972),]

FinalDataset1<-FinalDataset[FinalDataset$Company %in% Companies1$Company,]
DataWide1 <- reshape(FinalDataset1, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies1,FinalDataset1)

FinalDataset2<-FinalDataset[FinalDataset$Company %in% Companies2$Company,]
DataWide2 <- reshape(FinalDataset2, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies2,FinalDataset2)

FinalDataset3<-FinalDataset[FinalDataset$Company %in% Companies3$Company,]
DataWide3 <- reshape(FinalDataset3, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies3,FinalDataset3)

FinalDataset4<-FinalDataset[FinalDataset$Company %in% Companies4$Company,]
DataWide4 <- reshape(FinalDataset4, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies4,FinalDataset4)

FinalDataset5<-FinalDataset[FinalDataset$Company %in% Companies5$Company,]
DataWide5 <- reshape(FinalDataset5, idvar = c("Name","Company", "Country", "Nace_4d", "Size_class", "Date_Incorporation", "YearFirstAdoption"), 
                     timevar="CurrentYear", direction = "wide")
rm(Companies5,FinalDataset5)

DataWide <- rbind(DataWide1, DataWide2, DataWide3, DataWide4, DataWide5)
length(unique(DataWide$Company)) #26650
length(unique(FinalDataset$Company)) #26972, so the transformation from long to wide format lost 322 GUOs
write.csv2(DataWide, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_new_Acquis_match.csv", row.names = F) 

#4.Matching -----
##4.1.Pre-matching ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#open directory above
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_new_Acquis_match.csv", sep = ";", header = TRUE, dec=",")

#Drop companies that don't have the variable Complexity_Subs in ANY (i.e., Complexity = NA for all periods) of 
#the considered periods;
Exclude <- DataWide[is.na(DataWide$Relatedness_bothGuoAndSubs.2010) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2011) == T &
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2012) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2013) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2014) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2015) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2016) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2017) == T & 
                      is.na(DataWide$Relatedness_bothGuoAndSubs.2018) == T & is.na(DataWide$Relatedness_bothGuoAndSubs.2019) == T,] 

#no company will be excluded in this analysis (i.e., all of them present relatedness for GUOs and Subs in at least one period)
#but, just for the sake of standarization:
'%notin%' <- Negate('%in%')
DataWide<-DataWide[DataWide$Company %notin% Exclude$Company,] #26,650 Subs left;
rm(Exclude)

table(DataWide$YearFirstAdoption != "Control") #1,757 GUOs with data available;

#check the most used Nace codes in control
AIcompanies <- DataWide[DataWide$YearFirstAdoption != "Control",]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] #Nace code 6201 leads, with 124 companies, followed by 5829 (99), 2611 (96), 
#6420 (87);6209 (59); 2120 (46); 2899 (46); 2630 (44); 2651 (44)
rm(FreqNacecodes, AIcompanies)

#drop missing data of any variables used (matching doesn't allow any missing variables)
DataWide <- DataWide[complete.cases(DataWide$Date_Incorporation), ] #just this line drop 5 Subs, the rest is ok
DataWide <- DataWide[complete.cases(DataWide$Size_class), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2010), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2011), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2012), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2013), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2014), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2015), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2016), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2017), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2018), ]
DataWide <- DataWide[complete.cases(DataWide$NoPatentsYearGUOtotal.2019), ]
DataWide <- DataWide[complete.cases(DataWide$Nace_4d), ]

write.csv2(DataWide, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub_Acquis_match.csv", row.names = F) 

##4.2. Matching successful from unsuccessful AI innovators-----
rm(list=ls())
DataLong_Subs_sim <-read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",")
length(unique(DataLong_Subs_sim$Company)) #2390
#insert granted information
Patent_data <- read.csv("Input_code/Big_files_ignore/Additional_data_patents/Resulting_data_patent_info.csv", header = TRUE) #
DataLong_Subs_sim<- left_join(DataLong_Subs_sim,Patent_data, by=c("Company", "CurrentYear"))

DataLong_Subs_sim %<>% group_by(Company) %>% mutate(total_granted_ai = max(n_granted_yes_ai, na.rm =T )) %>% ungroup()

Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]

Current_treated$Year_before_adoption <- as.integer(Current_treated$YearFirstAdoption) -1
Current_treated %<>% group_by(Company) %>% mutate(new_treat = ifelse(total_granted_ai==0,0,1) ) %>% ungroup()

#the matching will be based on: size class, age group, NACE4d;
rm(DataLong_Subs_sim)
length(unique(Current_treated$Company)) #1257

setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub_Acquis_match.csv", sep = ";", header = TRUE, dec=",")
#DROP FROM DATAWIDE EVERYTHING THAT IS NOT IN CURRENT TREATED,
DataWide<-DataWide[DataWide$Company %in% Current_treated$Company,]
#PICK UNIQUE COMPANIES FROM CURRENT TREATED, SELECT JUST THE NEW VARIABLES (YEAR BEFORE AND NEW TREAT), MATCH THEM TO DATAWIDE
test <- distinct(Current_treated, Company, .keep_all = TRUE)
test <- test[,c("Company", "Year_before_adoption", "new_treat", "total_granted_ai")]
DataWide <- left_join(DataWide,test, by = "Company")

#create category age 
Age<-as.data.frame(table(DataWide$Date_Incorporation))
DataWide$Agegroup = cut(DataWide$Date_Incorporation,c(0,1980,2000,2005,2010,2021))
#DataWide(data$Agegroup) = c("0-5","6-10",">10")
Age<-as.data.frame(table(DataWide$Agegroup))
rm(Age)

m.out1_data_caliper <- matchit(new_treat ~ Nace_4d + Agegroup + Size_class + Year_before_adoption, data = DataWide, method = "genetic", distance = "cbps", 
                               exact = ~ Nace_4d + Agegroup + Size_class + Year_before_adoption, ratio=1, std.caliper = T)
summary(m.out1_data_caliper, un = T) 
m.out1_data_caliper_panel <- match.data(data= DataWide, m.out1_data_caliper)
Save <- m.out1_data_caliper_panel[,c("Company", "subclass", "Year_before_adoption", "new_treat")] #"total_granted_ai",
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(Save, file = "Output_code/Data/Matched_Group2_applied_for_but_nonegranted_vs_granted.csv", row.names = F)

##4.3.Matching by quartile ----
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub_Acquis_match.csv", sep = ";", header = TRUE, dec=",")

#create category age 
Age<-as.data.frame(table(DataWide$Date_Incorporation))
DataWide$Agegroup = cut(DataWide$Date_Incorporation,c(0,1980,2000,2005,2010,2021))
#DataWide(data$Agegroup) = c("0-5","6-10",">10")
Age<-as.data.frame(table(DataWide$Agegroup))
rm(Age)

#create category other nace;
AIcompanies <- DataWide[DataWide$YearFirstAdoption != "Control",]
AIcompanies <- AIcompanies[AIcompanies$YearFirstAdoption != "Control2",]

FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] 
#FreqNacecodes_top <- FreqNacecodes[FreqNacecodes$Freq>=5,]
#DataWide$CategoryNace <-ifelse(DataWide$Nace_4d %in% FreqNacecodes_top$Var1,DataWide$Nace_4d,"Less_used_codes")
rm(AIcompanies,FreqNacecodes)

table(DataWide$YearFirstAdoption) #2014, 2016 a 2019; 8 in total, so I apaarently lost 2 due to lack of data;

#add the new distance measure
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
CategoriesNace <-read.csv("Input_code/Big_files_ignore/Additional_data_patents/new_distance_testing_ready3.csv")#, sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance
CategoriesNace <- CategoriesNace[,c(1,10)] #for top9, which is the final chosen one
names(CategoriesNace) <- c("Nace_4d", "Quartile")

DataWide<- left_join(DataWide, CategoriesNace, by = "Nace_4d")
table(is.na(DataWide$Quartile))
#replace missing values by Bottom:
DataWide$Quartile[is.na(DataWide$Quartile)] <- "Bottom"
#table(DataWide$Quartile)
#2.4.2.3.4. Year 2014
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2014 <- DataWide[DataWide$YearFirstAdoption == "2014",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2014$treat <- 1

#rbind both
DataWide2014 <- rbind(DataWide2014,DataWideControl)

#build group variable:
DataWide2014$Group <- "2014"
#calculate the mean number of patents:
DataWide2014 <- mutate(DataWide2014, Mean_patents = 
                         rowMeans(select(DataWide2014, c(NoPatentsYearGUOtotal.2009, NoPatentsYearGUOtotal.2010,NoPatentsYearGUOtotal.2011,
                                                         NoPatentsYearGUOtotal.2012, NoPatentsYearGUOtotal.2013)), na.rm = TRUE)) 
#now that we have very few companies, let's try a bigger ration
m.out1_data_caliper2014 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2014,
                                   data = DataWide2014, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Quartile + Agegroup + Size_class, ratio=50, std.caliper = T)

summary(m.out1_data_caliper2014, un = T) #no match, not even using ration 1
m.data1_data_caliper2014 <- match.data(data= DataWide2014, m.out1_data_caliper2014)
rm(DataWide2014, DataWideControl)

#2.4.2.3.6. Year 2016
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2016 <- DataWide[DataWide$YearFirstAdoption == "2016",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2016$treat <- 1

#rbind both
DataWide2016 <- rbind(DataWide2016,DataWideControl)

#build group variable:
DataWide2016$Group <- "2016"
#calculate the mean number of patents:
DataWide2016 <- mutate(DataWide2016, Mean_patents = 
                         rowMeans(select(DataWide2016, c(NoPatentsYearGUOtotal.2011, NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, 
                                                         NoPatentsYearGUOtotal.2014, NoPatentsYearGUOtotal.2015)), na.rm = TRUE)) 
m.out1_data_caliper2016 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2016,
                                   data = DataWide2016, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Quartile + Agegroup + Size_class, ratio=50, std.caliper = T)

summary(m.out1_data_caliper2016, un = T)
m.data1_data_caliper2016 <- match.data(data= DataWide2016, m.out1_data_caliper2016)
rm(DataWide2016, DataWideControl)

#2.4.2.3.7. Year 2017
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2017 <- DataWide[DataWide$YearFirstAdoption == "2017",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2017$treat <- 1

#rbind both
DataWide2017 <- rbind(DataWide2017,DataWideControl)

#build group variable:
DataWide2017$Group <- "2017"
#calculate the mean number of patents:
DataWide2017 <- mutate(DataWide2017, Mean_patents = 
                         rowMeans(select(DataWide2017, c(NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,
                                                         NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016)), na.rm = TRUE)) 
m.out1_data_caliper2017<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2017,
                                  data = DataWide2017, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Quartile + Agegroup + Size_class, ratio=50, std.caliper = T)

summary(m.out1_data_caliper2017, un = T)
m.data1_data_caliper2017 <- match.data(data= DataWide2017, m.out1_data_caliper2017)
rm(DataWide2017, DataWideControl)

#2.4.2.3.8. Year 2018
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2018 <- DataWide[DataWide$YearFirstAdoption == "2018",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2018$treat <- 1

#rbind both
DataWide2018 <- rbind(DataWide2018,DataWideControl)

#build group variable:
DataWide2018$Group <- "2018"
#calculate the mean number of patents:
DataWide2018 <- mutate(DataWide2018, Mean_patents = 
                         rowMeans(select(DataWide2018, c(NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, 
                                                         NoPatentsYearGUOtotal.2016, NoPatentsYearGUOtotal.2017)), na.rm = TRUE)) 
m.out1_data_caliper2018<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2018,
                                  data = DataWide2018, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Quartile + Agegroup + Size_class, ratio=50, std.caliper = T)

summary(m.out1_data_caliper2018, un = T)
m.data1_data_caliper2018 <- match.data(data= DataWide2018, m.out1_data_caliper2018)
rm(DataWide2018, DataWideControl)

#2.4.2.3.9. Year 2019
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2019 <- DataWide[DataWide$YearFirstAdoption == "2019",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2019$treat <- 1

#rbind both
DataWide2019 <- rbind(DataWide2019,DataWideControl)

#build group variable:
DataWide2019$Group <- "2019"
#calculate the mean number of patents:
DataWide2019 <- mutate(DataWide2019, Mean_patents = 
                         rowMeans(select(DataWide2019, c(NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016,
                                                         NoPatentsYearGUOtotal.2017, NoPatentsYearGUOtotal.2018)), na.rm = TRUE)) 
m.out1_data_caliper2019<- matchit(treat ~   Mean_patents + NoPatentsYearGUOtotal.2019, # +
                                  data = DataWide2019, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Quartile + Agegroup + Size_class, ratio=50, std.caliper = T)

summary(m.out1_data_caliper2019, un = T)
m.data1_data_caliper2019 <- match.data(data= DataWide2019, m.out1_data_caliper2019)
rm(DataWide2019, DataWideControl)

#2.4.2.3.10. Put everything together and save
m.data1_data_caliperMatch <- rbind(m.data1_data_caliper2014, m.data1_data_caliper2016,m.data1_data_caliper2017, m.data1_data_caliper2018, m.data1_data_caliper2019)
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
write.csv2(m.data1_data_caliperMatch, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match3.csv", row.names = F)
table(m.data1_data_caliperMatch$treat == 1) #8 from 8 available matched now, from 3 before; for ratio = 50, 6 are matched out of 8
table(m.data1_data_caliperMatch$Group) #I'm missing one in 2018 and 1 in 2019 when ratio = 50;
table(DataWide$YearFirstAdoption) #
m.out0_data <- matchit(treat ~ Agegroup + Size_class + NoPatentsYearGUOtotal.2010 + NoPatentsYearGUOtotal.2011 + 
                         NoPatentsYearGUOtotal.2012 + NoPatentsYearGUOtotal.2013 + NoPatentsYearGUOtotal.2014 + NoPatentsYearGUOtotal.2015 +
                         NoPatentsYearGUOtotal.2016 + NoPatentsYearGUOtotal.2017 + NoPatentsYearGUOtotal.2018 + NoPatentsYearGUOtotal.2019 +
                         Quartile, data = m.data1_data_caliperMatch, method = NULL, distance = "cbps") 
# Check balance of this match
summary(m.out0_data)

##4.4.Matching by NACE (instead of distance)----
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/FinalDataset_withYs_wide_Relatedness_GuoSub_Acquis_match.csv", sep = ";", header = TRUE, dec=",")

#create category age 
Age<-as.data.frame(table(DataWide$Date_Incorporation))
DataWide$Agegroup = cut(DataWide$Date_Incorporation,c(0,1980,2000,2005,2010,2021))
#DataWide(data$Agegroup) = c("0-5","6-10",">10")
Age<-as.data.frame(table(DataWide$Agegroup))
rm(Age)

#create category other nace;
AIcompanies <- DataWide[DataWide$YearFirstAdoption != "Control",]
AIcompanies <- AIcompanies[AIcompanies$YearFirstAdoption != "Control2",]

FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] 
#FreqNacecodes_top <- FreqNacecodes[FreqNacecodes$Freq>=5,]
#DataWide$CategoryNace <-ifelse(DataWide$Nace_4d %in% FreqNacecodes_top$Var1,DataWide$Nace_4d,"Less_used_codes")
rm(AIcompanies,FreqNacecodes)

table(DataWide$YearFirstAdoption) #2014, 2016 a 2019; 8 in total, so I apaarently lost 2 due to lack of data;

#add the new distance measure
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
CategoriesNace <-read.csv("Input_code/Big_files_ignore/Additional_data_patents/new_distance_testing_ready3.csv")#, ~ Nace_4d + Agegroup + Size_class, ratio=1, std.caliper = T)
CategoriesNace <- CategoriesNace[,c(1,10)] #for top9, which is the final chosen one
names(CategoriesNace) <- c("Nace_4d", "Quartile")

DataWide<- left_join(DataWide, CategoriesNace, by = "Nace_4d")
table(is.na(DataWide$Quartile))
#replace missing values by Bottom:
DataWide$Quartile[is.na(DataWide$Quartile)] <- "Bottom"
DataWide$Nace_4d <- as.character(DataWide$Nace_4d)
DataWide$Nace_2d <- substr(DataWide$Nace_4d, 1, 2)
table(DataWide$Nace_2d )
#2.4.2.3.4. Year 2014
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2014 <- DataWide[DataWide$YearFirstAdoption == "2014",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2014$treat <- 1
table(DataWideControl$Nace_2d) 
table(DataWide2014$Nace_2d) 
#rbind both
DataWide2014 <- rbind(DataWide2014,DataWideControl)

#build group variable:
DataWide2014$Group <- "2014"

#calculate the mean number of patents:
DataWide2014 <- mutate(DataWide2014, Mean_patents = 
                         rowMeans(select(DataWide2014, c(NoPatentsYearGUOtotal.2009, NoPatentsYearGUOtotal.2010,NoPatentsYearGUOtotal.2011,
                                                         NoPatentsYearGUOtotal.2012, NoPatentsYearGUOtotal.2013)), na.rm = TRUE)) 
#now that we have very few companies, let's try a bigger ration
m.out1_data_caliper2014 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2014,
                                   data = DataWide2014, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_2d + Agegroup + Size_class, ratio=5, std.caliper = T)

summary(m.out1_data_caliper2014, un = T) #no match, not even using ration 1
m.data1_data_caliper2014 <- match.data(data= DataWide2014, m.out1_data_caliper2014)
rm(DataWide2014, DataWideControl)

#2.4.2.3.6. Year 2016
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2016 <- DataWide[DataWide$YearFirstAdoption == "2016",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2016$treat <- 1

#rbind both
DataWide2016 <- rbind(DataWide2016,DataWideControl)

#build group variable:
DataWide2016$Group <- "2016"
#calculate the mean number of patents:
DataWide2016 <- mutate(DataWide2016, Mean_patents = 
                         rowMeans(select(DataWide2016, c(NoPatentsYearGUOtotal.2011, NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, 
                                                         NoPatentsYearGUOtotal.2014, NoPatentsYearGUOtotal.2015)), na.rm = TRUE)) 
m.out1_data_caliper2016 <- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2016,
                                   data = DataWide2016, method = "genetic", caliper = .07, distance = "cbps", 
                                   exact = ~ Nace_2d + Agegroup + Size_class, ratio=5, std.caliper = T)

summary(m.out1_data_caliper2016, un = T)
m.data1_data_caliper2016 <- match.data(data= DataWide2016, m.out1_data_caliper2016)
rm(DataWide2016, DataWideControl)

#2.4.2.3.7. Year 2017
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2017 <- DataWide[DataWide$YearFirstAdoption == "2017",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2017$treat <- 1

#rbind both
DataWide2017 <- rbind(DataWide2017,DataWideControl)

#build group variable:
DataWide2017$Group <- "2017"
#calculate the mean number of patents:
DataWide2017 <- mutate(DataWide2017, Mean_patents = 
                         rowMeans(select(DataWide2017, c(NoPatentsYearGUOtotal.2012,NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,
                                                         NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016)), na.rm = TRUE)) 
m.out1_data_caliper2017<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2017,
                                  data = DataWide2017, method = "genetic", caliper = .07, distance = "cbps", 
                                  exact = ~ Nace_2d + Agegroup + Size_class, ratio=5, std.caliper = T)

summary(m.out1_data_caliper2017, un = T)
m.data1_data_caliper2017 <- match.data(data= DataWide2017, m.out1_data_caliper2017)
rm(DataWide2017, DataWideControl)

#2.4.2.3.8. Year 2018
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2018 <- DataWide[DataWide$YearFirstAdoption == "2018",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2018$treat <- 1

#rbind both
DataWide2018 <- rbind(DataWide2018,DataWideControl)

#build group variable:
DataWide2018$Group <- "2018"
#calculate the mean number of patents:
DataWide2018 <- mutate(DataWide2018, Mean_patents = 
                         rowMeans(select(DataWide2018, c(NoPatentsYearGUOtotal.2013, NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, 
                                                         NoPatentsYearGUOtotal.2016, NoPatentsYearGUOtotal.2017)), na.rm = TRUE)) 
m.out1_data_caliper2018<- matchit(treat ~  Mean_patents + NoPatentsYearGUOtotal.2018,
                                  data = DataWide2018, method = "genetic", caliper = .5, distance = "cbps", 
                                  exact = ~ Nace_2d + Agegroup + Size_class, ratio=1, std.caliper = T)

summary(m.out1_data_caliper2018, un = T)
m.data1_data_caliper2018 <- match.data(data= DataWide2018, m.out1_data_caliper2018)
rm(DataWide2018, DataWideControl)

#2.4.2.3.9. Year 2019
DataWideControl <- DataWide[DataWide$YearFirstAdoption == "Control",]
DataWide2019 <- DataWide[DataWide$YearFirstAdoption == "2019",]

#define control and treatment
DataWideControl$treat <- 0
DataWide2019$treat <- 1
table(DataWide2019$Nace_2d)
table(DataWideControl$Nace_2d)
#rbind both
DataWide2019 <- rbind(DataWide2019,DataWideControl)

#build group variable:
DataWide2019$Group <- "2019"
#calculate the mean number of patents:
DataWide2019 <- mutate(DataWide2019, Mean_patents = 
                         rowMeans(select(DataWide2019, c(NoPatentsYearGUOtotal.2014,NoPatentsYearGUOtotal.2015, NoPatentsYearGUOtotal.2016,
                                                         NoPatentsYearGUOtotal.2017, NoPatentsYearGUOtotal.2018)), na.rm = TRUE)) 
m.out1_data_caliper2019<- matchit(treat ~   Mean_patents + NoPatentsYearGUOtotal.2019, # +
                                  data = DataWide2019, method = "genetic", caliper = .5, distance = "cbps",  # 
                                  exact = ~ Nace_2d + Agegroup + Size_class, ratio=1, std.caliper = T) #IT WORKS WITH 1

summary(m.out1_data_caliper2019, un = T)
m.data1_data_caliper2019 <- match.data(data= DataWide2019, m.out1_data_caliper2019)
rm(DataWide2019, DataWideControl)

#2.4.2.3.10. Put everything together and save
m.data1_data_caliperMatch <- rbind(m.data1_data_caliper2014, m.data1_data_caliper2016,m.data1_data_caliper2017, m.data1_data_caliper2018, m.data1_data_caliper2019) #removed: m.data1_data_caliper2019
setwd("C:/Users/mathe/OneDrive/Documentos/R/Database")
write.csv2(m.data1_data_caliperMatch, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_BYNace.csv", row.names = F)
table(m.data1_data_caliperMatch$treat == 1) #8 matched now (if I change the caliper back to 0.07 and ratio to 5, I miss 2 in 2018 and 2 in 2019)
table(m.data1_data_caliperMatch$Group) #1 treatment for 2014, 2016, and 2017 (each matched to 5 controls), and 3 and 2 treated matched to 1 control each in 2018 and 2019, respectively
table(DataWide$YearFirstAdoption) #
m.out0_data <- matchit(treat ~ Agegroup + Size_class + NoPatentsYearGUOtotal.2010 + NoPatentsYearGUOtotal.2011 + 
                         NoPatentsYearGUOtotal.2012 + NoPatentsYearGUOtotal.2013 + NoPatentsYearGUOtotal.2014 + NoPatentsYearGUOtotal.2015 +
                         NoPatentsYearGUOtotal.2016 + NoPatentsYearGUOtotal.2017 + NoPatentsYearGUOtotal.2018 + NoPatentsYearGUOtotal.2019 +
                         Quartile, data = m.data1_data_caliperMatch, method = NULL, distance = "cbps") 
# Check balance of this match
summary(m.out0_data)
####4.4.1.Transform data from wide back into long format----
#####4.4.1.1. For distance-based-----
rm(list=ls())
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_ratio5.csv", sep = ";", header = TRUE, dec=",")
#drop mean column, so that we don't have to change anything in the command after:
DataWide <- subset(DataWide, select = -c(Mean_patents) )

DataLong <- melt(setDT(DataWide), 
                 measure.vars=list(c(8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84), 
                                   c(9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85),
                                   c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86), 
                                   c(11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79, 83, 87)),
                 variable.name='LongIdentifier', value.name=c('NoPatentsYearGUOtotal', 'No_AI_PatentsYearGUOtotal',
                                                              'Relatedness_GUOs','RelatednessGUosandSubs'))[,LongIdentifier:= paste0('f',LongIdentifier)][order(Company)]


DataLong$CurrentYear<-rep(c(2000:2019),times=nrow(DataLong)/20)
table(DataLong$CurrentYear)

length(unique(DataWide$Company)) #48 (8 treated matched to 48 control)
length(unique(DataLong$Company)) #48

#and save it
write.csv2(DataLong, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_ratio5.csv", row.names = F)

#####4.4.1.2. Prepare data for distance-based----
rm(list=ls())
DataLong_Subs <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_ratio5.csv", sep = ";", header = TRUE, dec=",")
DataLong_Subs$first.treat <- as.numeric(ifelse(DataLong_Subs$YearFirstAdoption=="Control",0,DataLong_Subs$YearFirstAdoption))

#fix current year: pick the last digit after fff and add 1999 to it:
DataLong_Subs$CurrentYear <- as.numeric(str_extract(DataLong_Subs$LongIdentifier, "\\-*\\d+\\.*\\d*")) + 1999

table(DataLong_Subs$CurrentYear)

#produce some short descriptives about nace codes:
DataLong_Subs_desc <- DataLong_Subs[DataLong_Subs$CurrentYear==2019,]
AIcompanies <- DataLong_Subs_desc[DataLong_Subs_desc$treat == 1,]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
rm(FreqNacecodes, DataLong_Subs_desc, AIcompanies)

#exclude data before 2006:
DataLong_Subs <- DataLong_Subs[DataLong_Subs$CurrentYear>2005,]
table(DataLong_Subs$CurrentYear)
table(DataLong_Subs$LongIdentifier)

DataLong_Subs %<>%
  group_by(Company, CurrentYear) %>%
  mutate(SeqOwner = seq_along(Company))

#create unique id for every Company;
DataLong_Subs$id <- paste0(DataLong_Subs$Company,DataLong_Subs$Group,DataLong_Subs$SeqOwner)
length(unique(DataLong_Subs$id)) #48 

DataLong_Subs$id <- as.numeric(factor(DataLong_Subs$id, 
                                      levels=unique(DataLong_Subs$id)))
length(unique(DataLong_Subs$id)) #2606
table(DataLong_Subs$id)

DataLong_Subs_sim <- DataLong_Subs

#read new indicators of relatedness:
DataRel_2006 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2006_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2007 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2007_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2008 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2008_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2009 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2009_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2010 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2010_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2011 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2011_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2012 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2012_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2013 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2013_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2014 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2014_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2015 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2015_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2016 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2016_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2017 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2017_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2018 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2018_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2019 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2019_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2006$CurrentYear <- 2006
DataRel_2007$CurrentYear <- 2007
DataRel_2008$CurrentYear <- 2008
DataRel_2009$CurrentYear <- 2009

DataRel_2010$CurrentYear <- 2010
DataRel_2011$CurrentYear <- 2011
DataRel_2012$CurrentYear <- 2012
DataRel_2013$CurrentYear <- 2013
DataRel_2014$CurrentYear <- 2014
DataRel_2015$CurrentYear <- 2015
DataRel_2016$CurrentYear <- 2016
DataRel_2017$CurrentYear <- 2017
DataRel_2018$CurrentYear <- 2018
DataRel_2019$CurrentYear <- 2019

DataRel <- rbind(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009,
                 DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, DataRel_2015, DataRel_2016, DataRel_2017,
                 DataRel_2018, DataRel_2019)
rm(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009, DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, 
   DataRel_2015, DataRel_2016, DataRel_2017, DataRel_2018, DataRel_2019)

DataRel %<>% 
  rename(Company = X)

#replace NAs by 0 (they refer to NaN values that were generated for firms that had no specialization)
table(is.na(DataRel$Relatedness_Association))
DataRel[is.na(DataRel)] <- 0

#merge data to the current dataset
DataLong_Subs_sim <- left_join(DataLong_Subs_sim,DataRel, by=c("Company","CurrentYear"),na_matches="never")

#fill missing data with the relatedness from the previous year;
DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Association)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jaccard)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cosine)) %>% 
  ungroup

DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Assoc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jacc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cos2, fromLast = TRUE)) %>% 
  ungroup
table(is.na(DataLong_Subs_sim$Relatedness_Assoc2)) #all False, as expected;

test<-DataLong_Subs_sim[DataLong_Subs_sim$Group == 2018,]
test2<-test[test$treat == 0,]
test3<-test[test$treat == 1,]
mean(test2$Relatedness_Assoc2) #nontreated; 7.057143
mean(test3$Relatedness_Assoc2) #treated; 3.238095
rm(test,test2,test3)

#calculate the mean relatedness of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Relatedness_Cos2), by = treat ] #or Relatedness_Assoc2 or Relatedness_Jacc2
#8.573214 for 0 and 7.133929 for 1
rm(DataLong_Subs_sim3)

TreatedUnits <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
length(unique(TreatedUnits$id)) #8 
rm(TreatedUnits)

#the code below could overwrite the original one, where I excluded Companies ids, so I'll save it twice
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(DataLong_Subs_sim, file = "Output_code/Data/Code_Matching/Matched_companies_Acquis_match_ratio5.csv", row.names = F)

#####4.4.2.1. For NACE-based-----
rm(list=ls())
DataWide <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_BYNace.csv", sep = ";", header = TRUE, dec=",")
#drop mean column, so that we don't have to change anything in the command after:
DataWide <- subset(DataWide, select = -c(Mean_patents) )

DataLong <- melt(setDT(DataWide), 
                 measure.vars=list(c(8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84), 
                                   c(9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85),
                                   c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86), 
                                   c(11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79, 83, 87)),
                 variable.name='LongIdentifier', value.name=c('NoPatentsYearGUOtotal', 'No_AI_PatentsYearGUOtotal',
                                                              'Relatedness_GUOs','RelatednessGUosandSubs'))[,LongIdentifier:= paste0('f',LongIdentifier)][order(Company)]


DataLong$CurrentYear<-rep(c(2000:2019),times=nrow(DataLong)/20)
table(DataLong$CurrentYear)

length(unique(DataWide$Company)) #24 
length(unique(DataLong$Company)) #24

#and save it
write.csv2(DataLong, file = "Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_Nace_2d.csv", row.names = F)

#####4.4.2.1. Prepare data for NACE-based----
rm(list=ls())
DataLong_Subs <- read.csv("Matheus/DataGenerated/Data_matching_stock2000_2010/DataLong_ThirdOption_4thY_RelGuosSubs_NEW_NACE_TESTS_Acquis_match_Nace_2d.csv", sep = ";", header = TRUE, dec=",")
DataLong_Subs$first.treat <- as.numeric(ifelse(DataLong_Subs$YearFirstAdoption=="Control",0,DataLong_Subs$YearFirstAdoption))

#fix current year: pick the last digit after fff and add 1999 to it:
DataLong_Subs$CurrentYear <- as.numeric(str_extract(DataLong_Subs$LongIdentifier, "\\-*\\d+\\.*\\d*")) + 1999

table(DataLong_Subs$CurrentYear)

#produce some short descriptives about nace codes:
DataLong_Subs_desc <- DataLong_Subs[DataLong_Subs$CurrentYear==2019,]
AIcompanies <- DataLong_Subs_desc[DataLong_Subs_desc$treat == 1,]
FreqNacecodes <- as.data.frame(table(AIcompanies$Nace_4d))
rm(FreqNacecodes, DataLong_Subs_desc, AIcompanies)

#exclude data before 2006:
DataLong_Subs <- DataLong_Subs[DataLong_Subs$CurrentYear>2005,]
table(DataLong_Subs$CurrentYear)
table(DataLong_Subs$LongIdentifier)

DataLong_Subs %<>%
  group_by(Company, CurrentYear) %>%
  mutate(SeqOwner = seq_along(Company))

#create unique id for every Company;
DataLong_Subs$id <- paste0(DataLong_Subs$Company,DataLong_Subs$Group,DataLong_Subs$SeqOwner)
length(unique(DataLong_Subs$id)) #48 

DataLong_Subs$id <- as.numeric(factor(DataLong_Subs$id, 
                                      levels=unique(DataLong_Subs$id)))
length(unique(DataLong_Subs$id)) #2606
table(DataLong_Subs$id)

DataLong_Subs_sim <- DataLong_Subs

#read new indicators of relatedness:
DataRel_2006 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2006_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2007 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2007_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2008 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2008_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2009 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2009_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2010 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2010_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2011 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2011_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2012 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2012_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2013 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2013_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2014 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2014_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2015 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2015_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2016 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2016_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2017 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2017_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2018 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2018_MNE.csv", sep = ";", header = TRUE, dec=",")
DataRel_2019 <- read.csv("Matheus/DataGenerated/Relatedness_corrected/Relatedness_2019_MNE.csv", sep = ";", header = TRUE, dec=",")

DataRel_2006$CurrentYear <- 2006
DataRel_2007$CurrentYear <- 2007
DataRel_2008$CurrentYear <- 2008
DataRel_2009$CurrentYear <- 2009

DataRel_2010$CurrentYear <- 2010
DataRel_2011$CurrentYear <- 2011
DataRel_2012$CurrentYear <- 2012
DataRel_2013$CurrentYear <- 2013
DataRel_2014$CurrentYear <- 2014
DataRel_2015$CurrentYear <- 2015
DataRel_2016$CurrentYear <- 2016
DataRel_2017$CurrentYear <- 2017
DataRel_2018$CurrentYear <- 2018
DataRel_2019$CurrentYear <- 2019

DataRel <- rbind(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009,
                 DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, DataRel_2015, DataRel_2016, DataRel_2017,
                 DataRel_2018, DataRel_2019)
rm(DataRel_2006,DataRel_2007,DataRel_2008,DataRel_2009, DataRel_2010, DataRel_2011, DataRel_2012, DataRel_2013, DataRel_2014, 
   DataRel_2015, DataRel_2016, DataRel_2017, DataRel_2018, DataRel_2019)

DataRel %<>% 
  rename(Company = X)

#replace NAs by 0 (they refer to NaN values that were generated for firms that had no specialization)
table(is.na(DataRel$Relatedness_Association))
DataRel[is.na(DataRel)] <- 0

#merge data to the current dataset
DataLong_Subs_sim <- left_join(DataLong_Subs_sim,DataRel, by=c("Company","CurrentYear"),na_matches="never")

#fill missing data with the relatedness from the previous year;
DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Association)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jaccard)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cosine)) %>% 
  ungroup

DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Relatedness_Assoc2 = na.locf0(Relatedness_Assoc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Jacc2 = na.locf0(Relatedness_Jacc2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Prob2 = na.locf0(Relatedness_Prob2, fromLast = TRUE)) %>% 
  mutate(Relatedness_Cos2 = na.locf0(Relatedness_Cos2, fromLast = TRUE)) %>% 
  ungroup
table(is.na(DataLong_Subs_sim$Relatedness_Assoc2)) #all False, as expected;

test<-DataLong_Subs_sim[DataLong_Subs_sim$Group == 2018,]
test2<-test[test$treat == 0,]
test3<-test[test$treat == 1,]
mean(test2$Relatedness_Assoc2) #nontreated; 7.057143
mean(test3$Relatedness_Assoc2) #treated; 3.238095
rm(test,test2,test3)

#calculate the mean relatedness of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Relatedness_Cos2), by = treat ] #or Relatedness_Assoc2 or Relatedness_Jacc2
#8.573214 for 0 and 7.133929 for 1
rm(DataLong_Subs_sim3)

TreatedUnits <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
length(unique(TreatedUnits$id)) #8 
rm(TreatedUnits)

#the code below could overwrite the original one, where I excluded Companies ids, so I'll save it twice
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(DataLong_Subs_sim, file = "Output_code/Data/Code_Matching/Matched_companies_Acquis_match_Nace_2d.csv", row.names = F)

#