library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)
library(readxl) #for reading the xlsx files
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
library(EconGeo) # Economic Geography functions
library(psych) #for descriptives
library(Metrics) #for mae
library(did) #for treatment effect over distinct periods of time
library(openxlsx)
library(zoo) #for some reason the function na.locf is just in the zoo package for me now
library(vtable) #for functions st and sumtable in the statistics
library(ggcorrplot) #for correlation table
library(janitor) #for improving the names of the columns (which in turn is done using data %<>%  clean_names())

#1.Estimating technological distance of sectors to AI-----
##1.1. Calculate AI matrix ------
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all_patents <-fread("Input_code/Big_files_ignore/All_patents.csv")

#Now we add the nace codes:
CompaniesData1<-read_excel("Input_code/Big_files_ignore/DataCompanies1.xlsx", sheet = "Results", na = "n.a.")
CompaniesData2<-read_excel("Input_code/Big_files_ignore/DataCompanies2.xlsx", sheet = "Results", na = "n.a.")
CompaniesData3<-read_excel("Input_code/Big_files_ignore/DataCompanies3.xlsx", sheet = "Results", na = "n.a.")
CompaniesData4<-read_excel("Input_code/Big_files_ignore/DataCompanies4.xlsx", sheet = "Results", na = "n.a.")
CompaniesData5<-read_excel("Input_code/Big_files_ignore/DataCompanies5.xlsx", sheet = "Results", na = "n.a.")
CompaniesData6<-read_excel("Input_code/Big_files_ignore/DataCompanies6.xlsx", sheet = "Results", na = "n.a.")
CompaniesData <- rbind(CompaniesData1,CompaniesData2,CompaniesData3,CompaniesData4,CompaniesData5,CompaniesData6)
rm(CompaniesData1,CompaniesData2,CompaniesData3,CompaniesData4,CompaniesData5,CompaniesData6)

CompaniesData <- CompaniesData[,c(2,3,6)] 
names(CompaniesData) <- c("Company_name", "Company", "Nace_4d")

all_patents <- left_join(all_patents, CompaniesData, by = "Company")
table(is.na(all_patents$Nace_4d)) #12,188,742   F (i.e., companies with NACE), and 570,280   T

#drop companies without Nace codes:
all_patents <- all_patents[is.na(all_patents$Nace_4d) == F,]
rm(CompaniesData)
#exclude company name:
all_patents<-all_patents[,c(-7)]

#create the 3 functions we'll use later:
#1.create a function for fractional counting IPC codes on which patent
group_by_applnID <- function (data){
  data %>%
    group_by(PubNo) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}
#2.create a function for counting the relative participation of each IPC code for each NACE sector
group_by_NACE_and_Subclass <- function (data){
  data %<>%
    group_by(Nace_4d, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}  
#3. create a function to build a square matrix that aggregates the co-occurrence one variable in regards to other (e.g., how many times two IPC codes appear in the same patent)
create_sparse_matrix <- function(i.input, j.input){
  require(Matrix)
  mat <- spMatrix(nrow = i.input %>% n_distinct(),
                  ncol = j.input %>% n_distinct(),
                  i = i.input %>% factor() %>% as.numeric(),
                  j = j.input %>% factor() %>% as.numeric(),
                  x = rep(1, i.input %>% length() ) )
  
  row.names(mat) <- i.input %>% factor() %>% levels()
  colnames(mat) <- j.input %>% factor() %>% levels()
  return(mat)
}

#introduce AI as a specific subclass
AI_Patents <- all_patents[all_patents$AIpatent == "Yes",]
length(unique(AI_Patents$PubNo)) #76,578
length(unique(all_patents$PubNo))#7,354,214

#pick distinct codes at two variables:
AI_Patents_with_code <- distinct_at(AI_Patents, vars(PubNo,Company), .keep_all = T)
length(unique(AI_Patents_with_code$PubNo))  #76,578
AI_Patents_with_code$Subclass <- "Artificial_Intelligence" #

AI_Patents <- rbind(AI_Patents, AI_Patents_with_code)
length(unique(AI_Patents$PubNo)) #76,578

all_patents <- rbind(all_patents, AI_Patents)
table(all_patents$AIpatent) #334,316 "Yes" 
rm(AI_Patents_with_code, AI_Patents)

#now let's create a square matrix where we see how often two IPC codes appear in the same patent
mat_tech <- create_sparse_matrix(i = all_patents %>% pull(PubNo),
                                 j = all_patents %>% pull(Subclass))

#let's convert this to a matrix that we can properly visualize:
mat_tech %<>% 
  crossprod() %>% 
  as.matrix() 

#finally, let's calculate the relatedness of every technology to AI
mat_tech %<>% 
  relatedness(method = "cosine")

#save this matrix to be used later
write.csv2(mat_tech, file = "Output_code/Data/Matrix_relatedness_technologies_to_AI.csv", row.names = T) #
rm(mat_tech)

##1.2. Measure how related Nace sectors are to AI technologies-----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all_patents <-fread("Input_code/Big_files_ignore/All_patents.csv")

#Now we add the nace codes:
CompaniesData1<-read_excel("Input_code/Big_files_ignore/DataCompanies1.xlsx", sheet = "Results", na = "n.a.")
CompaniesData2<-read_excel("Input_code/Big_files_ignore/DataCompanies2.xlsx", sheet = "Results", na = "n.a.")
CompaniesData3<-read_excel("Input_code/Big_files_ignore/DataCompanies3.xlsx", sheet = "Results", na = "n.a.")
CompaniesData4<-read_excel("Input_code/Big_files_ignore/DataCompanies4.xlsx", sheet = "Results", na = "n.a.")
CompaniesData5<-read_excel("Input_code/Big_files_ignore/DataCompanies5.xlsx", sheet = "Results", na = "n.a.")
CompaniesData6<-read_excel("Input_code/Big_files_ignore/DataCompanies6.xlsx", sheet = "Results", na = "n.a.")
CompaniesData <- rbind(CompaniesData1,CompaniesData2,CompaniesData3,CompaniesData4,CompaniesData5,CompaniesData6)
rm(CompaniesData1,CompaniesData2,CompaniesData3,CompaniesData4,CompaniesData5,CompaniesData6)

CompaniesData <- CompaniesData[,c(2,3,6)] 
names(CompaniesData) <- c("Company_name", "Company", "Nace_4d")

all_patents <- left_join(all_patents, CompaniesData, by = "Company")
table(is.na(all_patents$Nace_4d)) #12,188,742   F (i.e., companies with NACE), and 570,280   T

#drop companies without Nace codes:
all_patents <- all_patents[is.na(all_patents$Nace_4d) == F,]
rm(CompaniesData)
#exclude company name:
all_patents<-all_patents[,c(-7)]

#create the 3 functions we'll use later:
#1.create a function for fractional counting IPC codes on which patent
group_by_applnID <- function (data){
  data %>%
    group_by(PubNo) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}
#2.create a function for counting the relative participation of each IPC code for each NACE sector
group_by_NACE_and_Subclass <- function (data){
  data %<>%
    group_by(Nace_4d, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}  

length(unique(all_patents$PubNo)) #7,354,214 

#count total of patents of each sector, and total of AI patents;
all_patents %<>% group_by(Nace_4d) %>% 
  mutate(TotalPatents = length(unique(PubNo))) %>% #get the higher share of usage
  mutate(TotalAIPatents = length(na.omit(unique(PubNo[AIpatent == "Yes"])))) %>% 
  mutate(ShareAI = TotalAIPatents/TotalPatents) %>% 
  ungroup()

#Finally, let's apply the two functions we created before:
reg_tech <- group_by_applnID(all_patents) #this gets the relative participation of each IPC code on each patent
reg_tech <- group_by_NACE_and_Subclass(reg_tech) #this gets the sum of the relative participation of each code for each NACE
rm(all_patents)

#let's convert this into a matrix where we have one line per NACE code, and all technologies linked to them in the columns
mat_reg_tech <- reg_tech %>%
  arrange(Subclass) %>% 
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0)) #

#let's throw the nace codes into the column names
mat_reg_tech %<>% remove_rownames %>% column_to_rownames(var="Nace_4d") %>%
  as.matrix() %>%
  round() #

#and calculate binary specializations
rel_sectors_codes <- mat_reg_tech %>% location_quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("mat_reg_tech") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -mat_reg_tech) %>%
  arrange(mat_reg_tech, Subclass)
rm(mat_reg_tech)

#let's see now which ones are the main AI codes
mat_tech_rel <-read.csv2("Output_code/Data/Matrix_relatedness_technologies_to_AI.csv")#file we created before measuring the relatedness of AI to other technologies
mat_tech_rel <- mat_tech_rel[,c("X", "Artificial_Intelligence")] #select the AI and the other technologies columns
#let's pick the 9 codes with higher relatedness to AI:
AI_codes_top9 <- mat_tech_rel[mat_tech_rel$Artificial_Intelligence >.038,1] #9 codes

#these codes stand for:
#G06F	Electric Digital Data Processing — This subclass includes general-purpose digital computers, data processing systems, computer architecture, memory access, input/output control, etc. Widely regarded as a core category for computing hardware and software.
#G06K	Recognition of Data; Presentation of Data; Record Carriers — Encompasses technologies for scanning, optical character recognition (OCR), barcode processing, and data entry systems. Important in document automation and machine reading.
#G06N	Computer Systems Based on Specific Computational Models — The core IPC subclass for artificial intelligence, including neural networks, genetic algorithms, and fuzzy logic systems.
#G06Q	Data Processing Systems or Methods, Specially Adapted for Administrative, Commercial, Financial, Managerial, Supervisory or Forecasting Purposes — Covers business-oriented computing, such as e-commerce, logistics optimization, financial modeling, and enterprise resource planning.
#G06T	Image Data Processing or Generation — Includes image analysis, computer vision, image enhancement, video encoding/decoding, and rendering. A key area for visual AI and machine learning.
#G10L	Speech Analysis or Synthesis; Speech Recognition; Speech or Voice Processing — Dedicated to natural language and audio processing, including voice assistants, speech-to-text systems, and acoustic modeling.
#H04L	Transmission of Digital Information — Encompasses telecommunication systems, including network protocols, error detection/correction, secure communication, and internet data transfer. Relevant to AI in networked environments (e.g., edge computing).
#G05B	Control or Regulating Systems in General — Encompasses automatic control systems, process automation, and feedback control. Highly relevant for robotics, industrial automation, and AI-integrated control.
#G16H	Health Informatics — Dedicated subclass for ICT in healthcare, including electronic health records, decision support systems, telemedicine, and AI-based health analytics. Introduced in the G16 class for computer-based methods in specialized fields.

#the next ones stand for:
#A61B	Diagnosis; Surgery; Identification — Covers diagnostic devices and procedures (e.g., imaging, endoscopy), surgical tools, and biosignal monitoring (e.g., ECG, EEG). Heavily used in medical technology and AI-based diagnostics.
#G01V	Geophysics; Gravitational, Magnetic or Electric Prospecting; Detecting Masses or Objects — Used in remote sensing, subsurface imaging (e.g., seismic), and radar systems. AI is increasingly applied to automate such analyses.
#H04W	Wireless Communication Networks — Deals with mobile communication systems, 5G, network resource allocation, and mobility management. AI techniques are widely applied in network optimization and edge computing.
#G01N	Investigating or Analyzing Materials by Determining Their Chemical or Physical Properties — Includes chemical testing, biosensing, materials analysis; foundational in biotech, diagnostics, and laboratory automation.
#G01R	Measuring Electric Variables; Testing Electric Components — Concerns current, voltage, impedance, and circuit diagnostics. Relevant for smart sensors and electronics.
#H04N	Pictorial Communication (e.g., Television; Video Cameras; Image Transmission) — Covers technologies related to video encoding, streaming, image transmission, and multimedia interfaces. Often used in AI-driven image and video analytics.

length(unique(rel_sectors_codes$Subclass)) #645 subclasses
length(unique(rel_sectors_codes$mat_reg_tech)) #620 unique sectors

# Function to count specializations for a given list of codes
count_specializations <- function(data, code_list, column_name) {
  data %>% mutate(
    !!column_name := case_when(Subclass %in% code_list & RCA == 1 ~ 1,
                               TRUE ~ 0)) %>%
    group_by(mat_reg_tech) %>%
    summarise(!!column_name := sum(!!as.symbol(column_name), na.rm = TRUE), .groups = "drop")
}

# Create the new columns
rel_sectors_codes <- rel_sectors_codes %>%
  left_join(count_specializations(rel_sectors_codes, AI_codes_top9, "AI_specializations_top9"), by = "mat_reg_tech")

#rename the mat_reg_tech column
names(rel_sectors_codes)[names(rel_sectors_codes) == 'mat_reg_tech'] <- 'Nace_4d'

#now, I'll count the total number of specializations of each nace
rel_sectors_codes  %<>%  group_by(Nace_4d) %>% 
  mutate(number_spec = length(unique(Subclass[RCA == 1]))) %>% 
  mutate(Share_top9 = AI_specializations_top9/number_spec) %>% 
  ungroup()

#separate, keeping just one line per sector
Unique_sectors <- distinct_at(rel_sectors_codes, vars(Nace_4d), .keep_all = T)
Unique_sectors <- subset(Unique_sectors, select = -c(Subclass) )
table(is.nan(Unique_sectors$Share_top9))
#replace nan values by 0:
Unique_sectors <- Unique_sectors %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))
table(is.nan(Unique_sectors$Share_top9))

#create quartiles information
# Function to create quartile variable
create_quartile_variable <- function(data, share_column, quartile_column_name) {
  
  share_column_sym <- sym(share_column)  # Convert share_column to a symbol
  
  data %>%
    mutate(
      !!quartile_column_name := case_when(
        between(!!share_column_sym, 
                quantile(!!share_column_sym, 0, na.rm = TRUE), 
                quantile(!!share_column_sym, 0.25, na.rm = TRUE)) ~ "Bottom",
        between(!!share_column_sym, 
                quantile(!!share_column_sym, 0.25, na.rm = TRUE), 
                quantile(!!share_column_sym, 0.75, na.rm = TRUE)) ~ "IQR",
        between(!!share_column_sym, 
                quantile(!!share_column_sym, 0.75, na.rm = TRUE), 
                quantile(!!share_column_sym, 1, na.rm = TRUE)) ~ "Top",
        TRUE ~ NA_character_ # Handle missing values
      )
    )
}

# Create the new quartile columns
Unique_sectors <- Unique_sectors %>%
  create_quartile_variable("Share_top9", "Quartile_top9")

#check value counts in each category:
table(Unique_sectors$Quartile_top9)
#Bottom    IQR    Top 
#342       123    155 #due to missing specializations in any AI-related technologies, there are too many sectors at the bottom;
#therefore, it's rather 3 groups than a "quartile" per se
write.csv(Unique_sectors, file = "Output_code/Data/Distance_measure.csv", row.names = F) 

#2.Measuring effects-----
##2.1.Overall-----
###2.1.1.Non-standard - Relatedness and number of patents regardless of sector  ----
rm(list=ls())
DataLong_Subs_sim <- read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",") #
length(unique(DataLong_Subs_sim$Company))
#1.1.1.Simple Aggregation Relatedness
example_attgt <- att_gt(yname = "Relatedness_Cos2", tname = "CurrentYear", idname = "id",
                        gname = "first.treat", xformla = ~1, data = DataLong_Subs_sim, alp=.01
)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 1.0798               Std. Error: 0.1998         [ 99%  Conf. Int.]: 0.5651      1.5945 *
#This effect is shown in Panel A - Relatedness

#1.1.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 1.3202        0.2976     0.5535      2.0868 *
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g1_Relatedness_all.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G1) All quartiles - Relatedness") + theme(legend.position="right") +   ylab("Knowledge-relatedness") + 
  xlab("Length of exposure") + scale_color_manual(values=c("grey70","orange"),labels = c("Pre-treatment", "Post-treatment")) + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1, alpha = 0.3)
dev.off()

#Simple Aggregation Innovative performance
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", tname = "CurrentYear", idname = "id", 
                        gname = "first.treat", xformla = ~1, data = DataLong_Subs_sim, alp=.01
)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 18.14        Std. Error: 3.6236     [ 99%  Conf. Int.]: 8.8062     27.4737 ****
#This effect is shown in Panel A - Innovative Performance

#1.1.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 27.982        8.3811     6.3936     49.5703 *
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g2_N_patents_all.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G2) All quartiles - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Number of patents") + 
  xlab("Length of exposure") + 
  # ylim(-1, 4) +
  scale_color_manual(values=c("grey70","saddlebrown"),labels = c("Pre-treatment", "Post-treatment")) + 
  #labs(subtitle = "Effect on the number of patents")+
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

####2.1.1.1.Moderating effects (non-stand)
# Create Create the Post-Treatment binary Variable
DataLong_Subs_sim$post_treatment <- ifelse(DataLong_Subs_sim$CurrentYear >= DataLong_Subs_sim$first.treat & 
                                               DataLong_Subs_sim$Group <= DataLong_Subs_sim$CurrentYear, 1, 0)

#Basic DiD model without the moderator for relatedness
model <- lm(Relatedness_Cos2 ~ treat * post_treatment, data = DataLong_Subs_sim)
summary(model) ##treat:post_treatment  1.3380***, post_treatment 1.2469***, treat 0.6618***, 
#This effect is shown in Panel B - Relatedness

# Basic DiD model WITHOUT the moderator for innovative performance
model <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment, data = DataLong_Subs_sim)
summary(model) #treat:post_treatment   29.473***, post_treatment 33.084*** , treat 4.478. 
#This effect is shown in Panel B - Innovative Performance

# DiD model with Relatedness_Cos2 as a moderator 
model_moderation <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment * Relatedness_Cos2, data = DataLong_Subs_sim)
summary(model_moderation) #treat:post_treatment -21.9771**, treat:post_treatment:Relatedness_Cos2 1.9468***,
#treat:Relatedness_Cos2 1.9534*** 
#This effect is shown in Panel C

###2.1.2.Standardized - Relatedness and number of patents regardless of sector ------
DataLong_Subs_sim_standard <- DataLong_Subs_sim
DataLong_Subs_sim_standard[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_standard[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

#Simple Aggregation Relatedness
example_attgt <- att_gt(yname = "Relatedness_Cos2", tname = "CurrentYear", idname = "id", gname = "first.treat",
                        xformla = ~1, data = DataLong_Subs_sim_standard, alp=.01)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 0.0965  Std. Error: 0.0185   [ 99%  Conf. Int.]: 0.0489      0.1441 *
#This effect is shown in Panel A - Standardized - Relatedness

#Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 0.118        0.0275     0.0471      0.1889 *
ggdid(agg.es) 

#Simple Aggregation Innovative Performance
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", tname = "CurrentYear", idname = "id", gname = "first.treat",
                        xformla = ~1,data = DataLong_Subs_sim_standard, alp=.01)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 0.0874        Std. Error: 0.0159     [ 99%  Conf. Int.]: 0.0466      0.1282 ***
#This effect is shown in Panel A - Standardized - Innovative performance

#Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 0.1348        0.0383     0.0363      0.2334 *
ggdid(agg.es) 

####2.1.2.1.Moderating effects (stand)
# Create Create the Post-Treatment binary Variable
DataLong_Subs_sim_standard$post_treatment <- ifelse(DataLong_Subs_sim_standard$CurrentYear >= DataLong_Subs_sim_standard$first.treat & 
                                             DataLong_Subs_sim_standard$Group <= DataLong_Subs_sim_standard$CurrentYear, 1, 0)

#Basic DiD model without the moderator for relatedness
model <- lm(Relatedness_Cos2 ~ treat * post_treatment, data = DataLong_Subs_sim_standard)
summary(model) ##treat:post_treatment  0.11958***, post_treatment 0.11143***, treat 0.05914*** 
#This effect is shown in Panel B - Standardized - Relatedness

# Basic DiD model WITHOUT the moderator for innovative performance
model <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment, data = DataLong_Subs_sim_standard)
summary(model) #treat:post_treatment   0.142023***, post_treatment 0.159419*** , treat 0.021576. 
#This effect is shown in Panel B - Standardized - Innovative performance

# DiD model with Relatedness_Cos2 as a moderator 
model_moderation <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment * Relatedness_Cos2, data = DataLong_Subs_sim_standard)
summary(model_moderation) #treat:post_treatment 0.01644, treat:post_treatment:Relatedness_Cos2 0.10497***,
#treat:Relatedness_Cos2 0.10533*** 
#This effect is shown in Panel C - Standardized - All

##2.2. Measuring effects across sectors -----
rm(list=ls())

DataLong_Subs_sim <-read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",")
length(unique(DataLong_Subs_sim$id)) #2514 ids 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Extra_data <-read.csv("Input_code/Matched_companies_extra_data.csv")
length(unique(Extra_data$id)) #2514 ids 
DataLong_Subs_sim <- left_join(DataLong_Subs_sim, Extra_data, by = c("id", "CurrentYear"))
rm(Extra_data)

#calculate extra indicators
#subs:
#Rate granted subs/total
DataLong_Subs_sim$Rate_granted_subs_rel_HQ <- DataLong_Subs_sim$n_granted_yes_subsidiaries/(DataLong_Subs_sim$n_granted_yes_HQ+DataLong_Subs_sim$n_granted_yes_subsidiaries) 
#Rate success subs
DataLong_Subs_sim$Rate_success_subs <- DataLong_Subs_sim$n_granted_yes_subsidiaries/(DataLong_Subs_sim$n_granted_na_subsidiaries+DataLong_Subs_sim$n_granted_yes_subsidiaries) 
#Rate granted per sub (i.e., patent productivity per sub)
DataLong_Subs_sim$Rate_granted_per_sub <- DataLong_Subs_sim$n_granted_yes_subsidiaries/(DataLong_Subs_sim$No_subs_extended) 
#Rate granted subs/total considering the old numbers of patents calculated
DataLong_Subs_sim$Rate_granted_subs_rel_HQ_TOTAL <- DataLong_Subs_sim$NoPatentsYearALLSubs/(DataLong_Subs_sim$NoPatentsYearALLSubs+DataLong_Subs_sim$NoPatentsYearGUOtotal) 

#HQ:
#Rate success hq
DataLong_Subs_sim$Rate_success_HQ <- DataLong_Subs_sim$n_granted_yes_HQ/(DataLong_Subs_sim$n_granted_na_HQ+DataLong_Subs_sim$n_granted_yes_HQ) 

#Patent Productivity (Patents per R&D Investment)
DataLong_Subs_sim$Patent_productivity <- DataLong_Subs_sim$No_new_PatentsYearGUOalone/DataLong_Subs_sim$RandD_Year
#Patent Output per Employee
DataLong_Subs_sim$Patent_output_per_employee <- DataLong_Subs_sim$No_new_PatentsYearGUOalone/DataLong_Subs_sim$number_of_employees

#REPLACE NANs of these last 8 columns by NA (check first)
#shouldn't I always use NA instead of NAN in this kind of replacement?
DataLong_Subs_sim <- DataLong_Subs_sim %>%
  mutate(across(
    c("Rate_success_subs", 
      "Rate_success_HQ",
      "Patent_productivity", "Patent_output_per_employee"),
    ~ ifelse(is.nan(.), NA, .)
  ))

#replace infinite values from Patent_productivity ad Patent_output_per_employee for NA
DataLong_Subs_sim <- DataLong_Subs_sim %>%
  mutate(Patent_productivity = ifelse(is.infinite(Patent_productivity), NA, Patent_productivity)) %>% 
  mutate(Patent_output_per_employee = ifelse(is.infinite(Patent_output_per_employee), NA, Patent_output_per_employee)) %>% 
  mutate(Rate_granted_per_sub = ifelse(is.infinite(Rate_granted_per_sub), NA, Rate_granted_per_sub)) 
  
#Insert distance  measure
CategoriesNace <-read.csv("Output_code/Data/Distance_measure.csv", header = TRUE)#[,c(-1)] #doesn't work, no significance
CategoriesNace <- CategoriesNace[,c("Nace_4d", "Quartile_top9")]
names(CategoriesNace) <- c("Nace_4d", "Quartile")
CategoriesNace$Nace_4d <- as.character(CategoriesNace$Nace_4d)
DataLong_Subs_sim$Nace_4d <- as.character(DataLong_Subs_sim$Nace_4d)

DataLong_Subs_sim <- left_join(DataLong_Subs_sim,CategoriesNace, by = "Nace_4d")

table(is.na(DataLong_Subs_sim$Quartile)) #35196 F, no missing data
table(DataLong_Subs_sim$Quartile) #bottom 6048 medium  13496   top 15652       

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #bottom 3024       IQR   6748       top 7826  
table(nontreat_test$Quartile) #bottom 3024       IQR   6748       top 7826  
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #bottom 216  IQR 482        top 559     
rm(treat_test, nontreat_test)

DataLong_Subs_sim_group0 <- DataLong_Subs_sim
DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top" ,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" ,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Bottom",]

calculate_ci_and_significance <- function(att, se, group, variable_name) {
  # Confidence intervals at different significance levels
  ci_90_low <- att - 1.645 * se  # 90% confidence interval
  ci_90_high <- att + 1.645 * se
  
  ci_95_low <- att - 1.96 * se   # 95% confidence interval
  ci_95_high <- att + 1.96 * se
  
  ci_99_low <- att - 2.576 * se  # 99% confidence interval
  ci_99_high <- att + 2.576 * se
  
  # Determine the highest significance level
  significance <- ""
  
  if (ci_90_low > 0 | ci_90_high < 0) {
    significance <- "*"
  }
  if (ci_95_low > 0 | ci_95_high < 0) {
    significance <- "**"
  }
  if (ci_99_low > 0 | ci_99_high < 0) {
    significance <- "***"
  }
  if (!(ci_90_low > 0 | ci_90_high < 0)) {
    significance <- "" # No significance if not significant even at 90%
  }
  
  # Create a dataframe to store the results
  results_df <- data.frame(
    ATT = att,
    Std_Error = se,
    CI_90_Low = ci_90_low,
    CI_90_High = ci_90_high,
    CI_95_Low = ci_95_low,
    CI_95_High = ci_95_high,
    CI_99_Low = ci_99_low,
    CI_99_High = ci_99_high,
    Significance = significance,
    Group = group,
    Variable = variable_name
  )
  
  return(results_df)
}

#define the fixed variables
tname = "CurrentYear" 
idname = "id"
gname = "first.treat"

#define the possible groups
groups = c("DataLong_Subs_sim_group1","DataLong_Subs_sim_group2","DataLong_Subs_sim_group3", "DataLong_Subs_sim_group0")

#define the possible variables
variables = c(#main ones (8) - linked to knowledge creation, usage, and recombination
              "Relatedness_Cos2", #relatedness
              "NoPatentsYearGUOtotal", #innovative performance
              "Specializations_Number", #number of specializations
              "UniqueCodes", #for Sections
              "UniqueSubclass", #for 4-digit
              "Herfindahl", #the Herfindahl index
              "Shannon", #the Shannon index
              "AIcodes", #effect on the 5 codes most related to AI
              
              #quality measures (10) - knowledge quality
              "Rate_granted", #rate of granted patents
              "avg_claims", #most used technology
              "avg_number_of_family_members", #number of specializations
              "avg_backward_citations", #for Sections
              "avg_forward_citations", #for 4-digit
              "avg_time_filing_to_publication",
              "avg_time_filing_to_grant",
              "avg_time_publication_to_grant", #NEW, so now it's 19 of knowledge quality
              "n_granted_na", 
              "n_granted_yes",
              #subs and HQs (12) - knowledge flows
              "Rate_success_subs", 
              "Rate_success_HQ",
              "n_granted_yes_HQ", 
              "n_granted_na_HQ", 
              "avg_time_filing_to_publication_HQ", 
              "avg_time_filing_to_grant_HQ", 
              "avg_time_publication_to_grant_HQ", #new 1: for HQs
              "n_granted_yes_subsidiaries", 
              "n_granted_na_subsidiaries", 
              "avg_time_filing_to_publication_subsidiaries", 
              "avg_time_filing_to_grant_subsidiaries", 
              "avg_time_publication_to_grant_subsidiaries", 
              
              #aditional performance variables (3) - knowledge productivity
              "Patent_productivity", 
              "Patent_output_per_employee", 
              "RandD_Year")

###2.2.1.Calculate the effects Non-standardized across groups -----

#Group 1
variable_n = 1 #relatedness
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                        yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g3_Relatedness_1Q.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G3) Q1 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g4_Relatedness_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G4) IQR - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g5_Relatedness_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G5) Q4 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g6_Number_patents_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G6) Q1 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g7_Number_patents_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G7) IQR - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_g8_Number_patents_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "G8) Q4 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

###2.2.2.Calculate the effects Non-standardized for the whole dataset -----
#Group 4
variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 6
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 7
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 8
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "all_dataset"
results_df$mode <- "non_standard"

write.xlsx(results_df, file = "Output_code/Data/Table_2_absolute.xlsx", rowNames = F)

###2.2.3.Calculate the effects Standardized -----
DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")])

DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")])

DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")] <- 
  scale(DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal",  "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Herfindahl", "Shannon", "AIcodes",  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication", "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",      "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries",  "Patent_productivity", "Patent_output_per_employee", "RandD_Year")])

table(is.infinite(unlist(DataLong_Subs_sim_group0))) 
table(is.nan(unlist(DataLong_Subs_sim_group0))) 

#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#summary(agg.simple)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

###2.2.4.Calculate the effects Non-standardized for the whole dataset -----
#Group 4
variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 6
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 7
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 8
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "all_dataset"
results_df$mode <- "standard"

write.xlsx(results_df, file = "Output_code/Data/Table_2_stand.xlsx", rowNames = F)

##2.3. Contrasting granted versus non-granted-------
DataLong_Subs_sim %<>% group_by(Company) %>% mutate(total_granted_ai = max(n_granted_yes_ai, na.rm =T )) %>% ungroup()

####2.3.1. Treatment non-granted-----
Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
Current_non_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]

#so the treatment will now be companies that never had an AI patent granted
Current_treated_new_treat <-Current_treated[Current_treated$total_granted_ai ==0,]
length(unique(Current_treated_new_treat$id)) #777 non-granted

DataLong_Subs_sim_non_granted <- rbind(Current_treated_new_treat, Current_non_treated)
table(DataLong_Subs_sim_non_granted$Quartile)

DataLong_Subs_sim_group0 <- DataLong_Subs_sim_non_granted
DataLong_Subs_sim_group1 <- DataLong_Subs_sim_non_granted[DataLong_Subs_sim_non_granted$Quartile == "Top",]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_non_granted[DataLong_Subs_sim_non_granted$Quartile == "IQR" ,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_non_granted[DataLong_Subs_sim_non_granted$Quartile == "Bottom",]

#####2.3.1.1.Calculate the effects Non-standardized across groups -----
#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#summary(agg.simple)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#####2.3.1.2.Calculate the effects Non-standardized for the whole dataset -----
#Group 4
n_group = 4
variable_n = 9
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 10
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 11
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 12
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 13
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 14
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 15
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 16
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 17
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 18
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 19
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 20
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 21
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 22
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 23
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 24
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 25
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 26
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 27
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 28
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 29
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 30
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 31
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 32
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 33
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "non_granted"
results_df$mode <- "non_standard"

write.xlsx(results_df, file = "Output_code/Data/Table_3_and_Appendix_H_none_granted.xlsx", rowNames = F)

####2.3.2. Treatment granted-----
Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
Current_non_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]

#so the treatment will now be companies that had at least one AI patent granted
Current_treated_new_treat <-Current_treated[Current_treated$total_granted_ai >0,]
length(unique(Current_treated_new_treat$id)) #480 granted

DataLong_Subs_sim_granted <- rbind(Current_treated_new_treat, Current_non_treated)
table(DataLong_Subs_sim_granted$Quartile)

DataLong_Subs_sim_group0 <- DataLong_Subs_sim_granted
DataLong_Subs_sim_group1 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Top",]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "IQR" ,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Bottom",]

#####2.3.2.1.Calculate the effects Non-standardized across groups -----
#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#summary(agg.simple)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#####2.3.2.2.Calculate the effects Non-standardized for the whole dataset -----
#Group 4
n_group = 4
variable_n = 9
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 10
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 11
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 12
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 13
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 14
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 15
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 16
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 17
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 18
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 19
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 20
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 21
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 22
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 23
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 24
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 25
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 26
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 27
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 28
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 29
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 30
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 31
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 32
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 33
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se           
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "granted"
results_df$mode <- "non_standard"

write.xlsx(results_df, file = "Output_code/Data/Table_3_and_Appendix_H_granted.xlsx", rowNames = F)

##2.4. Treatment with more than 1 granted patents--------
###2.4.1.Treatment granted 2 granted patents-----
Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
Current_non_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]

#so the treatment will now be companies that had at least one AI patent granted
Current_treated_new_treat <-Current_treated[Current_treated$total_granted_ai >1,]
length(unique(Current_treated_new_treat$id)) #480 granted

DataLong_Subs_sim_granted <- rbind(Current_treated_new_treat, Current_non_treated)
table(DataLong_Subs_sim_granted$Quartile)

DataLong_Subs_sim_group0 <- DataLong_Subs_sim_granted
DataLong_Subs_sim_group1 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Top",]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "IQR" ,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Bottom",]

######2.4.1.1.Calculate the effects Non-standardized across groups -----
#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#summary(agg.simple)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "granted_2patents"
results_df$mode <- "non_standard"

write.xlsx(results_df, file = "Output_code/Data/Appendix_H_2plus_granted.xlsx", rowNames = F)

###2.4.2. Treatment granted 3 granted patents-----
Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
Current_non_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]

#so the treatment will now be companies that had at least one AI patent granted
Current_treated_new_treat <-Current_treated[Current_treated$total_granted_ai >2,]
length(unique(Current_treated_new_treat$id)) #480 granted

DataLong_Subs_sim_granted <- rbind(Current_treated_new_treat, Current_non_treated)
table(DataLong_Subs_sim_granted$Quartile)

DataLong_Subs_sim_group0 <- DataLong_Subs_sim_granted
DataLong_Subs_sim_group1 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Top",]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "IQR" ,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_granted[DataLong_Subs_sim_granted$Quartile == "Bottom",]

#####2.4.2.1.Calculate the effects Non-standardized across groups -----
#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#summary(agg.simple)

#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Innovative performance 
#Group 1
variable_n = 2
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Number of specializations
#Group 1
variable_n = 3
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique Classes
#Group 1
variable_n = 4
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Unique subclasses
#Group 1
variable_n = 5
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Herfindahl
#Group 1
variable_n = 6
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### Shannon
#Group 1
variable_n = 7
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#### AI codes
#Group 1
variable_n = 8
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df$dataset <- "granted_3patents"
results_df$mode <- "non_standard"

write.xlsx(results_df, file = "Output_code/Data/Appendix_H_3plus_granted.xlsx", rowNames = F)

##2.5. Contrasting successful from unsuccessful AI innovators-------
Group2 <-read.csv("Output_code/Data/Matched_Group2_applied_for_but_nonegranted_vs_granted.csv", sep = ";", header = TRUE, dec=",") 
DataLong_Subs_sim %<>% group_by(Company) %>% mutate(total_granted_ai = max(n_granted_yes_ai, na.rm =T )) %>% ungroup()

#select the companies that introduced AI
Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]

#Adjust year of treatment to the year - 1
Current_treated$Year_before_adoption <- as.integer(Current_treated$YearFirstAdoption) -1
#Adjust treat to total_granted_ai >0;
Current_treated %<>% group_by(Company) %>% mutate(new_treat = ifelse(total_granted_ai==0,0,1) ) %>% ungroup()
Current_treated_matched <- Current_treated[Current_treated$Company %in% Group2$Company,]
#replace NAs in subs by 0
#Current_treated_matched$No_subs_extended[is.na(Current_treated_matched$No_subs_extended)] <- 0
length(unique(Current_treated_matched$id)) #216
length(unique(Current_treated_matched$Company)) #216, so it's correct!

###2.5.1. F-tests ----- 
new_variables = c(
  #necessary ones:
  "Company","new_treat", "Quartile", "CurrentYear","YearFirstAdoption", "Year_before_adoption",
  
  #main ones:
  "Relatedness_Cos2", "NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass","Herfindahl", "Shannon", "AIcodes",
  
  #quality measures 
  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication",
  "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",
  #subs and HQs 
  "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", 
  "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries", 
  
  #aditional performance variables (3) - knowledge productivity
  "Patent_productivity", "Patent_output_per_employee", "RandD_Year",
  
  #extra ones
  "Size_class", "Agegroup", "Date_Incorporation", "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal", "No_subs_extended", "operating_revenue_turnover",
  "number_of_employees", "number_patents_year","NoPatentsYearGUOalone","NoPatentsYearALLSubs","Stock_value", "Market_capitalisation",
  "total_granted_ai")

reordered_data <- Current_treated_matched %>%
  select(all_of(new_variables))

reordered_data_Year_tminus1 <- reordered_data %>%
  filter(Year_before_adoption == CurrentYear)

tminus1 <- st(reordered_data_Year_tminus1,  group = "new_treat",  group.test = TRUE,
              summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

reordered_data_Year_t <- reordered_data %>%
  filter(as.integer(YearFirstAdoption) == CurrentYear)

t <- st(reordered_data_Year_t,  group = "new_treat",  group.test = TRUE,
        summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

reordered_data_Year_t_plus1 <- reordered_data %>%
  filter(as.numeric(YearFirstAdoption) +1 == CurrentYear)

t_plus1 <- st(reordered_data_Year_t_plus1,  group = "new_treat",  group.test = TRUE,
              summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

reordered_data_Year_t_plus2 <- reordered_data %>%
  filter(as.numeric(YearFirstAdoption) +2== CurrentYear)

t_plus2 <- st(reordered_data_Year_t_plus2,  group = "new_treat",  group.test = TRUE,
              summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

reordered_data_Year_t_plus3 <- reordered_data %>%
  filter(as.numeric(YearFirstAdoption)+3 == CurrentYear)

t_plus3<-st(reordered_data_Year_t_plus3,  group = "new_treat",  group.test = TRUE,
            summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

write.csv2(tminus1, file = "Output_code/Data/Descriptive_statistics_AppendixI_tminus1.csv", row.names = F) 
write.csv2(t, file = "Output_code/Data/Descriptive_statistics_AppendixI_t.csv", row.names = F) 
write.csv2(t_plus1, file = "Output_code/Data/Descriptive_statistics_AppendixI_t_plus1.csv", row.names = F) 
write.csv2(t_plus2, file = "Output_code/Data/Descriptive_statistics_AppendixI_t_plus2.csv", row.names = F) 
write.csv2(t_plus3, file = "Output_code/Data/Descriptive_statistics_AppendixI_t_plus3.csv", row.names = F) 

###2.5.2.Wilcoxon rank-sum test-----
variables_to_test <- c(
  #main ones:
  "Relatedness_Cos2", "NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass","Herfindahl", "Shannon", "AIcodes",
  
  #quality measures 
  "Rate_granted", "avg_claims", "avg_number_of_family_members", "avg_backward_citations", "avg_forward_citations", "avg_time_filing_to_publication",
  "avg_time_filing_to_grant", "avg_time_publication_to_grant", "n_granted_na", "n_granted_yes",
  #subs and HQs 
  "Rate_success_subs", "Rate_success_HQ", "n_granted_yes_HQ", "n_granted_na_HQ", "avg_time_filing_to_publication_HQ", "avg_time_filing_to_grant_HQ", 
  "avg_time_publication_to_grant_HQ", "n_granted_yes_subsidiaries", "n_granted_na_subsidiaries", "avg_time_filing_to_publication_subsidiaries", "avg_time_filing_to_grant_subsidiaries", "avg_time_publication_to_grant_subsidiaries", 
  
  #aditional performance variables (3) - knowledge productivity
  "Patent_productivity", "Patent_output_per_employee", "RandD_Year",
  
  #extra ones
  # "Size_class", "Agegroup", 
  "Date_Incorporation", "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal", "No_subs_extended", "operating_revenue_turnover",
  "number_of_employees", "number_patents_year","NoPatentsYearGUOalone","NoPatentsYearALLSubs","Stock_value", "Market_capitalisation",
  "total_granted_ai")

#for t-1
wilcox_results_t_minus1 <- list()
for (variable in variables_to_test) {
  test_formula <- as.formula(paste(variable, "~ new_treat"))
  test_result <- wilcox.test(test_formula, data = reordered_data_Year_tminus1)
  wilcox_results_t_minus1[[variable]] <- test_result$p.value
}
wilcox_results_t_minus1<-as.data.frame(wilcox_results_t_minus1) #for wilcox, p-value > 0.05 means no significant differences, and values below mean significant differences
wilcox_results_t_minus1$Period <- "t-1"

#for t
wilcox_results_t <- list()
for (variable in variables_to_test) {
  test_formula <- as.formula(paste(variable, "~ new_treat"))
  test_result <- wilcox.test(test_formula, data = reordered_data_Year_t)
  wilcox_results_t[[variable]] <- test_result$p.value}
wilcox_results_t<-as.data.frame(wilcox_results_t) 
wilcox_results_t$Period <- "t"

#for t+1
wilcox_results_t_plus1 <- list()
for (variable in variables_to_test) {
  test_formula <- as.formula(paste(variable, "~ new_treat"))
  test_result <- wilcox.test(test_formula, data = reordered_data_Year_t_plus1)
  wilcox_results_t_plus1[[variable]] <- test_result$p.value}
wilcox_results_t_plus1<-as.data.frame(wilcox_results_t_plus1) 
wilcox_results_t_plus1$Period <- "t+1"

#for t+2
wilcox_results_t_plus2 <- list()
for (variable in variables_to_test) {
  test_formula <- as.formula(paste(variable, "~ new_treat"))
  test_result <- wilcox.test(test_formula, data = reordered_data_Year_t_plus2)
  wilcox_results_t_plus2[[variable]] <- test_result$p.value}
wilcox_results_t_plus2<-as.data.frame(wilcox_results_t_plus2) 
wilcox_results_t_plus2$Period <- "t+2"

#for t+3
wilcox_results_t_plus3 <- list()
for (variable in variables_to_test) {
  test_formula <- as.formula(paste(variable, "~ new_treat"))
  test_result <- wilcox.test(test_formula, data = reordered_data_Year_t_plus3)
  wilcox_results_t_plus3[[variable]] <- test_result$p.value}
wilcox_results_t_plus3<-as.data.frame(wilcox_results_t_plus3) 
wilcox_results_t_plus3$Period <- "t+3"

wilcox <- rbind(wilcox_results_t_minus1, wilcox_results_t,wilcox_results_t_plus1, wilcox_results_t_plus2,wilcox_results_t_plus3)

transposed_df <- wilcox %>%
  pivot_longer(
    cols = -Period,
    names_to = "variables",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Period,
    values_from = value)

write.csv2(transposed_df, file = "Output_code/Data/Descriptive_statistics_AppendixI_Wilcox_results.csv", row.names = F) 

#3.Descriptive statistics and correlations-----
##3.1.Statistics All data------
rm(list=ls())
FinalDataset <- read.csv("Input_code/Data_all_years_all_MNEs.csv", sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance
FinalDataset$Nace_4d <- as.character(FinalDataset$Nace_4d)

CategoriesNace <-read.csv("Output_code/Data/Distance_measure.csv", header = TRUE)
CategoriesNace$Nace_4d <-as.character(CategoriesNace$Nace_4d)

FinalDataset <- left_join(FinalDataset,CategoriesNace, by = "Nace_4d")
table(is.na(FinalDataset$Quartile)) #539160     F; 280  T
table(FinalDataset$Quartile) #Bottom 186320    IQR 239220    Top    113620 

FinalDataset$Quartile[is.na(FinalDataset$Quartile)] <- "Bottom"
table(is.na(FinalDataset$Quartile)) #539440  
table(FinalDataset$Quartile) #Bottom 186600     IQR 239220    Top    113620 

#pick just one year of analysis:
FinalDataset_2019 <- FinalDataset[FinalDataset$CurrentYear== 2019,]
FinalDataset_summary <- FinalDataset_2019[,c("Relatedness_Cos2", "NoSubs_calculated", "NoSubsWithPatents", "Size_class", 
                                             "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal","No_employees_Year",
                                             "Date_Incorporation",  "Specializations_Number",
                                             "Herfindahl", "Shannon", "Quartile",
                                             "UniqueCodes", "UniqueSubclass")] 

#group test for Size_class
st(FinalDataset_summary, title = "Variables Quartile", group = "Quartile",  group.test = TRUE)

Quartile <- st(FinalDataset_summary, group = "Quartile",  group.test = TRUE,
               summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

write.csv2(Quartile, file = "Output_code/Data/Descriptive_statistics_all_MNEs_by_Quartile.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
rm( Quartile)

##3.2.Statistics matched data-----
DataLong_Subs_2006 <- read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",") #
length(unique(DataLong_Subs_2006$subclass)) #1257
length(unique(DataLong_Subs_2006$Company)) #2390

#get the mean of the last 5 years
DataLong_Subs_2006 %<>% group_by(Company, subclass) %>% 
  mutate(across(NoPatentsYearGUOtotal,
                .fns = list(avg = ~ rollmean(.,k=5,fill=NA,align = 'center'))))

#new: Relatedness_lagged 
DataLong_Subs_2006 %<>% group_by(Company, subclass) %>% 
  mutate(NoPatentsYearGUOtotal_avg_fixed = lag(NoPatentsYearGUOtotal_avg)) %>% 
  mutate(Relatedness_lagged = lag(Relatedness_Cos2)) 

DataLong_Subs_2006 <- subset(DataLong_Subs_2006, select = -c(NoPatentsYearGUOtotal_avg) )

table(DataLong_Subs_2006$Group)

#pick the variables of interest from FinalDataset and merge them with DataLong_Subs_2006 by CurrentYear and Company
For_Match <- FinalDataset[,c("Company","CurrentYear","Quartile", "Specializations_Number","Shannon","Herfindahl","NoSubs_calculated", "NoSubsWithPatents", "No_employees_Year",
                             "UniqueCodes", "UniqueSubclass")] #new 
DataLong_Subs_2006 <- left_join(DataLong_Subs_2006, For_Match, by=c("Company","CurrentYear"),na_matches="never")
rm(For_Match)

DataLong_Subs_2006 %<>% group_by(Company, subclass) %>% 
  mutate(N_specializations_lagged = lag(Specializations_Number.x)) 

DataLong_Subs_2006_summary <- DataLong_Subs_2006[,c("Relatedness_Cos2", "Size_class", "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal",
                                                    "Company","treat","Date_Incorporation", "Group", "CurrentYear", 
                                                    "NoPatentsYearGUOtotal_avg_fixed", "Nace_4d", "YearFirstAdoption",
                                                    "Specializations_Number.x","Shannon.x","Herfindahl.x","NoSubs_calculated", "NoSubsWithPatents",
                                                    "Quartile", "No_employees_Year",
                                                    "Relatedness_lagged", "N_specializations_lagged", 
                                                    "UniqueCodes.x", "UniqueSubclass.x" )] 

DataLong_Subs_2006_summary <- DataLong_Subs_2006_summary[DataLong_Subs_2006_summary$Group == DataLong_Subs_2006_summary$CurrentYear,]

DataLong_Subs_2006_summary <- subset(DataLong_Subs_2006_summary, select = -c(Company, Group,  YearFirstAdoption, Nace_4d) )#, "CurrentYear"

st(DataLong_Subs_2006_summary,  group = "treat",  group.test = TRUE) #title = "Treatment differences",

table(DataLong_Subs_2006_summary$CurrentYear) #from 2011 to 2019
Treated_2011 <- DataLong_Subs_2006_summary[DataLong_Subs_2006_summary$CurrentYear != 2011,]
Treated_2019 <- DataLong_Subs_2006_summary[DataLong_Subs_2006_summary$CurrentYear != 2019,]

st(Treated_2011,  group = "treat",  group.test = TRUE,
   summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'))
#so, when the 2011 group is excluded, there is no sign diff in the number of patents anymore!
rm(Treated_2011, Treated_2019)

st(DataLong_Subs_2006_summary,  group = "treat",  group.test = TRUE,
   summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'))

Treated <- st(DataLong_Subs_2006_summary,  group = "treat",  group.test = TRUE,
              summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

write.csv2(Treated, file = "Output_code/Data/Descriptive_statistics_matched_companies_Treated.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of

##3.3.Analyse correlations-----
#For the matched sample
data_normalized <- DataLong_Subs_2006_summary[,c("Relatedness_Cos2", "Specializations_Number.x",
                                                 "Date_Incorporation","No_AI_PatentsYearGUOtotal", "NoPatentsYearGUOtotal")] 

names(data_normalized) <- c("Relatedness", "N. of specializations","Date of incorporation", "N. of AI patents","N. of patents") 
data_normalized <- scale(data_normalized)
corr_matrix <- cor(na.omit(data_normalized))

jpeg("Output_code/Figures/Correlation_table_matched_companies.jpg", width = 15, height = 12, units = 'in', res = 300)
ggcorrplot(corr_matrix, legend.title = "Correlation", hc.order=F,lab = TRUE, lab_size = 8)+
  theme(axis.text.x=element_text(size=25),axis.text.y=element_text(size=25), legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=24), legend.text = element_text(size=20))
dev.off()

#For the whole dataset 
data_normalized <- FinalDataset_2019[,c("Relatedness_Cos2", "Specializations_Number",
                                   "Date_Incorporation","No_AI_PatentsYearGUOtotal", "NoPatentsYearGUOtotal")] 

names(data_normalized) <- c("Relatedness", "N. of specializations","Date of incorporation", "N. of AI patents","N. of patents") 
data_normalized <- scale(data_normalized)
corr_matrix <- cor(na.omit(data_normalized))
jpeg("Output_code/Figures/Correlation_table_all_companies.jpg", width = 15, height = 12, units = 'in', res = 300)
ggcorrplot(corr_matrix, legend.title = "Correlation", hc.order=F,lab = TRUE, lab_size = 8)+
  theme(axis.text.x=element_text(size=25),axis.text.y=element_text(size=25), legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=24), legend.text = element_text(size=20))
dev.off()

#4.Measuring effects of acquiring patents------
##4.1. For ratio 5 and using the Quartile measure-----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Input_code/Big_files_ignore/NEW_FIG/Matched_companies_Acquis_match_ratio5.csv", sep = ";", header = TRUE, dec=",") #
Data_all <- read.csv("Input_code/Data_all_years_all_MNEs.csv", sep = ";", header = TRUE, dec=",") #

DataLong_Subs_sim <- left_join(DataLong_Subs_sim, Data_all[,c("Company", "CurrentYear","Specializations_Number", 
                                                              "UniqueCodes", "UniqueSubclass")], by = c("Company", "CurrentYear"))

table(is.na(DataLong_Subs_sim$Quartile)) #672    F
table(DataLong_Subs_sim$Quartile) # IQR 420     top 252         

#add dynamic capabilities
dynamic_capabilities <- read.csv("Input_code/Big_files_ignore/df_dynamic_capabilities.csv", header = TRUE)
names(dynamic_capabilities)[names(dynamic_capabilities) == 'Year'] <- 'CurrentYear'
DataLong_Subs_sim <- left_join(DataLong_Subs_sim, dynamic_capabilities, by=c("Company", "CurrentYear"))

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #IQR 70      top 42   
table(nontreat_test$Quartile) #iqr 350      top 210   
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #iqr 5  top 3      
rm(treat_test, nontreat_test)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top"& is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" & is.na(DataLong_Subs_sim$Quartile) == F,]

calculate_ci_and_significance <- function(att, se, group, variable_name) {
  # Confidence intervals at different significance levels
  ci_90_low <- att - 1.645 * se  # 90% confidence interval
  ci_90_high <- att + 1.645 * se
  
  ci_95_low <- att - 1.96 * se   # 95% confidence interval
  ci_95_high <- att + 1.96 * se
  
  ci_99_low <- att - 2.576 * se  # 99% confidence interval
  ci_99_high <- att + 2.576 * se
  
  # Determine the highest significance level
  significance <- ""
  
  if (ci_90_low > 0 | ci_90_high < 0) {
    significance <- "*"
  }
  if (ci_95_low > 0 | ci_95_high < 0) {
    significance <- "**"
  }
  if (ci_99_low > 0 | ci_99_high < 0) {
    significance <- "***"
  }
  if (!(ci_90_low > 0 | ci_90_high < 0)) {
    significance <- "" # No significance if not significant even at 90%
  }
  
  # Create a dataframe to store the results
  results_df <- data.frame(
    ATT = att,
    Std_Error = se,
    CI_90_Low = ci_90_low,
    CI_90_High = ci_90_high,
    CI_95_Low = ci_95_low,
    CI_95_High = ci_95_high,
    CI_99_Low = ci_99_low,
    CI_99_High = ci_99_high,
    Significance = significance,
    Group = group,
    Variable = variable_name
  )
  
  return(results_df)
}

#define the fixed variables
tname = "CurrentYear" 
idname = "id"
gname = "first.treat"

#define the possible groups
groups = c("DataLong_Subs_sim_group1","DataLong_Subs_sim_group2","DataLong_Subs_sim_group3", "DataLong_Subs_sim_group0")

#define the possible variables
variables = c(#main ones (11) - linked to knowledge creation, usage, and recombination
  "Relatedness_Cos2", #relatedness
  "NoPatentsYearGUOtotal", #innovative performance
  "Specializations_Number", #number of specializations
  "UniqueCodes", #for Sections
  "UniqueSubclass", #for 4-digit
  "Dynamic_capabilities") #dynamic capabilities, ignored for now due to too many missing variables

###Calculate the effects Non-standardized
DataLong_Subs_sim_group0 <- DataLong_Subs_sim

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 2
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 1
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df_non_stand <- results_df %>%
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  arrange(Variable, Group)

results_df_non_stand$Effect <- "non-standard"
results_df_non_stand$Matching <- "1_to_5"

######Standard
DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

DataLong_Subs_sim_group1[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 2
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 1
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df_stand <- results_df %>%
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  arrange(Variable, Group)

results_df_stand$Effect <- "standard"
results_df_stand$Matching <- "1_to_5"

results_df_ratio5 <- rbind(results_df_non_stand, results_df_stand)
rm(results_df_stand, results_df_non_stand)

write.xlsx(results_df_ratio5, file = "Output_code/Data/Table_5b.xlsx", rowNames = F) 

##4.2. For ratio 1 and 5, and using the Nace 2-d measure -----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Input_code/Big_files_ignore/NEW_FIG/Matched_companies_Acquis_match_Nace_2d.csv", sep = ";", header = TRUE, dec=",") #
Data_all <- read.csv("Input_code/Data_all_years_all_MNEs.csv", sep = ";", header = TRUE, dec=",") #

DataLong_Subs_sim <- left_join(DataLong_Subs_sim, Data_all[,c("Company", "CurrentYear","Specializations_Number", 
                                                              "UniqueCodes", "UniqueSubclass")], by = c("Company", "CurrentYear"))

table(is.na(DataLong_Subs_sim$Quartile)) #672    F
table(DataLong_Subs_sim$Quartile) # IQR 420     top 252         

#add dynamic capabilities
dynamic_capabilities <- read.csv("Input_code/Big_files_ignore/df_dynamic_capabilities.csv", header = TRUE)
names(dynamic_capabilities)[names(dynamic_capabilities) == 'Year'] <- 'CurrentYear'
DataLong_Subs_sim <- left_join(DataLong_Subs_sim, dynamic_capabilities, by=c("Company", "CurrentYear"))

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #IQR 70      top 42   
table(nontreat_test$Quartile) #iqr 350      top 210   
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #iqr 5  top 3      
rm(treat_test, nontreat_test)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top"& is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" & is.na(DataLong_Subs_sim$Quartile) == F,]

calculate_ci_and_significance <- function(att, se, group, variable_name) {
  # Confidence intervals at different significance levels
  ci_90_low <- att - 1.645 * se  # 90% confidence interval
  ci_90_high <- att + 1.645 * se
  
  ci_95_low <- att - 1.96 * se   # 95% confidence interval
  ci_95_high <- att + 1.96 * se
  
  ci_99_low <- att - 2.576 * se  # 99% confidence interval
  ci_99_high <- att + 2.576 * se
  
  # Determine the highest significance level
  significance <- ""
  
  if (ci_90_low > 0 | ci_90_high < 0) {
    significance <- "*"
  }
  if (ci_95_low > 0 | ci_95_high < 0) {
    significance <- "**"
  }
  if (ci_99_low > 0 | ci_99_high < 0) {
    significance <- "***"
  }
  if (!(ci_90_low > 0 | ci_90_high < 0)) {
    significance <- "" # No significance if not significant even at 90%
  }
  
  # Create a dataframe to store the results
  results_df <- data.frame(
    ATT = att,
    Std_Error = se,
    CI_90_Low = ci_90_low,
    CI_90_High = ci_90_high,
    CI_95_Low = ci_95_low,
    CI_95_High = ci_95_high,
    CI_99_Low = ci_99_low,
    CI_99_High = ci_99_high,
    Significance = significance,
    Group = group,
    Variable = variable_name
  )
  
  return(results_df)
}

#define the fixed variables
tname = "CurrentYear" 
idname = "id"
gname = "first.treat"

#define the possible groups
groups = c("DataLong_Subs_sim_group1","DataLong_Subs_sim_group2","DataLong_Subs_sim_group3", "DataLong_Subs_sim_group0")

#define the possible variables
variables = c(#main ones (11) - linked to knowledge creation, usage, and recombination
  "Relatedness_Cos2", #relatedness
  "NoPatentsYearGUOtotal", #innovative performance
  "Specializations_Number", #number of specializations
  "UniqueCodes", #for Sections
  "UniqueSubclass", #for 4-digit
  "Dynamic_capabilities") #dynamic capabilities, ignored for now due to too many missing variables

###Calculate the effects Non-standardized
DataLong_Subs_sim_group0 <- DataLong_Subs_sim

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 2
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 1
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#variable_n = 4 #this one doesn't work, for some reason
#variable_name <- variables[variable_n]
#estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
#                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
#agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#att <- agg.simple$overall.att          
#se <- agg.simple$overall.se            # Standard Error
#results_new <- calculate_ci_and_significance(att, se, group, variable_name)
#results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df_non_stand <- results_df %>%
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  arrange(Variable, Group)

results_df_non_stand$Effect <- "non-standard"
results_df_non_stand$Matching <- "Nace_2d"

######Standard
DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

DataLong_Subs_sim_group1[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2","NoPatentsYearGUOtotal", "Specializations_Number", "UniqueCodes", "UniqueSubclass", "Dynamic_capabilities")])

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

n_group = 2
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#variable_n = 5
#variable_name <- variables[variable_n]
#estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
#                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
#agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#att <- agg.simple$overall.att          
#se <- agg.simple$overall.se            # Standard Error
#results_new <- calculate_ci_and_significance(att, se, group, variable_name)
#results_df <- rbind(results_df, results_new)

n_group = 1
variable_n = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 3
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 4
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

variable_n = 5
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

results_df_stand <- results_df %>%
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  arrange(Variable, Group)

results_df_stand$Effect <- "standard"
results_df_stand$Matching <- "Nace_2d"

results_df_nace_2d <- rbind(results_df_non_stand, results_df_stand)
rm(results_df_stand, results_df_non_stand)

write.xlsx(results_df_nace_2d, file = "Output_code/Data/Table_5a.xlsx", rowNames = F)
#5.New Figure 3------
rm(list=ls())
options(scipen=999)
raw <- read_excel("Input_code/Table 3.xlsx")

variable_order <- c(
  "Relatedness",
  "Innovative Performance",
  "Number of specializations",
  "Sections",
  "Subclasses",
  "Herfindahl index",
  "Shannon entropy index",
  "No. of spec. in the 5 techn. most related to AI")

df_long <- raw %>%
  tidyr::fill(Variable) %>%
  pivot_longer(cols = -c(Variable, Q),
               names_to = "Group", values_to = "Value") %>%
  mutate(
    est_chr = str_extract(Value, "(?<!\\()[−-]?\\d+(?:,\\d+)?"),
    sd_chr  = str_extract(Value, "(?<=\\()[^)]+(?=\\))"),
    Estimate = as.numeric(str_replace(est_chr, ",", ".")),
    SD       = as.numeric(str_replace(sd_chr,  ",", ".")),
    Group = factor(Group,
                   levels = c("None granted","Granted 1+","Granted 2+","Granted 3+")),
    Q = factor(Q, levels = c("Q1", "IQR", "Q4")),
    Variable = factor(Variable, levels = variable_order) ) %>%
  filter(!is.na(Variable), !is.na(Estimate))

df_long$Variable <- fct_recode(df_long$Variable,
                               "Number of unique IPC Subclasses used" = "Subclasses")
df_long$Variable <- fct_recode(df_long$Variable,
                               "Number of unique IPC Sections used" = "Sections")

jpeg("Output_code/Figures/Fig3_Table3.jpg", width = 10, height = 8, units = 'in', res = 400)
ggplot(df_long, aes(x = Group, y = Estimate, fill = Group)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Estimate - SD, ymax = Estimate + SD), width = 0.2) +
  facet_grid(rows = vars(Variable), cols = vars(Q), scales = "free_y") +
  labs(title = "Effects by grant group and quantile",
       y = "Effect (estimate ± SD)", x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text.y = element_text(angle = 0))
dev.off()
#end-----