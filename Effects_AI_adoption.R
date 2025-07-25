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

#1.Measuring technological distance-----
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

##1.1.Technological distance of 4-digit IPC codes to AI-----
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

##1.2.Technological distance of NACE sectors to AI-----
#let's start by removing AI as a separate technology
length(unique(all_patents$PubNo)) #7,354,214
table(all_patents$AIpatent) #334,316 
all_patents <- all_patents[all_patents$Subclass != "Artificial_Intelligence",]
length(unique(all_patents$PubNo)) #7,354,214 (same number, because we just removed the repeated added AI patents)
table(all_patents$AIpatent) #257,044 (different number because we removed the extra AI patents but not the original ones)

#now let's add AI as if it were an industry:
AIPatents<- all_patents[all_patents$AIpatent == "Yes",]

AIPatents$Nace_4d <- "AI"
length(unique(all_patents$Company)) #27707 MNEs
length(unique(all_patents$Nace_4d)) #620 NACEs
#merge this new sector back to the longer dataset:
all_patents<-rbind(all_patents, AIPatents)
length(unique(all_patents$Company)) #27707 MNEs
length(unique(all_patents$Nace_4d)) #621 NACEs
rm(AIPatents)

#count total of patents of each sector, and total of AI patents;
all_patents %<>% group_by(Nace_4d) %>% 
  mutate(TotalPatents = length(unique(PubNo))) %>% #get the higher share of usage
  mutate(TotalAIPatents = length(na.omit(unique(PubNo[AIpatent == "Yes"])))) %>% 
  mutate(ShareAI = TotalAIPatents/TotalPatents) %>% 
  ungroup()

describe(all_patents$TotalPatents) #I'm using the describe from the psych library
#so, number of patents varies from 1 to 936,534  , with a median value of 142,695. Let's exclude the sectors that are way below that, let's say, by having less than 500 patents:
all_patents <- all_patents[all_patents$TotalPatents >= 500,]
length(unique(all_patents$Nace_4d)) #280 sectors (including the fake AI one) with at least 500 patents

#Finally, let's apply the two functions we created before:
reg_tech <- group_by_applnID(all_patents) #this gets the relative participation of each IPC code on each patent
reg_tech <- group_by_NACE_and_Subclass(reg_tech) #this gets the sum of the relative participation of each code for each NACE

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
#let's transform this back into a matrix:
rel_sectors_codes %<>%
  arrange(Subclass, mat_reg_tech) %>%
  pivot_wider(names_from = Subclass, values_from = RCA, values_fill = list(RCA = 0)) #transform on a binary-like matrix

#replace NA values by 0
rel_sectors_codes[is.na(rel_sectors_codes)] <- 0
#let's set the AI sector as a target/reference for calculating the distance:
predicted <- rel_sectors_codes[rel_sectors_codes$mat_reg_tech=="AI",]
predicted<- predicted[,(-1)]
predicted<-as.vector(predicted,mode='numeric')

#let's calculate the distance from all sectors to this AI reference:
AverageDistance <- as.data.frame("Nace_4d", col.names = "a")

for (i in 1:length(rel_sectors_codes$mat_reg_tech)){
  
  AverageDistance[i] <- mae(as.vector(rel_sectors_codes[i,][,(-1)],mode='numeric'),predicted)
}

AverageDistance <- as.data.frame(t(AverageDistance))
Sectors <- as.data.frame(rel_sectors_codes[,1])
AverageDistance <- cbind(AverageDistance,Sectors)
names(AverageDistance) <- c("AverageDistance", "Nace_4d")
#let's create the group category based on quartiles. the lower the distance to AI, the closer the sector is to it
AverageDistance$Quartile <- ifelse(AverageDistance$AverageDistance>=quantile(AverageDistance$AverageDistance)[[4]], "bottom",
                                                ifelse(AverageDistance$AverageDistance<quantile(AverageDistance$AverageDistance)[[4]] & 
                                                         AverageDistance$AverageDistance>quantile(AverageDistance$AverageDistance)[[2]], "medium", "top"))
#and save this file
write.csv2(AverageDistance, file = "Output_code/Data/Distance_NACE_sectors_to_AI.csv", row.names = F) #MAE_9a.csv

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

#1.1.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 1.3202        0.2976     0.5535      2.0868 *
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h1_Relatedness_all.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H1) All quartiles - Relatedness") + theme(legend.position="right") +   ylab("Knowledge-relatedness") + 
  xlab("Length of exposure") + scale_color_manual(values=c("grey70","orange"),labels = c("Pre-treatment", "Post-treatment")) + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1, alpha = 0.3)
dev.off()

#Simple Aggregation Innovative performance
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", tname = "CurrentYear", idname = "id", 
                        gname = "first.treat", xformla = ~1, data = DataLong_Subs_sim, alp=.01
)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 18.14        Std. Error: 3.6236     [ 99%  Conf. Int.]: 8.8062     27.4737 ****

#1.1.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 27.982        8.3811     6.3936     49.5703 *
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h2_N_patents_all.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H2) All quartiles - Innovative performance") + 
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
#The did package was not created aiming at testing moderating effects, so let's do it with a regular implementation:
# Create Create the Post-Treatment binary Variable
DataLong_Subs_sim$post_treatment <- ifelse(DataLong_Subs_sim$CurrentYear >= DataLong_Subs_sim$first.treat & 
                                               DataLong_Subs_sim$Group <= DataLong_Subs_sim$CurrentYear, 1, 0)

#Basic DiD model without the moderator for relatedness
model <- lm(Relatedness_Cos2 ~ treat * post_treatment, data = DataLong_Subs_sim)
summary(model) ##treat:post_treatment  1.3380***, post_treatment 1.2469***, treat 0.6618***, 

# Basic DiD model WITHOUT the moderator for innovative performance
model <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment, data = DataLong_Subs_sim)
summary(model) #treat:post_treatment   29.473***, post_treatment 33.084*** , treat 4.478. 

# DiD model with Relatedness_Cos2 as a moderator 
model_moderation <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment * Relatedness_Cos2, data = DataLong_Subs_sim)
summary(model_moderation) #treat:post_treatment -21.9771**, treat:post_treatment:Relatedness_Cos2 1.9468***,
#treat -18.7609***, post_treatment -2.4439 Relatedness_Cos2 6.1295***, 
#treat:Relatedness_Cos2 1.9534*** , post_treatment:Relatedness_Cos2 1.0060***, 

###2.1.2.Standardized - Relatedness and number of patents regardless of sector ------
DataLong_Subs_sim_standard <- DataLong_Subs_sim
DataLong_Subs_sim_standard[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_standard[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

#Simple Aggregation Relatedness
example_attgt <- att_gt(yname = "Relatedness_Cos2", tname = "CurrentYear", idname = "id", gname = "first.treat",
                        xformla = ~1, data = DataLong_Subs_sim_standard, alp=.01)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 0.0965  Std. Error: 0.0185   [ 99%  Conf. Int.]: 0.0489      0.1441 *

#Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #effect 0.118        0.0275     0.0471      0.1889 *
ggdid(agg.es) 

#Simple Aggregation Innovative Performance
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", tname = "CurrentYear", idname = "id", gname = "first.treat",
                        xformla = ~1,data = DataLong_Subs_sim_standard, alp=.01)

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple) #effect: 0.0874        Std. Error: 0.0159     [ 99%  Conf. Int.]: 0.0466      0.1282 ***

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

# Basic DiD model WITHOUT the moderator for innovative performance
model <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment, data = DataLong_Subs_sim_standard)
summary(model) #treat:post_treatment   0.142023***, post_treatment 0.159419*** , treat 0.021576. 

# DiD model with Relatedness_Cos2 as a moderator 
model_moderation <- lm(NoPatentsYearGUOtotal ~ treat * post_treatment * Relatedness_Cos2, data = DataLong_Subs_sim_standard)
summary(model_moderation) #treat:post_treatment 0.01644, treat:post_treatment:Relatedness_Cos2 0.10497***,
#treat 0.03235*, post_treatment 0.05144**, Relatedness_Cos2 0.33050***, 
#treat:Relatedness_Cos2 0.10533*** , post_treatment:Relatedness_Cos2 0.05425**, 

##2.2. Measuring effects across sectors -----
rm(list=ls())
RCA_All_Years <-read.csv("Input_code/RCA_All_Years_Simplified.csv", sep = ";", header = TRUE, dec=",") 

mat_tech_rel <-read.csv2("Output_code/Data/Matrix_relatedness_technologies_to_AI.csv")#file we created before measuring the relatedness of AI to other technologies
mat_tech_rel <- mat_tech_rel[,c("X", "Artificial_Intelligence")] #select the AI and the other technologies columns
mat_tech_rel <- mat_tech_rel[mat_tech_rel$Artificial_Intelligence >0,] #from 646 to 515 with relatedness above 0
#let's pick the 5 codes with higher relatedness to AI:
AI_codes <- mat_tech_rel[mat_tech_rel$Artificial_Intelligence >.06,1] #5 codes

DataLong_Subs_sim <-read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",")
length(unique(DataLong_Subs_sim$id)) #2514 ids 

RCA_All_Years$AI_code <- ifelse(RCA_All_Years$Subclass %in% AI_codes, "AI_code", "Other_codes")

RCA_All_Years %<>% group_by(id,CurrentYear, AI_code) %>% 
  mutate(AIcodes = sum(RCA)) %>% 
  ungroup()

RCA_All_Years_treat_AI <- RCA_All_Years[RCA_All_Years$AI_code == "AI_code",]
RCA_All_Years_treat_AI <- distinct_at(RCA_All_Years_treat_AI, vars(id,CurrentYear), .keep_all = T)
RCA_All_Years_treat_AI <- RCA_All_Years_treat_AI[,c("id", "CurrentYear", "AIcodes")]

DataLong_Subs_sim <- left_join(DataLong_Subs_sim, RCA_All_Years_treat_AI, by = c("id", "CurrentYear"))

table(is.na(DataLong_Subs_sim$AIcodes)) #13607 F, 21589   T
#Replace NAs by 0:
DataLong_Subs_sim$AIcodes[is.na(DataLong_Subs_sim$AIcodes)] <- 0
table(is.na(DataLong_Subs_sim$AIcodes)) #35196 F

#Insert distance  measure
CategoriesNace <-read.csv("Output_code/Data/Distance_NACE_sectors_to_AI.csv", sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance
DataLong_Subs_sim$Nace_4d <- as.character(DataLong_Subs_sim$Nace_4d)
DataLong_Subs_sim <- left_join(DataLong_Subs_sim,CategoriesNace, by = "Nace_4d")

table(is.na(DataLong_Subs_sim$Quartile)) #33096        F  2100      T
table(DataLong_Subs_sim$Quartile) #bottom 9380              medium  11004            top 12712      

DataLong_Subs_sim$Quartile[is.na(DataLong_Subs_sim$Quartile)] <- "bottom"
table(is.na(DataLong_Subs_sim$Quartile)) #35196 F
table(DataLong_Subs_sim$Quartile) #bottom 11480             medium  11004            top 12712     

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #bottom 5740    medium   5502    top 6356 
table(nontreat_test$Quartile) #bottom 5740    medium   5502    top 6356 
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #bottom 410                    medium  393                      top 454     
rm(treat_test, nontreat_test)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "top" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "medium" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "bottom"& is.na(DataLong_Subs_sim$Quartile) == F,]

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
groups = c("DataLong_Subs_sim_group1","DataLong_Subs_sim_group2","DataLong_Subs_sim_group3")

#define the possible variables
variables = c("Relatedness_Cos2", #relatedness
              "NoPatentsYearGUOtotal", #innovative performance
              "Specializations_Usage", #usage of specializations
              "HigherUsage", #most used technology
              "Specializations_Number", #number of specializations
              "UniqueCodes", #for Sections
              "UniqueSubclass", #for 4-digit
              "Herfindahl",
              "Shannon",
              "AIcodes", #effect on the 5 codes most related to AI
              "No_AI_PatentsYearGUOtotal") #effect on number of AI patents

###2.2.1.Calculate the effects Non-standardized -----
####2.2.1.1.Relatedness----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h3_Relatedness_1Q.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H3) Q1 - Relatedness") + 
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h4_Relatedness_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H4) IQR - Relatedness") + 
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h5_Relatedness_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H5) Q4 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

####2.2.1.2.Innovative performance----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h6_Number_patents_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H6) Q1 - Innovative performance") + 
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h7_Number_patents_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H7) IQR - Innovative performance") + 
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

jpeg("Output_code/Figures/Fig_appendix_h8_Number_patents_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H8) Q4 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

####2.2.1.3.Usage of specializations----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.4.Most used technology----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.5.Number of specializations----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.6.Number of sections ----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.7.Number of subclasses----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.8.Herfindahl index----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.9.Shannon index----
#Group 1
variable_n = 9
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.10.Number AI codes----
#Group 1
variable_n = 10
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.1.18. Number of AI patents----
#Group 1
variable_n = 11
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #1.7991        0.1562     1.4929      2.1053 *
ggdid(agg.es) 

write.xlsx(results_df, file = "Output_code/Data/Results_Effects_non_stand.xlsx", rowNames = F)
rm(results_df)

###2.2.2.Calculate the effects Standardized -----
#standardize per group
DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")])

DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal")])

####2.2.2.1.Relatedness----
#Group 1
variable_n = 1
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, variable_name)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0719        0.0448    -0.0159      0.1598 
ggdid(agg.es) 

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)

agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.108        0.0431     0.0235      0.1925 *
ggdid(agg.es) 

####2.2.2.2.Innovative performance----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.2826        0.1538    -0.0189      0.5841
ggdid(agg.es) 

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) # 0.1222        0.0431     0.0378      0.2066 *
ggdid(agg.es) 

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #0.0597        0.0357    -0.0103      0.1297
ggdid(agg.es) 

####2.2.2.3.Usage of specializations----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.4.Most used technology----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.5.Number of specializations----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.6.Number of sections ----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.7.Number of subclasses----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.8.Herfindahl index----
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.9.Shannon index----
#Group 1
variable_n = 9
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.10.Number AI codes----
#Group 1
variable_n = 10
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

####2.2.2.11. Number of AI patents----
#Group 1
variable_n = 11
n_group = 1
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

#Group 2
n_group = 2
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

#Group 3
n_group = 3
group = print(groups[n_group], quote=FALSE)
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group3)
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#update dataset:
results_new <- calculate_ci_and_significance(att, se, group, variable_name)
results_df <- rbind(results_df, results_new)

#dynamic events:
agg.es <- aggte(estim_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es) #
ggdid(agg.es) 

write.xlsx(results_df, file = "Output_code/Data/Results_Effects_stand.xlsx", rowNames = F)

#3.Descriptive statistics and correlations-----
rm(list=ls())
FinalDataset <- read.csv("Input_code/Data_all_years_all_MNEs.csv", sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance
FinalDataset$Nace_4d <- as.character(FinalDataset$Nace_4d)
CategoriesNace <-read.csv("Output_code/Data/Distance_NACE_sectors_to_AI.csv", sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance

FinalDataset <- left_join(FinalDataset,CategoriesNace, by = "Nace_4d")
table(is.na(FinalDataset$Quartile)) #476620 F; 62820 T
table(FinalDataset$Quartile) #bottom 213880  medium  175220  top 87520     

FinalDataset$Quartile[is.na(FinalDataset$Quartile)] <- "bottom"
table(is.na(FinalDataset$Quartile)) #539440  
table(FinalDataset$Quartile) #bottom 276700     medium 175220       top 87520 

##3.1.Statistics All data------
#pick just one year of analysis:
FinalDataset_2019 <- FinalDataset[FinalDataset$CurrentYear== 2019,]
FinalDataset_summary <- FinalDataset_2019[,c("Relatedness_Cos2", "NoSubs_calculated", "NoSubsWithPatents", "Size_class", 
                                        "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal","No_employees_Year",
                                        "Date_Incorporation", "Specializations_Usage", "HigherUsage", "Specializations_Number",
                                        "Herfindahl", "Shannon", "Quartile",
                                        "UniqueCodes", "UniqueSubclass")] 

#group test for Size_class
st(FinalDataset_summary, title = "Variables Size_class", group = "Size_class",  group.test = TRUE)
Size_class <- st(FinalDataset_summary, group = "Size_class",  group.test = TRUE,
                 summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

Quartile <- st(FinalDataset_summary, group = "Quartile",  group.test = TRUE,
               summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

write.csv2(Size_class, file = "Output_code/Data/Descriptive_statistics_all_MNEs_by_Size_class.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
write.csv2(Quartile, file = "Output_code/Data/Descriptive_statistics_all_MNEs_by_Quartile.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
rm(Size_class, Quartile, CategoriesNace)

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
For_Match <- FinalDataset[,c("Company","CurrentYear","Quartile", "HigherUsage","Specializations_Usage", "Specializations_Number","Shannon","Herfindahl","NoSubs_calculated", "NoSubsWithPatents", "No_employees_Year",
                             "UniqueCodes", "UniqueSubclass")] #new 
DataLong_Subs_2006 <- left_join(DataLong_Subs_2006, For_Match, by=c("Company","CurrentYear"),na_matches="never")
rm(For_Match)

DataLong_Subs_2006 %<>% group_by(Company, subclass) %>% 
  mutate(N_specializations_lagged = lag(Specializations_Number.x)) 

DataLong_Subs_2006_summary <- DataLong_Subs_2006[,c("Relatedness_Cos2", "Size_class", "NoPatentsYearGUOtotal", "No_AI_PatentsYearGUOtotal",
                                                    "Company","treat","Date_Incorporation", "Group", "CurrentYear", 
                                                    "NoPatentsYearGUOtotal_avg_fixed", "Nace_4d", "YearFirstAdoption",
                                                    "HigherUsage.x","Specializations_Usage.x", "Specializations_Number.x","Shannon.x","Herfindahl.x","NoSubs_calculated", "NoSubsWithPatents",
                                                    "Quartile", "No_employees_Year",
                                                    "Relatedness_lagged", "N_specializations_lagged", 
                                                    "UniqueCodes.x", "UniqueSubclass.x" )] 

DataLong_Subs_2006_summary <- DataLong_Subs_2006_summary[DataLong_Subs_2006_summary$Group == DataLong_Subs_2006_summary$CurrentYear,]

test <- DataLong_Subs_2006_summary[,c("Company","Group")]
length(unique(test$Company)) #2390
rm(test)

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

Quartile <- st(DataLong_Subs_2006_summary,  group = "Quartile",  group.test = TRUE,
               summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')
Size_class <- st(DataLong_Subs_2006_summary,  group = "Size_class",  group.test = TRUE,
                 summ=c('mean(x)','median(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'), out = 'return')

write.csv2(Treated, file = "Output_code/Data/Descriptive_statistics_matched_companies_Treated.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
write.csv2(Quartile, file = "Output_code/Data/Descriptive_statistics_matched_companies_Quartile.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of
write.csv2(Size_class, file = "Output_code/Data/Descriptive_statistics_matched_companies_Size_class.csv", row.names = F) #from now on, the addition of _new means that the complexity and relatedness of

##3.3.Analyse correlations-----
#For the matched sample
data_normalized <- DataLong_Subs_2006_summary[,c("Relatedness_Cos2", "HigherUsage.x", "Specializations_Usage.x", "Specializations_Number.x",
                                                 "Date_Incorporation","No_AI_PatentsYearGUOtotal", "NoPatentsYearGUOtotal")] 

names(data_normalized) <- c("Relatedness", "Usage of main code", "Usage of specializations",
                            "N. of specializations","Date of incorporation", "N. of AI patents","N. of patents") 
data_normalized <- scale(data_normalized)
corr_matrix <- cor(na.omit(data_normalized))

jpeg("Output_code/Figures/Correlation_table_matched_companies.jpg", width = 15, height = 12, units = 'in', res = 300)
ggcorrplot(corr_matrix, legend.title = "Correlation", hc.order=F,lab = TRUE, lab_size = 8)+
  theme(axis.text.x=element_text(size=25),axis.text.y=element_text(size=25), legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=24), legend.text = element_text(size=20))
dev.off()

#For the whole dataset 
data_normalized <- FinalDataset_2019[,c("Relatedness_Cos2", "HigherUsage", "Specializations_Usage", "Specializations_Number",
                                   "Date_Incorporation","No_AI_PatentsYearGUOtotal", "NoPatentsYearGUOtotal")] 

names(data_normalized) <- c("Relatedness", "Usage of main code", "Usage of specializations",
                            "N. of specializations","Date of incorporation", "N. of AI patents","N. of patents") 
data_normalized <- scale(data_normalized)
corr_matrix <- cor(na.omit(data_normalized))
jpeg("Output_code/Figures/Correlation_table_all_companies.jpg", width = 15, height = 12, units = 'in', res = 300)
ggcorrplot(corr_matrix, legend.title = "Correlation", hc.order=F,lab = TRUE, lab_size = 8)+
  theme(axis.text.x=element_text(size=25),axis.text.y=element_text(size=25), legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=24), legend.text = element_text(size=20))
dev.off()

#end-----