
#10. Effects considering non-granted patents----
##10.1. For all sectors -----
##10.2. Measuring effects across sectors -----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
RCA_All_Years <-read.csv("Input_code/RCA_All_Years_Simplified.csv", sep = ";", header = TRUE, dec=",") 

mat_tech_rel <-read.csv2("Output_code/Data/Matrix_relatedness_technologies_to_AI.csv")#file we created before measuring the relatedness of AI to other technologies
mat_tech_rel <- mat_tech_rel[,c("X", "Artificial_Intelligence")] #select the AI and the other technologies columns
mat_tech_rel <- mat_tech_rel[mat_tech_rel$Artificial_Intelligence >0,] #from 646 to 515 with relatedness above 0
#let's pick the 5 codes with higher relatedness to AI:
AI_codes <- mat_tech_rel[mat_tech_rel$Artificial_Intelligence >.06,1] #5 codes

DataLong_Subs_sim <-read.csv("Input_code/Matched_companies.csv", sep = ";", header = TRUE, dec=",")
dynamic_capabilities <- read.csv("Input_code/Big_files_ignore/df_dynamic_capabilities.csv", header = TRUE)
names(dynamic_capabilities)[names(dynamic_capabilities) == 'Year'] <- 'CurrentYear'
DataLong_Subs_sim <- left_join(DataLong_Subs_sim, dynamic_capabilities, by=c("Company", "CurrentYear"))

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
CategoriesNace <-read.csv("Input_code/Big_files_ignore/Additional_data_patents/new_distance_testing_ready3.csv")#, sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance
#CategoriesNace <-read.csv("Input_code/Big_files_ignore/Additional_data_patents/new_distance_testing_ready_2ndAlternative.csv")#, sep = ";", header = TRUE, dec=",")#[,c(-1)] #doesn't work, no significance

DataLong_Subs_sim$Nace_4d <- as.character(DataLong_Subs_sim$Nace_4d)
CategoriesNace$Nace_4d <- as.character(CategoriesNace$Nace_4d)
#CategoriesNace <- CategoriesNace[,c(1,8)] #for top6 alternative file
#CategoriesNace <- CategoriesNace[,c(1,12)] #for top6
#CategoriesNace <- CategoriesNace[,c(1,11)] #for top8
CategoriesNace <- CategoriesNace[,c(1,10)] #for top9, which is the final chosen one
names(CategoriesNace) <- c("Nace_4d", "Quartile")
#table(CategoriesNace$Quartile_top5)

DataLong_Subs_sim <- left_join(DataLong_Subs_sim,CategoriesNace, by = "Nace_4d")

table(is.na(DataLong_Subs_sim$Quartile)) #33096        F  2100      T
table(DataLong_Subs_sim$Quartile) #bottom 9380              medium  11004            top 12712      

DataLong_Subs_sim$Quartile[is.na(DataLong_Subs_sim$Quartile)] <- "bottom"
table(is.na(DataLong_Subs_sim$Quartile)) #35196 F
table(DataLong_Subs_sim$Quartile) #bottom 11480             medium  11004            top 12712     

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #bottom 3024    medium   6748       top 7826  
table(nontreat_test$Quartile) #bottom 3024    medium   6748       top 7826  
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #bottom 216  medium  482                      top 559     
rm(treat_test, nontreat_test)

###10.2.1.Define new treatment-----
rm(CategoriesNace, dynamic_capabilities, mat_tech_rel, RCA_All_Years_treat_AI, RCA_All_Years)
#First, include information about granted and not granted patents
Patent_data <- read.csv("Input_code/Big_files_ignore/Additional_data_patents/Resulting_data_patent_info.csv", header = TRUE) #
DataLong_Subs_sim<- left_join(DataLong_Subs_sim,Patent_data, by=c("Company", "CurrentYear"))
DataLong_Subs_sim %<>% group_by(Company) %>% mutate(total_granted_ai = max(n_granted_yes_ai, na.rm =T )) %>% ungroup()

test <- DataLong_Subs_sim[,c("Company", "CurrentYear", "NoPatentsYearGUOtotal","n_granted_yes", "n_granted_na")]

Current_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
Current_non_treated <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]

#so the treatment will now be companies that never had an AI patent granted
Current_treated_new_treat <-Current_treated[Current_treated$total_granted_ai ==0,]
backup_DataLong_Subs_sim <- DataLong_Subs_sim

####10.2.1.1.Treatment non-granted-----
DataLong_Subs_sim <- rbind(Current_treated_new_treat, Current_non_treated)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Bottom"& is.na(DataLong_Subs_sim$Quartile) == F,]

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
              "No_AI_PatentsYearGUOtotal",#effect on number of AI patents
              "Dynamic_capabilities") #the share of codes that changed from one year to the other 

####10.2.1.1.0.Calculate the effects Non-standardized -----
######10.2.1.1.1.Relatedness----
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

#jpeg("Output_code/Figures/Fig_appendix_h3_Relatedness_1Q.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H3) Q1 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
##dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h4_Relatedness_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H4) IQR - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h5_Relatedness_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H5) Q4 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.2.Innovative performance----
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

#jpeg("Output_code/Figures/Fig_appendix_h6_Number_patents_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H6) Q1 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h7_Number_patents_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H7) IQR - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h8_Number_patents_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H8) Q4 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.2.00.NEW DYNAMIC CAPABILITIES ----
#Group 1
variable_n = 12
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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H6) Q1 - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H7) IQR - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H8) Q4 - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.3.Usage of specializations----
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

#######10.2.1.1.4.Most used technology----
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

#######10.2.1.1.5.Number of specializations----
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

#######10.2.1.1.6.Number of sections ----
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
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(UniqueCodes = replace_na(UniqueCodes, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(UniqueCodes = replace_na(UniqueCodes, 0))
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

#######10.2.1.1.7.Number of subclasses----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(UniqueSubclass = replace_na(UniqueSubclass, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(UniqueSubclass = replace_na(UniqueSubclass, 0))
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

#######10.2.1.1.8.Herfindahl index----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(Herfindahl = replace_na(Herfindahl, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(Herfindahl = replace_na(Herfindahl, 0))
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

#######10.2.1.1.9.Shannon index----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(Shannon = replace_na(Shannon, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(Shannon = replace_na(Shannon, 0))
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

#######10.2.1.1.10.Number AI codes----
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

#######10.2.1.1.11. Number of AI patents----
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

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/Results_new_distance_non_stand_non_granted_alternative_top8.xlsx", rowNames = F) 
rm(results_df)

####10.2.1.2.0.Calculate the effects Standardized -----
#standardize per group
DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

######10.2.1.2.1.Relatedness
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

#######10.2.1.2.2.Innovative performance
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

#######10.2.1.2.2.00. NEW DYNAMIC CAPABILITIES
#Group 1
variable_n = 12
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

#######10.2.1.2.3.Usage of specializations
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

#######10.2.1.2.4.Most used technology
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

#######10.2.1.2.5.Number of specializations
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

#######10.2.1.2.6.Number of sections 
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

#######10.2.1.2.7.Number of subclasses
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

#######10.2.1.2.8.Herfindahl index
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

#######10.2.1.2.9.Shannon index
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

#######10.2.1.2.10.Number AI codes
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

#######10.2.1.2.11. Number of AI patents
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

#write.xlsx(results_df, file = "Output_code/Data/Results_Effects_stand.xlsx", rowNames = F)
write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/Results_new_distance_stand_non_granted_alternative_top8.xlsx", rowNames = F)

####10.2.1.1.Treatment granted-----
Current_treated_new_treat_granted <-Current_treated[Current_treated$total_granted_ai > 0,]
length(unique(Current_treated_new_treat_granted$Company)) #480
DataLong_Subs_sim <- rbind(Current_treated_new_treat_granted, Current_non_treated)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Bottom"& is.na(DataLong_Subs_sim$Quartile) == F,]

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
              "No_AI_PatentsYearGUOtotal",#effect on number of AI patents
              "Dynamic_capabilities") #the share of codes that changed from one year to the other 

####10.2.1.1.0.Calculate the effects Non-standardized -----
######10.2.1.1.1.Relatedness----
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

#jpeg("Output_code/Figures/Fig_appendix_h3_Relatedness_1Q.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H3) Q1 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
##dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h4_Relatedness_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H4) IQR - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h5_Relatedness_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H5) Q4 - Relatedness") + 
  theme(legend.position="right") +
  ylab("Estimated effect on relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.2.Innovative performance----
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

#jpeg("Output_code/Figures/Fig_appendix_h6_Number_patents_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H6) Q1 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h7_Number_patents_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H7) IQR - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Output_code/Figures/Fig_appendix_h8_Number_patents_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H8) Q4 - Innovative performance") + 
  theme(legend.position="right") +
  ylab("Estimated effect on number of patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.2.00.NEW DYNAMIC CAPABILITIES ----
#Group 1
variable_n = 12
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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_Q1.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H6) Q1 - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","steelblue3")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_IQR.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H7) IQR - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","green4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

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

#jpeg("Input_code/Big_files_ignore/NEW_FIG/Fig_dyn_cap_Q4.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "H8) Q4 - Share of dynamic capabilities") + 
  theme(legend.position="right") +
  ylab("Estimated effect on the share of dynamic capabilities") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","red4")) + 
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
#dev.off()

#######10.2.1.1.3.Usage of specializations----
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

#######10.2.1.1.4.Most used technology----
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

#######10.2.1.1.5.Number of specializations----
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

#######10.2.1.1.6.Number of sections ----
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
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(UniqueCodes = replace_na(UniqueCodes, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(UniqueCodes = replace_na(UniqueCodes, 0))
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

#######10.2.1.1.7.Number of subclasses----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(UniqueSubclass = replace_na(UniqueSubclass, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(UniqueSubclass = replace_na(UniqueSubclass, 0))
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

#######10.2.1.1.8.Herfindahl index----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(Herfindahl = replace_na(Herfindahl, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(Herfindahl = replace_na(Herfindahl, 0))
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

#######10.2.1.1.9.Shannon index----
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
variable_name
DataLong_Subs_sim_group2 <- DataLong_Subs_sim_group2 %>%
  mutate(Shannon = replace_na(Shannon, 0))

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
DataLong_Subs_sim_group3 <- DataLong_Subs_sim_group3 %>%
  mutate(Shannon = replace_na(Shannon, 0))
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

#######10.2.1.1.10.Number AI codes----
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

#######10.2.1.1.11. Number of AI patents----
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

#write.xlsx(results_df, file = "Output_code/Data/Results_Effects_non_stand.xlsx", rowNames = F) #Results_Effects_non_stand.xlsx with dyn capabilities now
write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/Results_new_distance_non_stand_granted_alternative_top8.xlsx", rowNames = F) 
rm(results_df)

####10.2.1.2.Calculate the effects Standardized -----
#standardize per group
DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                            "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")] <- 
  scale(DataLong_Subs_sim_group3[,c("Relatedness_Cos2", "Specializations_Usage", "HigherUsage","Specializations_Number",
                                    "Herfindahl", "Shannon", "NoPatentsYearGUOtotal", "UniqueCodes", "UniqueSubclass", "AIcodes", "No_AI_PatentsYearGUOtotal", "Dynamic_capabilities")])

######10.2.1.2.1.Relatedness
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

#######10.2.1.2.2.Innovative performance
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

#######10.2.1.2.2.00. NEW DYNAMIC CAPABILITIES
#Group 1
variable_n = 12
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

#######10.2.1.2.3.Usage of specializations
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

#######10.2.1.2.4.Most used technology
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

#######10.2.1.2.5.Number of specializations
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

#######10.2.1.2.6.Number of sections 
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

#######10.2.1.2.7.Number of subclasses
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

#######10.2.1.2.8.Herfindahl index
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

#######10.2.1.2.9.Shannon index
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

#######10.2.1.2.10.Number AI codes
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

#######10.2.1.2.11. Number of AI patents
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

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/Results_new_distance_stand_granted_alternative_top8.xlsx", rowNames = F)

#11.Analysing main critical variables plust HQ and Subsidiaries for whole dataset------
##11.1. Measuring effects for all sectors together for ratio 5-----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Input_code/Big_files_ignore/NEW_FIG/Matched_companies_Acquis_match_ratio5.csv", sep = ";", header = TRUE, dec=",") #

table(is.na(DataLong_Subs_sim$Quartile)) #672    F
table(DataLong_Subs_sim$Quartile) # IQR 420     top 252         

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
variables = c("Relatedness_Cos2", #relatedness
              "NoPatentsYearGUOtotal") #

###11.2.1.Calculate the effects Non-standardized -----
DataLong_Subs_sim_group0 <- DataLong_Subs_sim

#####11.2.1.1. All variables group0----
######11.2.1.1.1.All treated together------
variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

results_df <- results_df %>%
  # Make Group a factor with the desired order
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  # Arrange by Variable (alphabetically) and then by Group factor
  arrange(Variable, Group)

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/data_acquisitions_ratio_5_non_stand.xlsx", rowNames = F) 

######Standard-----
DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

results_df <- results_df %>%
  # Make Group a factor with the desired order
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  # Arrange by Variable (alphabetically) and then by Group factor
  arrange(Variable, Group)

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/data_acquisitions_ratio_5_stand.xlsx", rowNames = F) 

##11.1. Measuring effects for all sectors together for ratio 50-----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Input_code/Big_files_ignore/NEW_FIG/Matched_companies_Acquis_match_ratio50.csv", sep = ";", header = TRUE, dec=",") #

table(is.na(DataLong_Subs_sim$Quartile)) #672    F
table(DataLong_Subs_sim$Quartile) # IQR 420     top 252         

treat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
nontreat_test <-DataLong_Subs_sim[DataLong_Subs_sim$treat == 0,]
table(treat_test$Quartile) #IQR 70      top 42   
table(nontreat_test$Quartile) #iqr 350      top 210   
treat_test <- distinct_at(treat_test, vars(id,subclass), .keep_all = T)
nontreat_test <- distinct_at(nontreat_test, vars(id,subclass), .keep_all = T)
table(treat_test$Quartile) #iqr 5  top 3      
rm(treat_test, nontreat_test)

DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "IQR" & is.na(DataLong_Subs_sim$Quartile) == F,]
DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "Top"& is.na(DataLong_Subs_sim$Quartile) == F,]

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
variables = c("Relatedness_Cos2", #relatedness
              "NoPatentsYearGUOtotal") #

###11.2.1.Calculate the effects Non-standardized -----
DataLong_Subs_sim_group0 <- DataLong_Subs_sim

#####11.2.1.1. All variables group0----
######11.2.1.1.1.All treated together------
variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

results_df <- results_df %>%
  # Make Group a factor with the desired order
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  # Arrange by Variable (alphabetically) and then by Group factor
  arrange(Variable, Group)

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/data_acquisitions_ratio_50_non_stand.xlsx", rowNames = F) 

######Standard-----
DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group0[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group2[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")] <- 
  scale(DataLong_Subs_sim_group1[,c("Relatedness_Cos2", "NoPatentsYearGUOtotal")])

variable_n = 1
n_group = 4
group = print(groups[n_group], quote=FALSE)
variable_name <- variables[variable_n]
#estimate:
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
#pick the estimate values:
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
#create the dataset:
results_df <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group0 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group2 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
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
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

variable_n = 2
variable_name <- variables[variable_n]
estim_attgt <- att_gt(tname = tname, idname = idname, gname = gname, 
                      yname = variables[variable_n], data = DataLong_Subs_sim_group1 )
agg.simple <- aggte(estim_attgt, type = "simple", na.rm = TRUE)
att <- agg.simple$overall.att          # Average Treatment Effect on the Treated (ATT)
se <- agg.simple$overall.se            # Standard Error
results_new <- calculate_ci_and_significance(att, se, group, paste0(variable_name, "_all_treat"))
results_df <- rbind(results_df, results_new)

results_df <- results_df %>%
  # Make Group a factor with the desired order
  mutate(Group = factor(Group, levels = c("DataLong_Subs_sim_group0", "DataLong_Subs_sim_group1", "DataLong_Subs_sim_group2"))) %>%
  # Arrange by Variable (alphabetically) and then by Group factor
  arrange(Variable, Group)

write.xlsx(results_df, file = "Input_code/Big_files_ignore/NEW_FIG/data_acquisitions_ratio_50_stand.xlsx", rowNames = F) 

#END----
