# Estimating the effects of introducing an AI innovation for MNEs

This repository contains files that can be used to reproduce the analysis presented in the paper entitled **"Relate to Innovate: The Impact of Artificial Intelligence on Multinational Enterprises’ Innovative Activities"**. It includes an R code that can be used to produce the analysis (named *"Effects_AI_adoption"*), one Rmarkdown file (named *"Effects_AI_adoption_markdown"*), which is used to generate an [html file](https://matheusleusin.github.io/MNEs_and_Artificial_Intelligence/Effects_AI_adoption_markdown.html) (*Effects_AI_adoption_markdown*) that allows visualizing the reproduction of the main parts of the code, and twomain folders, named "Input_code" and *"Output_code"*.

The folder named **"Input_code"** is used as input to reproduce all of the estimations presented in the paper. The files contained in this folder look like this:

![image](https://github.com/user-attachments/assets/2f2e0fad-b9dc-4b45-ab11-7b680308e5f9)

In more detail, the content of each of these three files is:

  -Data_all_years_all_MNEs.csv → This file contains panel data that is used to create the descriptive statistics presented in the paper. This includes data on more than 26,000 MNEs for the variables considered.
  
  -Matched_companies.csv → This file contains panel data on the more than 1,000 MNEs that were matched to counterparts from with same or similar "Average number of patents owned in the 5 years before treatment", "Number of patents in the year of treatment",   "Firm age", Industry (NACE 4-digit level), and Size. The matching allows that untreated companies are matched more than 1 time to treatment companies, if they are the most similar ones to these from the available companies.
  
  -RCA_All_Years_Simplified.csv → This file contains the binary specializations of all matched companies in all of the possible 4-digit IPC codes. It is used to calculate variables linked to number of specializations.

Besides these files, a subfolder named *"Big_files_ignore"* is also contained in this folder, but set to be ignored. This hidden folder contains data about all patents and companies linked to the MNE dataset. The folder is hidden due to Orbis intellectual property rights. These files are used in the first part of the code, entitled *"Measuring technological distance"*. If you want to reproduce the code but don't have access to these files, please jump this first part in the code and start from the section *"Measuring effects"*

All files created through executing the code are saved in **Output_code**. This folder has two subfolders, one with Figures and one with Data. 

The *"Figures"* folder should look like this after the reproduction of the code:

![image](https://github.com/user-attachments/assets/31561c8e-ab31-4ae4-beff-047670b88623)


The *"Data"* folder should look like this after the reproduction of the code: 

![image](https://github.com/user-attachments/assets/f363b4e5-2d4b-443e-aed9-81b5c3c662d1)

Particularly, the 5 files starting with "Descriptive_statistics" in the name are created in the third section of the code (*Descriptive statistics and correlations*). The remaining 4 files are:

  -Distance_NACE_sectors_to_AI.csv → This file is generated in the subsection *"1.2.Technological distance of NACE sectors to AI"* of the code. It contains the estimated distance of every NACE sector (with more than 500 patents) to AI.
  
  -Matrix_relatedness_technologies_to_AI.csv → This file is generated in the subsection *"1.1.Technological distance of 4-digit IPC codes to AI"* of the code. It contains the estimated distance of every 4-digit IPC technology to AI.
  
  -Results_Effects_non_stand.xlsx → This file is generated in the subsection *"2.2.2.Calculate the effects Standardized"* of the code. It estimates the **standardized** effects considering the three considered groups for all variables.
  
  -Results_Effects_stand.xlsx → This file is generated in the subsection *"2.2.1.Calculate the effects Non-standardized"* of the code. It estimates the **non-standardized** effects considering the three considered groups for all variables.


