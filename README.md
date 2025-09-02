# Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs

This repository contains the R code and data to reproduce the analysis and figures for the paper entitled "Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs". 
To facilitate the overall understanding of the code, a visualization of the main estimations and descriptvies presented in the paper is available in an [html file](https://relatedness-in-mnes.netlify.app/) (named *index.html* in the repository).
## Overview

This research investigates why firms show significant differences in the returns they get from investing in Artificial Intelligence. The paper draws on Absorptive Capacity (ACAP) theory to explore the firm-level mechanisms that separate successful AI innovators from the rest.

Using a matched-pair, Difference-in-Differences (DiD) design on patent data from over 30,000 Multinational Enterprises (MNEs), the analysis reveals:
* Introducing an AI innovation increases the **knowledge relatedness** of a firm's subsequent patents and boosts its overall innovative output.
* These gains are highly **path-dependent**, with effects being about three times stronger for firms already technologically close to AI.
* Crucially, positive outcomes depend on overcoming a **"conversion challenge"**—significant gains are almost exclusively seen in firms that secure a **granted** AI patent, not just an application.
* Benefits do not extend to firms that merely **acquire** AI patents instead of developing them internally, highlighting the importance of the internal learning process.

This repository provides the necessary resources to replicate these findings.

---

## Repository Structure

The repository is organized as follows:

```
/
├── Input_code/
│   ├── Data_all_MNEs_2019.csv # This file contains the data used to create the descriptive statistics presented in the paper (Appendix F). This includes data on more than 26,000 MNEs with available data for the variables considered in the year 2019.
│   ├── Matched_companies.csv # This is the main group of matched companies (i.e., MNEs that introduced an AI innovation vs MNEs that didn't introduce it, but are from have a similar patent history and the exact same age, industry, and size).
│   ├── Matched_companies_extra_data.csv # This data has additional variables used to estimate effects from Tables 3, 4, and 5.
│   ├── nace_rev2.csv #NACE information
│   ├── RCA_All_Years_Simplified.csv #Calculated specializations in AI-related codes
│   ├── Table 3.xlsx #Table used to generate Figure 3
│   └── Big_files_ignore/ (Directory for large source files not tracked by Git)
│
├── Output_code/
│   ├── Data/ (Contains generated tables and intermediate datasets)
│   └── Figures/ (Contains all generated plots and figures)
│
└── Effects_AI_adoption.R     # The main R script for running the entire analysis
└── Matching_treated_and_untreated.R
└── index.Rmd
└── index.html
└── README.md                   # This file
```
---

## Methodological Workflow

The main script, `Effects_AI_adoption.R`, executes the entire empirical analysis in a sequence of steps. Below is an overview of the workflow and how it corresponds to the paper's findings.

1.  **Estimating Sectoral Distance to AI:** The script first calculates the technological relatedness between all technology fields (IPC codes) and AI to establish a baseline proximity matrix. It then uses this matrix to measure how specialized each economic sector (NACE code) is in core AI-related technologies, creating the sectoral distance metric used to categorize firms into quartiles (Q1, IQR, Q4).

2.  **Measuring Overall Effects (H1, H2):** The script implements a staggered Difference-in-Differences (DiD) model to estimate the average treatment effect of introducing a first AI innovation on a firm's knowledge relatedness and innovative performance (Table 1). It also includes a two-way fixed effects (TWFE) model to test the moderating role of knowledge relatedness.

3.  **Analyzing Path Dependency (H3, H4):** The analysis is then segmented by sectoral proximity quartiles (Q1, IQR, Q4) to test how a firm's initial technological position conditions the innovation gains (Table 2).

4.  **The "Conversion Test" (H5, H6):** This is a key part of the analysis where the treatment group is split between firms that only applied for an AI patent (Potential ACAP) and those that successfully had one or more granted (Realized ACAP).This analysis reveals the stark divergence in outcomes (Figure 3, Tables 3 & 4).

5.  **Antecedents of Success:** A dynamic matched-pair analysis compares successful vs. unsuccessful AI innovators year-by-year to map the causal timeline of their divergence, identifying a pre-existing "conversion capability" (Table 5).

6.  **Robustness Check (Acquisition Effects):** The script runs a final DiD analysis on a group of firms that only acquired AI patents (without internal development) to confirm that the observed benefits are driven by internal learning, not just ownership (Table 6).

---

## System Requirements & Setup

* **R version:** 4.0.0 or newer.
* **R Packages:** The script requires the following packages. You can install them all by running the command below in your R console.

```R
install.packages(c("data.table", "readxl", "tidyverse", "magrittr", "EconGeo", "psych", "Metrics", "did", "openxlsx", "zoo", "vtable", "ggcorrplot", "janitor"))
```

## How to Run the Analysis

1.  **Clone the Repository:**
    ```bash
    git clone [https://github.com/matheusleusin/MNEs_and_Artificial_Intelligence.git](https://github.com/matheusleusin/MNEs_and_Artificial_Intelligence.git)
    cd MNEs_and_Artificial_Intelligence
    ```

2.  **Prepare the Data:**
    * The core matched panel dataset (`Matched_companies.csv`) is provided in the `Input_code/` directory.
    * **Important:** The raw, large-scale datasets used for the initial matching procedure (e.g., `All_patents.csv`, `DataCompanies1.xlsx`, etc.) are not included in this repository due to their size. The script `Effects_AI_adoption.R` references these files from a local directory named `Input_code/Big_files_ignore/`. The methodology for constructing the full dataset is detailed in a separate repository: [Method-to-create-a-dataset-of-MNEs-ownership-structures](https://github.com/matheusleusin/Method-to-create-a-dataset-of-MNEs-ownership-structures).

3.  **Execute the Script:**
    * Open the `Effects_AI_adoption.R` script in R or RStudio.
    * Set the working directory to the script's location using `setwd()`. The script is designed to do this automatically if you are using RStudio (`setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`).
    * Run the script from top to bottom. The script will automatically generate all tables and figures in the `Output_code/` directory.

---

## Description of Key Files

### `Input_code/` Directory
This folder contains the pre-processed data required to run the main DiD analyses.

* **`Matched_companies.csv`**: The final, matched panel dataset created by the genetic matching algorithm. Each row represents a firm-year observation. It contains the primary variables for innovative performance, knowledge relatedness, and the treatment indicators (`treat`, `first.treat`).
* **`Matched_companies_extra_data.csv`**: A supplementary file containing additional firm-level variables (e.g., patent quality metrics, R&D expenses) that are merged into the main panel for the analyses in Sections 4.3 and 4.4.

### `Output_code/` Directory
This folder is where all outputs from the R script are saved. It is divided into two subdirectories.

* **`Data/`**: Contains all empirical results in `.csv` or `.xlsx` format. For example, `Table_2_absolute.xlsx` contains the raw numerical estimates that were used to create Table 2 in the manuscript. This allows for direct verification of the reported coefficients and standard errors.
* **`Figures/`**: Contains all figures and plots generated by the script, such as `Fig3_Table3.jpg` and the dynamic treatment effect plots for the appendix (e.g., `Fig_appendix_g1_Relatedness_all.jpg`).

---
