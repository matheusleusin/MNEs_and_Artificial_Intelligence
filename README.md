# Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs

This repository contains the R code and data to reproduce the analysis and figures for the paper, "Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs."

For a user-friendly overview of the code and the main estimations, please see the supplementary **[HTML summary file](https://relatedness-in-mnes.netlify.app/)** (also available as `index.html` in this repository).

## Overview

This study investigates why firms realize vastly different returns from their investments in Artificial Intelligence. Drawing on Absorptive Capacity (ACAP) theory, the paper explores the firm-level mechanisms that separate successful AI innovators from the rest.

Using a matched-pair, Difference-in-Differences (DiD) design on patent data from over 30,000 Multinational Enterprises (MNEs), the analysis reveals:
* Introducing an AI innovation increases the **knowledge relatedness** of a firm's subsequent patents and boosts its overall innovative output.
* These gains are highly **path-dependent**, with effects being about three times stronger for firms already technologically close to AI.
* Crucially, positive outcomes depend on overcoming a **"conversion challenge"**—significant gains accrue almost exclusively to firms that secure a **granted** AI patent, not just an application.
* Benefits do not extend to firms that merely **acquire** AI patents instead of developing them internally, highlighting the importance of the internal learning process.

This repository provides the necessary resources to replicate these findings.

---

## Repository Structure

The repository is organized as follows:

```
/
├── Input_code/
│   ├── Data_all_MNEs_2019.csv               # Data for Appendix F descriptive statistics.
│   ├── Matched_companies.csv                # The primary matched-pair dataset for DiD analysis.
│   ├── Matched_companies_extra_data.csv     # Supplementary variables for Tables 3, 4, and 5.
│   ├── nace_rev2.csv                        # NACE industry classification codes and descriptions.
│   ├── RCA_All_Years_Simplified.csv         # Pre-calculated specializations in AI-related technologies.
│   └── Big_files_ignore/                    # (Local directory for large raw data files, not tracked by Git).
│
├── Output_code/
│   ├── Data/                                # Contains generated tables and intermediate datasets.
│   └── Figures/                             # Contains all generated plots and figures.
│
├── Effects_AI_adoption.R                    # Main R script for running the entire analysis.
├── Matching_treated_and_untreated.R         # (For transparency) The script used to perform the genetic matching.
├── index.Rmd                                # R Markdown source for the HTML summary file.
└── index.html                               # User-friendly HTML overview of the analysis.
└── README.md                                # This file.
```
---

## Methodological Workflow

The main script, `Effects_AI_adoption.R`, executes the entire empirical analysis. Below is an overview of the workflow and how it corresponds to the paper's hypotheses.

1.  **Estimating Sectoral Distance to AI:** The script first calculates the technological relatedness between all IPC codes and AI. It then uses the nine most related technologies to measure each NACE sector's specialization in core AI fields, creating the sectoral distance metric used to categorize firms into quartiles (Q1, IQR, Q4).

2.  **Measuring Overall Effects (H1, H2):** The script implements a staggered DiD model to estimate the average treatment effect of a firm's first AI innovation on its knowledge relatedness and innovative performance (Table 1). A two-way fixed effects (TWFE) model is also used to test the moderating role of knowledge relatedness.

3.  **Analyzing Path Dependency (H3, H4):** The analysis is then segmented by sectoral proximity quartiles to test how a firm's initial technological position conditions its innovation gains (Table 2).

4.  **The "Conversion Test" (H5, H6):** This key analysis splits the treatment group into firms that only *applied* for an AI patent (Potential ACAP) and those that successfully had one or more *granted* (Realized ACAP). This reveals a stark divergence in outcomes (Figure 3, Tables 3 & 4).

5.  **Antecedents of Success:** A dynamic analysis compares matched successful vs. unsuccessful AI innovators year-by-year to map their divergent paths, identifying a pre-existing "conversion capability" (Table 5).

6.  **Robustness Check (Acquisition Effects):** A final DiD analysis is run on firms that only *acquired* AI patents to confirm that the observed benefits are driven by internal learning, not just ownership (Table 6).

---

## System Requirements & Setup

* **R version:** 4.0.0 or newer.
* **R Packages:** The script requires the following packages. You can install them all by running this command in your R console:

```R
install.packages(c("data.table", "readxl", "tidyverse", "magrittr", "EconGeo", "psych", "Metrics", "did", "openxlsx", "zoo", "vtable", "ggcorrplot", "janitor"))
```

## How to Run the Analysis

1.  **Clone the Repository:**
    ```bash
    git clone [anonymized-repository-url]
    cd MNEs_and_Artificial_Intelligence
    ```

2.  **Prepare the Data:**
    * The core matched panel dataset (`Matched_companies.csv`) is provided in the `Input_code/` directory.
    * **Important:** The raw, large-scale datasets used for the initial matching procedure (e.g., `All_patents.csv`, `DataCompanies1.xlsx`) are not included in this repository due to their size and proprietary nature. The script `Effects_AI_adoption.R` is configured to read these files from a local directory named `Input_code/Big_files_ignore/`. The methodology for constructing the full dataset is detailed in a separate, anonymized repository to ensure the integrity of the peer-review process.

3.  **Execute the Script:**
    * Open `Effects_AI_adoption.R` in R or RStudio.
    * The script is designed to set the working directory automatically if you are using RStudio (`setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`). Otherwise, set the working directory manually to the script's location.
    * Run the script from top to bottom. It will automatically generate all tables and figures in the `Output_code/` directory.

---

## Description of Key Files

### `Input_code/` Directory
This folder contains the pre-processed data required to run the primary analyses.

* **`Matched_companies.csv`**: The final matched-pair panel dataset. Each row is a firm-year observation, containing the main variables for innovative performance, knowledge relatedness, and treatment indicators (`treat`, `first.treat`).
* **`Matched_companies_extra_data.csv`**: A supplementary file with additional firm-level variables (e.g., patent quality metrics, R&D expenses) used for the analyses in Sections 4.3 and 4.4.

### `Output_code/` Directory
This folder stores all outputs generated by the R script.

* **`Data/`**: Contains all empirical results in `.csv` or `.xlsx` format. For example, `Table_2_absolute.xlsx` contains the raw numerical estimates used to create Table 2 in the manuscript, allowing for direct verification of the reported coefficients and standard errors.
* **`Figures/`**: Contains all plots and figures generated by the script, such as `Fig3_Table3.jpg` and the dynamic treatment effect plots for the appendix.
