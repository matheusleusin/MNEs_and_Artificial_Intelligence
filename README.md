# Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs

This repository contains the R code and data to reproduce the analysis and figures for the paper "Performance Pending: The AI Conversion Challenge and the Power of Knowledge Relatedness in MNEs".

For a user-friendly overview of the code and the main estimations, please see the supplementary **[HTML summary file](https://relatedness-in-mnes.netlify.app/)** (also available as `index.html` in this repository).

## Overview

The referenced paper investigates why firms realize vastly different returns from their investments in Artificial Intelligence. Drawing on Absorptive Capacity (ACAP) theory, the paper explores the firm-level mechanisms that separate successful AI innovators from the rest.

Using a matched-pair, Difference-in-Differences (DiD) design on patent data from over 30,000 Multinational Enterprises (MNEs), the analysis reveals:
* Introducing an AI innovation increases the **knowledge relatedness** of a firm's subsequent patents and boosts its overall innovative output.
* These gains are highly **path-dependent**, with effects being about three times stronger for firms already technologically close to AI.
* Crucially, positive outcomes depend on overcoming a **"conversion challenge"** — significant gains accrue almost exclusively to firms that secure a **granted** AI patent, not just an application.
* Benefits do not extend to firms that merely **acquire** AI patents instead of developing them internally, highlighting the importance of the internal learning process.

This repository provides the necessary resources to replicate these findings.

---

## Repository Structure

The repository is organized as follows:

```
/
├── Input_code/
│   ├── Data_acquistions.csv                 # Supplementary variables for Table 6.
│   ├── Data_all_MNEs_2019.csv               # Data for Appendix F descriptive statistics.
│   ├── Matched_companies.csv                # The primary matched-pair dataset for DiD analysis.
│   ├── Matched_companies_extra_data.csv     # Supplementary variables for Tables 3, 4, and 5.
│   ├── nace_rev2.csv                        # NACE industry classification codes and descriptions.
│   ├── RCA_All_Years_Simplified.csv         # Pre-calculated specializations in AI-related technologies.
│   └── Big_files_ignore/                    # (Local directory for large raw data files, not tracked by Git).
│
├── Output_code/
│   ├── Data/                                # Contains generated tables and intermediate datasets.
│       ├── Code_Matching/                   # Contains the matched pairs used in the additional analyses of patent acquisition and dynamic temporal dynamics of the conversion challenge
│   └── Figures/                             # Contains all generated plots and figures.
│
├── Effects_AI_adoption.R                    # Main R script for running the entire analysis.
├── Matching_treated_and_untreated.R         # (For transparency) The script used to perform the genetic matching.
├── index.Rmd                                # R Markdown source for the HTML summary file.
└── index.html                               # User-friendly HTML overview of the analysis.
└── README.md                                # This file.
```
---

### Methodological Workflow

The main script, `Effects_AI_adoption.R`, executes the entire empirical analysis presented in the paper. The workflow is structured in four main stages, as visualized in the text-based flowchart below:

```
├── [1] Overall Impact on Innovation
│   ├── Focus: Core effects (H1, H2)
│   ├── Method: Staggered DiD
│   └── Key Variables: Knowledge Relatedness, Innovative Performance
│
├── [2] Transformation of Innovation Dynamics
│   ├── Focus: Path dependency & sectoral proximity (H3, H4)
│   ├── Method: Grouped DiD (by Q1, IQR, Q4)
│   └── Key Variables: New Specializations, Technological Diversification, specializations in AI-related techologies
│
├── [3] The "Conversion Test"
│   ├── Focus: Realized ACAP, innovation quality & efficiency (H5, H6)
│   ├── Method: Grouped DiD (by patent grant status)
│   └── Key Variables: Patent success rate, patent quality, patenting efficiency, R&D Investments
│
└── [4] Probing the ACAP Mechanisms
    ├── Focus: Antecedents of the conversion challenge & role of internal learning
    ├── Method: Dynamic Comparison (F-tests & Wilcoxon) on a new "success" vs. "unsuccessful" matched sample; DiD on Acquisition-Only sample
    └── Key Variables: Pre-existing capabilities (at t-1); Post-treatment outcomes
```

The R script is organized to follow this structure:

* **Initial Setup (Sectoral Distance):** Before the main analyses, **Section 1** of the script calculates the technological relatedness of sectors to AI. This initial step creates the sectoral distance metric (Q1, IQR, Q4) that is used as a key grouping variable in the subsequent analyses.

* **1. Overall Impact on Innovation (H1, H2):** Corresponding to **Box [1]** in the text-based flowchart above, the script implements a staggered DiD model to estimate the average treatment effect of a firm's first AI innovation on its knowledge relatedness and innovative performance (Table 1).

* **2. Transformation of Innovation Dynamics (H3, H4):** Following **Box [2]** in the text-based flowchart above, the analysis is segmented by sectoral proximity quartiles to test how a firm's initial technological position conditions its innovation gains (Table 2).

* **3. The "Conversion Test" (H5, H6):** This analysis, outlined in **Box [3]**, splits the treatment group by patent grant status to reveal the stark divergence in outcomes between firms with Potential vs. Realized ACAP (Figure 3, Tables 3 & 4).

* **4. Probing ACAP Mechanisms:** Finally, corresponding to **Box [4]**, the script investigates the mechanisms behind the conversion challenge. It first runs a dynamic comparison to identify the pre-existing capabilities of successful innovators (Table 5) and then performs a robustness check on acquisition-only firms to isolate the effect of internal learning (Table 6).

---

## System Requirements & Setup

* **R version:** 4.0.0 or newer.
* **R Packages:** The script requires the following packages. You can install them all by running this command in your R console:

```R
install.packages(c("data.table", "readxl", "tidyverse", "magrittr", "EconGeo", "psych", "Metrics", "did", "openxlsx", "zoo", "vtable", "ggcorrplot", "janitor"))
```

## Reproducibility and Instructions

#### Important Note on Reproducibility

Not all scripts in this repository are fully reproducible with the publically available data.
* **Non-Reproducible Steps:** The initial data processing steps — specifically the `Matching_treated_and_untreated.R` script and **Section 1 (`#1.Estimating technological distance...`)** of the `Effects_AI_adoption.R` script — are not directly executable. They rely on raw source datasets (e.g., `All_patents.csv`, `DataCompanies1.xlsx`) which are not included here due to their **large size** and **proprietary nature**, as the data is licensed from Orbis and is protected by intellectual property rights. These scripts are provided for methodological transparency only.
* **Reproducible Analysis:** The core empirical analysis, which begins at **Section 2 (`#2.Measuring effects-----`)** of the `Effects_AI_adoption.R` script, **is fully reproducible** using the provided `Matched_companies.csv` and other files from the `Input_code/` directory.

#### How to Run the Analysis

1.  **Clone the Repository:**
    ```bash
    git clone [anonymized-repository-url]
    cd MNEs_and_Artificial_Intelligence
    ```

2.  **Open the Script:**
    * Open `Effects_AI_adoption.R` in R or RStudio.

3.  **Execute the Reproducible Sections:**
    * The script is designed to set the working directory automatically if you are using RStudio (`setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`). Otherwise, set the working directory manually to the script's location.
    * Run the script starting from **Section 2 (`#2.Measuring effects-----`)** to the end. This will automatically generate all tables and figures in the `Output_code/` directory based on the provided matched data.
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
