# NHANES Depression Analysis

## Project Overview

This project examines factors associated with depression among 6,337 U.S. adults using data from the 2021–2023 National Health and Nutrition Examination Survey (NHANES).

The study evaluates demographic, socioeconomic, and health-related factors associated with depression using statistical and predictive modeling techniques in R.

## Objectives

- Identify factors associated with depression among U.S. adults.
- Examine relationships between demographic, socioeconomic, and health-related characteristics and depression.
- Use multivariable logistic regression to evaluate factors independently associated with depression.
- Examine age-related depression patterns using kernel smoothing.
- Evaluate predictor importance using Random Forest and Leave-One-Covariate-Out (LOCO) analysis.

## Dataset

**Source:** National Health and Nutrition Examination Survey (NHANES), 2021–2023.

**Final analytical sample:** 6,337 U.S. adults.

Three NHANES components were used:
- Depression Screener Questionnaire (DPQ_L)
- Demographic Variables and Sample Weights (DEMO_L)
- Body Measurements (BMX_L)

Depression was assessed using the Patient Health Questionnaire-9 (PHQ-9).

## Methods

- Data cleaning and preprocessing
- Descriptive statistics
- Bivariate analysis
- Multivariable logistic regression
- Kernel smoothing
- Random Forest
- Leave-One-Covariate-Out (LOCO) analysis

## Key Findings

- Higher BMI was significantly associated with greater odds of depression. Each one-unit increase in BMI was associated with approximately 4% higher odds of depression (OR = 1.04, 95% CI: 1.02–1.06, p < 0.001).
- Higher income-to-poverty ratio was significantly associated with lower odds of depression (OR = 0.807, 95% CI: 0.698–0.934, p = 0.003).
- Divorced participants had significantly higher odds of depression compared with the reference marital-status group (OR = 1.68, 95% CI: 1.09–2.59, p = 0.018).
- Random Forest analysis identified age, BMI, income, and marital status as important predictors of depression.
- LOCO analysis showed that education had the largest impact on predictive accuracy when removed, with BMI, age, and income-to-poverty ratio also demonstrating predictive importance.

## Tools & Technologies
- RStudio
- NHANES
- Statistical Analysis
- Predictive Modeling
- Data Visualization

## Repository Contents

- `NHANES-Depression Analysis.R` – R code used for data preparation and statistical analysis.
- `NHANES-Depression Analysis Report.pdf` – Full project report including methodology, results, visualizations, discussion, and conclusions.

## Author

**Mounisha Pedapudi, MPH**  
Master of Public Health – Epidemiology & Biostatistics  
The University of Southern Mississippi
