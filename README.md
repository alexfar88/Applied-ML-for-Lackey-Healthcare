# Applied-ML-for-Lackey-Healthcare
Predicting patient churn for a free healthcare clinic using R, balancing model interpretability (logistic regression) and prediction performance (KNN) to support targeted interventions.

# Patient Churn Prediction for a Healthcare Clinic

## Overview
This project applies statistical modeling and machine learning techniques to predict patient
churn for Lackey Clinic, a free and low-cost healthcare provider in Yorktown, Virginia.
Patient retention is critical to the clinic’s mission of delivering preventative care and
reducing avoidable hospitalizations, while onboarding new patients is costly due to
healthcare regulatory constraints.

The goal of this project was to identify patients most likely to churn and provide
actionable insights that could inform targeted outreach and operational decisions.

---

## Problem Definition
Patient churn was defined as occurring if **either**:
- A patient has not had a clinical encounter in the past year, or
- The patient’s enrollment status is marked as *disenrolled*

This definition aligns with preventative care guidelines and the clinic’s operational goals.

---

## Data & Feature Engineering
De-identified patient-level data included demographics, diagnoses, enrollment status, and
visit history. Multiple raw datasets were cleaned and joined to create a unified analytic
table.

Feature engineering included:
- Aggregated visit and diagnosis indicators
- Housing, employment, and language classifications
- Distance from patient zip code to clinic location using geographic data (`zipcodeR`)

---

## Modeling Approach
Two models were developed with complementary objectives:

### Logistic Regression (Interpretability)
- Used to identify statistically significant drivers of churn
- Enables clear interpretation of effect sizes and directionality
- Cross-validated to assess generalization performance

### K-Nearest Neighbors (Prediction)
- Selected for improved predictive performance
- Tuned via cross-validation
- Achieved:
  - **Accuracy:** 65% (baseline: 56%)
  - **Sensitivity:** 86%

Sensitivity was prioritized to minimize missed churners, reflecting the higher cost of
failing to identify at-risk patients compared to false positives.

---

## Key Insights
Key predictors of churn included:
- **Employment status:** Higher churn among self-employed and unemployed patients
- **Chronic disease:** Lower churn among patients with chronic conditions
- **Housing stability:** Elevated churn among patients experiencing housing instability
- **Language:** Lower churn among non-native English speakers
- **Age:** High churn among patients aged 65+, likely due to Medicaid eligibility

These findings highlight both structural and behavioral factors influencing retention.

---

## Recommendations
Based on model results, recommended actions included:
- Expanding telehealth options for patients with unpredictable schedules
- Reinforcing preventative care messaging for acute-care patients
- Recognizing Medicaid transition as a major, expected driver of churn
- Connecting patients with unstable housing to support services
- Continuing strong multilingual support offerings

---

## Limitations
- Lack of qualitative patient feedback
- Potential edge cases in churn definition (e.g., newly enrolled patients)
- Results reflect historical behavior and may shift over time

---

## Repository Structure
- `src/` – Modular R scripts for data preparation, modeling, and evaluation
- `notebooks/` – Code walkthroughs, Quarto files, and detailed analysis
- `reports/` – Final reports and stakeholder-facing materials, including:
  - `Handout_Material.pdf` – Handout from the presentation
  - `Executive_Insights_Summary.pdf` – Executive insights summary
- `data/` – Input datasets
- `README.md` – Overview of the project, repository structure, and key insights


---

## Notes
This project uses de-identified data and is shared for educational and portfolio purposes.

