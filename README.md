# Hospital Patient Analytics Dashboard Using R Programming & Shiny

[![R](https://img.shields.io/badge/R-4.6%2B-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.14%2B-0284c7.svg)](https://shiny.posit.co/)
[![Machine Learning](https://img.shields.io/badge/ML-Random%20Forest-10b981.svg)](https://cran.r-project.org/web/packages/randomForest/)
[![License](https://img.shields.io/badge/License-MIT-purple.svg)](LICENSE)

An enterprise-grade, modern healthcare analytics intelligence system and machine learning platform built using **R, Shiny, bslib, Plotly, and Random Forest**.

Designed for hospital executives, clinical directors, epidemiologists, and data analysts, this platform ingests inpatient clinical records, performs automated data cleansing and schema validation, tracks real-time hospital performance across clinical, operational, and financial dimensions, and predicts patient discharge outcomes using supervised machine learning.

---

## 🌟 Key Features

### 1. Executive Overview & Real-Time KPIs
- **8 Dynamic KPI Cards**:
  - Total Patient Population
  - Inpatient Encounters / Admissions
  - Population Mean Age
  - Average Length of Stay (ALOS in days)
  - Gross Hospital Billed Revenue (USD)
  - Average Cost per Patient Encounter
  - Clinical Discharge & Recovery Success Rate
  - Operational Medical Specialties & Departments
- **6 High-Impact Interactive Plotly Visualizations**:
  - Monthly Inpatient Admissions Trend (with smoothed spline area)
  - Primary Disease Distribution (interactive donut breakdown)
  - Gender Cohort Distribution
  - Department-wise Patient Census (horizontal bar chart)
  - Clinical Outcome Distribution
  - Gross Hospital Revenue Trajectory

### 2. Comprehensive Cohort Demographics
- Patient Age Distribution Histogram with Kernel Density Overlay
- Demographic Stratification across Life Stages (Pediatric, Young Adult, Middle Aged, Geriatric)
- ABO and Rh Factor Blood Group Prevalence Analysis
- Age vs. Primary Diagnosis Distribution Boxplots

### 3. Epidemiological Disease Analytics
- Top 10 Diagnosed Medical Conditions Ranking
- Interactive Disease Deep-Dive Explorer with dedicated condition dossier (Average Age, Length of Stay, Procedure Breakdown, Common Medications, Recovery Rate)
- Disease Breakdown across Gender and Age Brackets
- Longitudinal Multi-Condition Incidence Time Series

### 4. Inpatient Admission Dynamics
- Multi-Scale Admission Velocity (toggle between **Daily**, **Monthly**, and **Yearly** views)
- Intake Severity Categorization (Emergency vs. Elective vs. Urgent vs. Trauma)
- Length of Stay (LOS) Frequency Distribution
- Inpatient Stay Duration by Admission Classification

### 5. Financial & Revenue Intelligence
- Departmental Gross Billing Contributions
- Insurance Payer Mix Breakdown (Medicare, Blue Cross, UnitedHealthcare, Aetna, Cigna, Medicaid, Self-Pay)
- Treatment Cost Analysis by Medical Condition
- Revenue Generation by Admission Classification

### 6. Clinical Department & Physician Scorecards
- Bed Utilization and Turnover (Average LOS by Specialty)
- Physician Caseload Volume Ranking
- Doctor-Associated Billing Contributions
- Average Hospital Days per Attending Physician

### 7. Interactive Patient Registry & Clinical Dossier
- High-Performance DT DataTable with full-text search, column filtering, pagination, and multi-column sorting
- Color-Coded Status and Admission Badges
- **Interactive Patient Dossier**: Selecting any patient row displays an immediate medical card and **Clinical Journey Timeline** (Hospital Intake → Specialty Assignment → Therapy & Prescription → Outcome Status).

### 8. Machine Learning Patient Outcome Prediction
- **Algorithm**: Supervised Random Forest Classifier (`randomForest`)
- **Target Feature**: `Patient_Status` (Recovered, Discharged, Under Treatment, Critical, etc.)
- **Predictive Features**: Patient Age, Gender, Primary Disease, Department, Admission Type, Insurance Payer, Expected Length of Stay, and Estimated Billing Amount
- **Data Safeguards**: Automatic 80% Train / 20% Test Split with strictly zero data leakage
- **Performance Evaluation**: Accuracy, Precision, Recall, Macro F1-Score, and Test Set Confusion Matrix
- **Feature Importance**: Gini Importance Ranking
- **Real-Time Prediction Simulator**: Interactive clinical intake form with instant outcome prediction, confidence score meter, case summary, and medical disclaimers
- **Retraining Engine**: One-click model retraining on updated hospital datasets with persistence to `models/patient_outcome_model.rds`

### 9. Multi-Format Report Generation
- Direct exports in both **CSV** and styled **Microsoft Excel (.xlsx)** workbooks (using `openxlsx`):
  1. Complete Patient Population Dataset
  2. Currently Filtered Cohort Dataset
  3. Disease Epidemiology Report
  4. Clinical Department Performance Report
  5. Hospital Financial Summary Report
  6. Patient Outcomes & ML Predictions Audit

### 10. Data Quality & Schema Integrity Audit
- Algorithmic Data Health Score Gauge
- Total Ingested vs. Deduplicated Clean Records
- Column Completeness and Missing Value Audit Table
- Dynamic Schema and Column Aliasing Detection

---

## 🛠️ Technology Stack

| Layer | Technologies |
| :--- | :--- |
| **Language** | R (v4.4+) |
| **Framework** | Shiny, bslib |
| **Data Processing** | dplyr, tidyr, readr, lubridate |
| **Data Visualization**| plotly, ggplot2 |
| **Interactive Tables**| DT (DataTables) |
| **Spreadsheets** | openxlsx |
| **Machine Learning** | randomForest, caret |
| **Styling & Assets** | Custom CSS3 (Modern Clinical Theme), FontAwesome 6, SVG/PNG |

---

## 📁 Project Architecture

```
hospital-patient-analytics/
│
├── app.R                       # Complete Shiny application (UI & Server)
│
├── data/
│   └── hospital_patients.csv   # Inpatient clinical database
│
├── models/
│   └── patient_outcome_model.rds # Serialized Random Forest model & metrics
│
├── www/
│   ├── style.css               # Healthcare CSS design system
│   ├── logo.svg                # Scalable vector clinical crest
│   └── logo.png                # High-res hospital platform branding
│
├── install_packages.R          # Automated CRAN dependency installer
├── generate_dataset.py         # Realistic medical data synthesis engine
├── requirements.txt            # Dependency specification
└── README.md                   # Comprehensive documentation
```

---

## 📥 Installation & Setup

### Prerequisites
- Install **R** (Version 4.2 or higher): [Download from CRAN](https://cran.r-project.org/)
- (Optional) Install **RStudio**: [Download RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### 1. Clone or Open the Repository
```bash
cd "c:/Users/Asus/hospital dashboard"
```

### 2. Install Required R Packages
Run the automated installer script:
```r
# In R or RStudio terminal:
source("install_packages.R")
```
Or manually run:
```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "plotly", 
  "DT", "readr", "lubridate", "tidyr", "openxlsx", 
  "caret", "randomForest"
))
```

---

## 🚀 Running the Application

Launch the application directly from the command line:

```bash
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

Or from within an interactive R / RStudio session:

```r
shiny::runApp()
```

The application will start the local HTTP server and open in your default web browser at `http://127.0.0.1:3838`.

---

## ☁️ Deploying on Render (Free Tier Step-by-Step Guide)

Render does not offer a native R runtime, so the dashboard is packaged with a high-performance **Dockerfile** using `rocker/shiny-verse:latest` and pre-compiled Linux binary packages from the **Posit Package Manager (P3M)**. This ensures fast build times (under 2 minutes) without memory limits or compilation errors.

### Method 1: Automated Blueprint Deployment (Recommended)

1. **Push your code to GitHub or GitLab**:
   ```bash
   git init
   git add .
   git commit -m "Hospital Patient Analytics Dashboard ready for Render"
   git branch -M main
   git remote add origin https://github.com/YOUR_USERNAME/hospital-patient-analytics.git
   git push -u origin main
   ```

2. **Log into Render**:
   - Go to [dashboard.render.com](https://dashboard.render.com/) and sign in with GitHub.

3. **Deploy with Blueprint**:
   - Click the **"New +"** button in the top navigation bar.
   - Select **"Blueprint"**.
   - Connect your GitHub repository: `hospital-patient-analytics`.
   - Render will automatically read [`render.yaml`](file:///c:/Users/Asus/hospital%20dashboard/render.yaml) and configure the Docker Web Service.
   - Click **"Apply"**.

4. **Your dashboard goes live**:
   - Render will build the Docker container and deploy the app at:
     `https://hospital-patient-analytics.onrender.com`

---

### Method 2: Manual Web Service Deployment

If you prefer setting up the web service manually on Render:

1. Click **"New +"** → **"Web Service"**.
2. Select your repository.
3. Configure the settings:
   - **Name**: `hospital-patient-analytics`
   - **Region**: Choose the closest region (e.g., *Oregon (US West)* or *Frankfurt (EU)*).
   - **Runtime**: **Docker** (Render detects the included `Dockerfile`).
   - **Instance Type**: **Free**.
4. **Advanced Settings (Environment Variables)**:
   - `PORT`: `10000` (Render routes web traffic to this port).
   - `HOST`: `0.0.0.0`
5. Click **"Create Web Service"**.
   - Build completes in ~2 minutes and your live URL is active!

---

---

## 📊 Dataset Schema & Handling

The ingestion pipeline automatically maps any incoming CSV dataset into standard canonical fields:

| Field | Type | Description |
| :--- | :--- | :--- |
| `Patient_ID` | String | Unique patient identifier (`PAT-XXXX`) |
| `Name` | String | Full name of patient |
| `Age` | Numeric | Patient age in years |
| `Gender` | Factor | Male, Female, Other |
| `Blood_Group`| Factor | ABO and Rh factor (A+, O-, B+, etc.) |
| `Disease` | Factor | Primary diagnosed medical condition |
| `Department` | Factor | Medical ward / specialty unit |
| `Doctor` | Factor | Attending physician |
| `Admission_Date`| Date | Date of inpatient admission |
| `Discharge_Date`| Date | Date of discharge (blank for active cases) |
| `Admission_Type`| Factor | Emergency, Elective, Urgent, Trauma, Routine |
| `Treatment` | Factor | Primary procedure or therapeutic regimen |
| `Medication` | Factor | Prescribed pharmaceutical treatment |
| `Insurance` | Factor | Health insurance provider or Self-Pay |
| `Billing_Amount`| Numeric | Total billed hospital charges ($) |
| `Patient_Status`| Factor | Clinical outcome (Recovered, Discharged, Under Treatment, Critical, etc.) |
| `Length_of_Stay`| Numeric | Hospitalization duration in days |

### Adaptive Ingestion Features:
- **Automatic Aliasing**: Recognizes column naming variations (`patient_id`, `MRN`, `Diagnosis`, `cost`, `charges`, `LOS`, etc.).
- **Missing Value Imputation**: Gracefully imputes missing numerical and categorical values without crashing.
- **Date Normalization**: Parses ambiguous date formats (`YYYY-MM-DD`, `DD/MM/YYYY`, `MM/DD/YYYY`).
- **Length of Stay Synthesis**: Automatically calculates `Discharge_Date - Admission_Date` when missing.

---

## 🧠 Machine Learning Methodology

1. **Cohort Preprocessing**:
   - Categorical features are encoded as standardized factor vectors.
   - Low-frequency target classes are sanitized to ensure balanced cross-validation.
2. **Train/Test Splitting**:
   - Stratified 80% training set and 20% holdout test set.
   - Zero contamination or target leakage.
3. **Model Training**:
   - Random Forest ensemble with 120 bootstrap trees (`ntree = 120`).
   - Gini Impurity criterion for feature selection and split evaluation.
4. **Evaluation**:
   - Test Set Accuracy, Macro Precision, Recall, and F1-Score computed against the holdout set.
   - Confusion Matrix rendered as an interactive audit table.

---

## 🔒 Security & Privacy Notice
- The application uses anonymized synthetic healthcare records for research, educational, and portfolio demonstrations.
- No protected health information (PHI) or personally identifiable information (PII) is exposed.
- All machine learning predictions are analytical simulations and **do not constitute clinical diagnosis or medical guidance**.

---

## 📄 License
This project is open-source and distributed under the **MIT License**.
