---

# 🫀 Predicting Coronary Heart Disease (CHD) Risk

This project explores the risk prediction of **Coronary Heart Disease (CHD)** using patient data, applying both statistical modeling and machine learning techniques. The analysis is performed in **R** and structured around a classic Data Science workflow: **ETL → EDA → Modeling → Evaluation → SMOTE Resampling**.

---

## 🔍 Overview

CHD remains a major cause of death worldwide. Early risk prediction can drive proactive prevention and intervention. This project aims to:

- Clean and preprocess CHD patient data
- Explore relationships through data visualization
- Build classification models to predict CHD occurrence
- Address **class imbalance** via SMOTE
- Evaluate models with a focus on **false negatives**

---

## 📦 Tech Stack & Libraries

- **Language**: R
- **Key Libraries**:  
  `tidyverse`, `caret`, `e1071`, `ROCR`, `smotefamily`, `ggplot2`, `corrplot`, `patchwork`, `car`

---

## 🔄 Workflow

### 🧼 ETL
- Load patient-level data
- Impute missing values (medians for continuous, modes for categorical)
- Convert categorical variables to factors
- Identify nominal variables for later analysis

### 📊 EDA
- **Boxplots & barplots** to visualize relationships with CHD
- **Chi-square tests & Cramer's V** to quantify categorical relationships
- **Correlation matrix** to detect collinearity
- Examine **class imbalance** of the response variable (CHD)

### 🧪 Modeling (Phase 1)
- **Logistic Regression (GLM)**: classic statistical approach
- **K-NN**: non-parametric classification
- Evaluate using:
  - Accuracy
  - Sensitivity
  - Specificity
  - Confusion Matrix
- ➡️ Results: High specificity, **poor sensitivity** due to class imbalance

### 🧬 Modeling (Phase 2 - SMOTE)
- Apply **SMOTE** to synthetically balance the training set
- Retrain:
  - **Logistic Regression**
  - **K-NN** (tuned for sensitivity)
- Re-evaluate performance, with a focus on reducing **False Negative Rate (FNR)**

---

## 📈 Highlights

| Model             | Accuracy | Specificity | Sensitivity | Notes |
|------------------|----------|-------------|-------------|-------|
| Logistic (Base)  | High     | Excellent   | 🚨 Very Low | Biased toward "No CHD" |
| K-NN (Base)       | High     | Excellent   | 🚨 Very Low | Can't identify minority class |
| Logistic (SMOTE) | Moderate | Slight drop | ✅ Improved | Balanced with synthetic data |
| K-NN (SMOTE)     | Moderate | ✅ Good      | ✅✅ Great   | Best in sensitivity |

---

## 💡 Key Takeaways

- **EDA is critical**: Visual + quantitative checks shape modeling decisions.
- **Class imbalance hurts**: Especially for life-critical predictions.
- **SMOTE works**: Improves minority class performance.
- **Sensitivity matters**: In healthcare, catching **false negatives** is vital.

---

## 🧠 Future Work

- Explore **ensemble methods** (Random Forest, XGBoost)
- Implement **cost-sensitive learning**
- Deploy models into a Shiny app for interactive CHD risk prediction

---

## 🧪 Run It Yourself

1. Clone this repo
2. Open the R script in RStudio
3. Make sure all libraries are installed (`pacman::p_load(...)`)
4. Run the script line by line to follow the pipeline

---

## 📁 Dataset

A CSV file of anonymized patient data including:
- Demographics (Age, Sex, Education)
- Health metrics (Cholesterol, Blood Pressure, BMI, HR)
- Lifestyle indicators (Smoking, Stroke, Diabetes)
- Outcome: `CHD` (Yes/No)

---

## 🤝 Credits

Project by **Ettore** as part of a **Statistical Modeling course** at UNITN  
Thanks to the amazing R community and the authors of the `smotefamily`, `caret`, and `ggplot2` packages!

---

## 📜 License

This project is for educational purposes. Feel free to fork, learn, and adapt — with attribution.  
🧠 Knowledge is meant to be shared.

---
