TMDB Pre-release Hit + Revenue Triage
============================================================

Overview
--------
This project predicts movie hit probability and revenue before release using TMDB data (1990–2023). It combines advanced analytics, feature engineering, and machine learning models (Logistic Regression, Random Forest, XGBoost) to generate business KPIs, shortlists, and Expected Net Benefit (ENB) metrics.

Repository Structure
--------------------
1. TMDB_GREENLIGHTING_CLEANSCRIPT.R
   - Cleans and preprocesses TMDB dataset.
   - Handles missing values, normalizes text fields, and creates engineered features.


2.TMDB_GreenLighting.R
   - scripts with XGBoost integration, calibration, and advanced visuals.
   - Adds radar charts, heatmaps, and per-title ENB curves.

Dataset:
--------
The full dataset is available here:
https://drive.google.com/drive/folders/1Q42tNfQQKRAWQ0-ir4T9n6Nl58yd_vo0?usp=drive_link

How to Run
----------
1. Run TMDB_GREENLIGHTING_CLEANSCRIPT.R to clean raw TMDB data and export `tmdb_1990_2023_model_input.csv`.
2. Execute TMDB_GreenLighting.R for modeling and predictions.
3. Ensure R environment with required libraries (tidyverse, tidymodels, xgboost, etc.).

Outputs
-------
- Predicted hit probabilities and revenue for upcoming titles.
- ENB-based decision tables and override recommendations.
- Visualizations: Radar charts, heatmaps, ENB curves, and genre contribution plots.
- Shortlists and KPIs for business decision-making.

Author
------
Rohith Ramakrishnappa
