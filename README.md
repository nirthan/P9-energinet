# 🔋 Potential for Energy Sharing in Denmark  
### A Logistic Analysis of Energy Sharing Interest and Surplus Energy Coverage

## 📘 Overview

This project explores the potential of peer-to-peer energy sharing among Danish households, focusing on identifying the factors that influence individuals’ interest in participating and analyzing how solar energy surplus can be used to cover household consumption. The analysis is based on survey data and forecasting models, providing insight into the technical feasibility and social acceptance of energy-sharing initiatives in Denmark.

## 🎯 Objectives

- Construct logistic regression models to determine what influences household interest in energy sharing.
- Estimate how much household electricity consumption can be covered by solar panel surplus.
- Explore the feasibility of forecasting energy surplus and consumption using historical data.

## 🧪 Methods

- **Data Collection**: Survey conducted among solar panel owners and potential receivers of shared energy.
- **Modeling**:
  - Initial logistic regression model using 25 explanatory variables.
  - Applied LASSO regularization for variable selection.
  - Compared logit** and probit models to check the effect of the link function.
- **Forecasting**:
  - Implemented VAR(168) and naive forecasting models to predict surplus production and household consumption.
  - Conducted Granger causality tests to evaluate the effect of sunshine on surplus generation.

## 📊 Key Results

- **Model Performance**:
  - Initial logistic model: Adjusted R² = 37.67%, Goodness-of-Fit p = 0.3119.
  - Refined logistic model (LASSO): Adjusted R² = 57.55%, Goodness-of-Fit p = 1 (possible overfitting).
  - Probit models confirmed robustness of logistic findings.

- **Significant Predictors**:
  - Age (older individuals, especially 58–80)
  - Income (lower to moderate more likely to participate)
  - Electricity consumption (4,501–6,000 kWh range)
  - Solar panel size (6–7.99 kW users less likely)
  - Battery storage (ownership or interest in owning increases likelihood)

- **Interest in Energy Sharing**:
  - 72% of solar panel owners willing to share ~69% of their surplus.
  - 88.2% of potential receivers are open to participation.

- **Energy Coverage Potential**:
  - One solar owner’s surplus can supply:
    - 6 households in DK1
    - 9 households in DK2 (under optimal sunshine)
  - In 2023, full 100% coverage occurred:
    - 1,687 hours in DK1
    - 2,069 hours in DK2
  - Partial coverage (80%) increased those hours to:
    - 1,871 (DK1), 2,218 (DK2)

- **Forecasting Insights**:
  - VAR(168) outperformed naive forecasting.
  - Prediction accuracy declines over time, rolling forecasts suggested for improvement.
  - Sunshine significantly affects surplus production (Granger test), though energy storage complicates this relationship.

## 🌍 Implications

- Strong public interest in energy sharing indicates social feasibility.
- Surplus energy can meaningfully contribute to Denmark’s household energy demand.
- Effective implementation will require addressing variability in solar production and integrating energy storage solutions.
