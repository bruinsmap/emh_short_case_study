# Coca-Cola Efficient Market Hypothesis (EMH) Analysis

This project analyzes the stock performance of Coca-Cola (KO) in relation to the S&P 500, PepsiCo (PEP), and sugar prices using econometric methods in R.

## Project Structure

- `cocacola_emh.R`: The main R script for data analysis, modeling, and forecasting.
- `figures/`: A folder containing all generated plots and visualizations.
- `model_summary.txt`: A text file containing the summary of the final Arima model.
- `cocacola_emh.docx`: Documentation related to the analysis.

## Requirements

The script requires the following R libraries:
- `quantmod`
- `tseries`
- `lmtest`
- `urca`
- `forecast`
- `ggplot2`
- `extrafont`
- `dplyr`

## How to Run

1. Ensure all required libraries are installed.
2. Open `cocacola_emh.R` in RStudio or your preferred R environment.
3. Update the `setwd()` path if necessary.
4. Run the script. Figures will be automatically saved to the `figures/` directory.
