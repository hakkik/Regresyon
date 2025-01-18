# Agricultural Yield Regression Analysis

This project analyzes the effects of various environmental factors on agricultural yield using comprehensive regression analysis. The study examines how light, temperature, water, and mineral levels influence plant yield.

## Overview

- Normality analysis of data
- Outlier detection and cleaning
- Multiple regression model creation
- Model assumptions verification
- Ridge regression analysis
- Forward/backward variable selection

## Required Libraries

```R
library(car)
library(fBasics)
library(lmtest)
library(MASS)
library(nortest)
library(olsrr)
library(stats)
library(zoo)
library(fastDummies)
library(DAAG)
```

## Analysis Steps

1. **Data Preprocessing**
   - Normality check
   - Log and square root transformations
   - Outlier detection and removal

2. **Model Building**
   - Multiple regression model setup
   - Verification of model assumptions
   - Calculation of confidence intervals

3. **Model Validation**
   - Residual analysis
   - Cook's distance calculation
   - VIF values check
   - Autocorrelation testing

4. **Variable Selection**
   - Forward selection
   - Backward selection
   - Stepwise selection
   - Ridge regression

## Usage

1. Place your dataset in the project's main directory
2. Update the file path according to your system:
```R
data=read.table("your_path/your_file.txt", header = T)
```
3. Run the code and analyze the results

## Requirements

- R 4.0.0 or higher
- All R packages listed above
- Input data in txt format with headers

## Key Features

- Outlier cleaning process
- Square root transformation for normality
- AIC criterion for model selection
- Comprehensive residual analysis
- Multiple variable selection methods

## Important Notes

- Dataset has been cleaned for outliers
- Square root transformation was used for normality assumption
- AIC criterion was used for model selection
- VIF analysis included for multicollinearity check

## Results

The analysis provides:
- Optimal model selection
- Confidence intervals for parameters
- Residual diagnostics
- Prediction capabilities
- Variable importance assessment

## Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a new Pull Request

## Project Structure

```
├── regression_analysis.R    # Main analysis script
├── data.txt                # Input data
└── README.md              # Project documentation
```
