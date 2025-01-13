# Regression Analysis Assignment

This project involves the completion of a regression analysis assignment using the R programming language. The study includes data analysis using various transformations and modeling techniques.

## Packages Used
- `car`
- `fBasics`
- `lmtest`
- `MASS`
- `nortest`
- `olsrr`
- `stats`
- `zoo`
- `fastDummies`
- `DAAG`

## Steps

1. **Data Loading and Cleaning**:
   - Data was loaded using the `read.table()` function, and variable names were renamed.
   - Outliers were detected and cleaned using the `boxplot()` and `which()` functions.

2. **Normality Tests**:
   - Normality tests were performed using `shapiro.test()` and `ks.test()`.
   - Data transformations (logarithmic and square root transformations) were attempted.

3. **Modeling**:
   - Regression models were created using the `lm()` function.
   - Model statistics and confidence intervals were examined.
   - Model assumptions were tested using `bptest()` and `dwtest()`.

4. **Model Selection and Adjustments**:
   - Forward, backward, and stepwise model selection was performed using `step()` and `stepAIC()`.
   - Ridge regression was applied using the `lm.ridge()` function.

## Files
- `2220381067_HakkiKondak.txt`: The initial dataset.
- `2220381067_HakkiKondak(1).txt`: The dataset after removing outliers.
- `2220381067_HakkiKondak(2).txt`: The final dataset.

## How to Run
1. Open the project in R or RStudio.
2. Install the necessary packages (using the `install.packages()` function).
3. Run the `regresyon_odev.r` file to follow the steps.

## Results
- As a result of analyzing and modeling the data, it was observed that the data was made to conform to normality assumptions with certain transformations and analyzed using appropriate regression models.
