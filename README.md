Part 1: This was done in R Programming Language

Time Series Analysis and Forecasting of Beer Sales & CME Seat Prices
Overview
This project is part of my final assignment for the MS in Applied Data Science program. The project consists of two main parts:

Time Series Forecasting of Beer Sales (1975-1990)

Utilized ARIMA models to forecast monthly beer sales for 1990 using two different multi-step forecasting approaches.
Analyzed the residuals of the models to check for white noise, normality, and autocorrelation.
Compared the mean squared error (MSE) and computational time for the two approaches.
Interpolation of CME Seat Prices (2001-2013)

Developed an algorithm to create a continuous monthly time series from irregularly spaced data for three classes of CME seats.
The algorithm was applied to CME, IMM, and IOM seat prices, with results validated through visualization.
Data Sources
Beer Sales Data: Monthly beer sales data (in millions of barrels) from January 1975 to December 1990, available in the TSA package.
CME Seat Prices: Confidential seat price data from the Chicago Mercantile Exchange, spanning January 2001 to December 2013.
1A - Recursive Forecasting

Method: Used the forecast() function with an h period to predict all months of 1990.
Analysis: Residuals were analyzed for autocorrelation, mean, and normality.
1B - Direct Recursive Forecasting
Method: Forecasted each month of 1990 individually, updating the model with each new forecast.
Analysis: Residuals were similarly analyzed for model performance.

Comparison & Results
Plots: Included plots comparing actual vs. forecasted values for both methods.
MSE Calculation: Computed MSE for 1990 to determine model accuracy.
Computational Time: Discussed the difference in computation time between the two methods.


Interpolation of CME Seat Prices
Methodology
Algorithm: Developed a custom algorithm to interpolate missing seat prices, creating a complete monthly time series for each seat class.
Why This Method?: Selected this approach for its ability to handle irregularly spaced data and ensure consistency across all three datasets.
Alternative Considerations
Other Methods: Considered methods such as spline interpolation and linear regression, but they were rejected due to limitations in handling large gaps or irregular time intervals.
Visualization
Plots: Three plots showing the original data with gaps and the interpolated monthly data points overlaid in red.

Part 2: This was done in R Programming Language

Linear Regression Analysis on Brooklyn Real Estate Data (2016-2020)

In this project, I conducted a comprehensive analysis of Brooklyn real estate purchases from 2016 to 2020, focusing on single-family residences and single-unit apartments or condos. I used linear regression models to explain housing prices during this period. The analysis involved extensive data cleaning, feature engineering, and model evaluation, ensuring optimal predictive accuracy while maintaining the model's interpretability. This work was part of my final assignment for my MS program.
