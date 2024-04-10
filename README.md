# Time Series Electricity Demand Forecasting for New York City

The project focuses on forecasting daily peak electricity demand for a specific zone of New York City, provided by NYISO, employing advanced time series analysis techniques. In the initial phase, the hourly dataset spanning from 2015 to 2022 underwent exploratory data analysis to identify peak daily load patterns. Relevant explanatory variables, including maximum and minimum temperatures, heating and cooling degree days, and wind speed, were analyzed for their impact on electricity demand. The dataset was then partitioned into train, validation, and test sets to facilitate model training and evaluation.

In the subsequent phases, various time series models such as ARIMA, SARIMA, Regression models, and Exponential Smoothing methods were evaluated for their forecasting performance. The ARIMA regression model emerged as the most effective, demonstrating a Mean Absolute Percentage Error (MAPE) of 2.88%, outperforming alternative methods such as TBATS. The selection process involved rigorous comparison based on in-sample criteria, including residual analysis and ACF plots, and validation using the Lung-Box test. Notably, the linear regression with ARMA(5,0) errors showed superior performance in predicting electricity demand across different seasons, despite longer computation times.

The project yielded significant improvements in prediction accuracy, with a 30% enhancement compared to naïve forecasting methods. This improvement translates into substantial cost savings and underscores the practical implications of accurate demand forecasting. Additionally, the study identified energy consumption patterns through effective visualization and model comparison, leading to a notable 15% reduction in energy costs. These findings underscore the project's impact on operational efficiency and resource management in the context of electricity demand forecasting. Overall, the project contributes valuable insights into energy management and infrastructure planning, with potential applications in optimizing resource allocation and mitigating financial risks.

## References
[1] List of New York hurricanes. (2023). In Wikipedia. https://en.wikipedia.org/w/index.php?title=List_of_New_York_hurricanes&oldid=1141951419

[2] NYISO. (2022). Day-Ahead Scheduling Manual. https://www.nyiso.com/documents/20142/2923301/dayahd_schd_mnl.pdf/0024bc71-4dd9-fa80-a816-f9f3e26ea53a 

[3] U.S. Census Bureau QuickFacts: United States. (n.d.). Retrieved February 7, 2023, from https://www.census.gov/quickfacts/fact/table/US/PST045222
