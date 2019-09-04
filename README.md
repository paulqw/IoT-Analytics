# **Market basket analysis**  
  
Date: 02/09/2019  
  
## Project summary
**Business case:** Working as an IoT-Analyst at the fictitious company IoT-Analytics for a developer(client) of Smart Homes. Their major goal is to develop highly efficient Smart Homes make use of cutting-edge technology like electrical sub-metering devices and power management enhanced by IoT-Analytics.  
To support the client and empower Smart Home owners with greater understanding and control of their electrical power usage a large dataset has been analysed containing 47 month of energy usage data of an average house.  
  
**Goal of analysis:** The goal of analysis is to perform time series analysis to discover seasonalities and trends within a typical profile of a family’s electrical consumption. Based on the analysis meaningful visualization strategy has been pursued to add the most value to potential customers of the client - Smart Home owners. Hence, both the total electrical power consumption and available data for submetering-devices installed for the laundry, kitchen, and heating/AC appliances have been analysed. Mainly, the analysis unveils the total yearly amount of energy consumed, allows a deep dive in the consumption split by submeters, seeks to forecast the electrical consumption, and demonstrates possible saving potential. Finally, results are visualized and organised in a dashboard using Power BI.  
  
## Technical approach of analysis  
  
**1. Exploration and preparation of data** 
- Set time-zone, format, and intervals 
- Identify, pad, and fill NAs
- Aggregate and check different granularity
  
**2. Stationarity, autocorrelation, and transformation**    
- Create and analysis (multi-seasonal) decomposition
- Check stationarity and autocorrelation
- Ensure stationarity/conduct transformations
  
**3. (Multi-seasonal) Time series and data splitting**  
- Create time series and split data 
  
**4. Train models, evaluate, forecast**  
- Train several forecasting models (e.g. ARIMA, prophet)
- Perform cross-validation
- Evaluate models based on several performance measures such as RMSE, MAE, MAPE
- Forecast energy consumption
  
**5. Create visualization and dashboard**  
  