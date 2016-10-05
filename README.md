"tsSelect Package - Choose the best time series model" 

  This package has as input a Time Series object.
  
  The first step is fitting a series of model:
  
  - Mean model
  - Naive model (all forecasts = last observation)
  - Seasonal Naive model (each forecast is equal to the last observed value
                            from the same season)
  - 'drift' model (adds a "trend" over time to the naive method)
  - tslm_1 (linear-regression by 'trend')
  - tslm_2 (linear-regression by 'trend' and 'season')
  
  - Exponential smoothing models (with several variations)
  
  - ARIMA (with and without stepwise selection)
  
  The second step is choosing the lowest error measures, either by a specific measure
  or by majority vote among all possible error measures.
  
  The possible error measures are:
  ME, RMSE, MAE, MPE ,MAPE, MASE, ACF1
  
  
  
  
