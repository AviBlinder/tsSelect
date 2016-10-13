[![Build Status](https://travis-ci.org/AviBlinder/tsSelect.svg?branch=master)](https://travis-ci.org/AviBlinder/tsSelect)
<h3> "tsSelect Package - Pick the best time series model" </h3>

  This package has as input a Time Series object.
  
The first step fits a series of models:
  
  - Mean model
  - Naive model (all forecasts = last observation)
  - Seasonal Naive model (each forecast is equal to the last observed value
                            from the same season)
  - 'drift' model (adds a "trend" over time to the naive method)
  - tslm_1 (linear-regression by 'trend')
  - tslm_2 (linear-regression by 'trend' and 'season')
  
  - Exponential smoothing models (with several variations)
  
  - ARIMA (with and without stepwise selection)
  
The second step picks the model with lowest error measures, either by a specific measure or by majority vote, among all possible error measures.
  
The different error measures are:
  ME, RMSE, MAE, MPE ,MAPE, MASE, ACF1
  

***Execution example:***

library(tsSelect)

run_models(ts_object)
  
  
  
  
  
