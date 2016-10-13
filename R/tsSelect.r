#' @return the best time series model
#' @title Run ts Models
#' @author Avi Blinder
#' @description Function that executes several models and picks
#' the best one.
#' @param ts1 A timeseries object
#' @param accuracy_measure - Possilbe error meassures:
#'        ME, RMSE, MAE, MPE ,MAPE, MASE, ACF1
#' @export
#' @import forecast
#' @examples
#' data(ros1_ts)
#' run_models(ros1_ts)
#' run_models(ros1_ts,"RMSE")



run_models <- function(ts1,accuracy_measure = NULL){

  check_object(ts1)

  ##Check Auto-Correlations
  acf <- Acf(ts1)
  acf_max <- sort(acf$acf,decreasing = T)[2]
  acf_treshold <- abs(2/length(ts1))/0.1
  #filter 'white noise" series
  if (acf_max < acf_treshold){
    cat ("white noise !!")
    stop
  }

  ##################################################################################
  #Step 1: Fit models

  fit_tbats <- tbats(ts1)
  seasonal <- !is.null(fit_tbats$seasonal)


  fit_benchm1 <- meanf(ts1,6)  # Mean forecast (x, h) h = horizon

  fit_benchm2 <- naive(ts1,6)  # Navive forecast (all forecasts = last observation)

  if(seasonal){
    fit_benchm3 <- snaive(ts1,6) # Seassonal Naive

    fit_lm2 <- tslm(ts1 ~ trend + season)

    fit_hw1    <- hw(ts1,seasonal="additive")

    fit_hw2    <- hw(ts1,seasonal="multiplicative")

    }
    #    (each forecast to be equal to the last observed value
    #      from the same season)

  fit_benchm4 <- rwf(ts1,6,drift = TRUE) #Drift method - adds a "trend" over time to the naive method

  fit_lm1 <- tslm(ts1 ~ trend)

  fit_ses <- ses(ts1)

  fit_ets <- ets(ts1)

  fit_holt1 <- holt(ts1)

  fit_holt2 <- holt(ts1,exponental=TRUE)

  fit_holt3 <- holt(ts1,damped=TRUE)

  fit_holt4 <- holt(ts1,exponential=TRUE,damped=TRUE)

  fit_auto_arima1 <- auto.arima(ts1,stepwise=TRUE)

  fit_auto_arima2 <-  auto.arima(ts1, stepwise=FALSE, approximation=FALSE)

  ##################################################################################
  #Step 2: Measure accuracies

  ac_benchm1 <- data.frame(accuracy(fit_benchm1))
  ac_benchm1$model <- "meanf"
  row.names(ac_benchm1) <- NULL
  #
  ac_benchm2 <- data.frame(accuracy(fit_benchm2))
  ac_benchm2$model <- "naive"
  row.names(ac_benchm2) <- NULL
  #
  ac_benchm4 <- data.frame(accuracy(fit_benchm4))
  ac_benchm4$model <- "rwf"
  row.names(ac_benchm4) <- NULL
  #
  ac_lm1     <- data.frame(accuracy(fit_lm1))
  ac_lm1$ACF1 <- NA
  ac_lm1$model <- "lm_with_trend"
  row.names(ac_lm1) <- NULL

  ac_tbats <- data.frame(accuracy(fit_tbats))
  ac_tbats$model <- "tbats"
  row.names(ac_tbats) <- NULL

  #
  ac_ets     <- data.frame(accuracy(fit_ets))
  ac_ets$model <- "ets"
  row.names(ac_ets) <- NULL
  #
  ac_ses     <- data.frame(accuracy(fit_ses))
  ac_ses$model <- "ses"
  row.names(ac_ses) <- NULL

  #
  if (seasonal){
    ac_benchm3 <- data.frame(accuracy(fit_benchm3))
    ac_benchm3$model <- "snaive"
    row.names(ac_benchm3) <- NULL

    ac_lm2     <- data.frame(accuracy(fit_lm2))
    ac_lm2$ACF1 <- NA
    ac_lm2$model <- "lm_with_trend_and_season"
    row.names(ac_lm2) <- NULL

    #
    ac_hw1     <- data.frame(accuracy(fit_hw1))
    ac_hw1$model <- "Holt-Winters_additive"
    row.names(ac_hw1) <- NULL

    ac_hw2     <- data.frame(accuracy(fit_hw2))
    ac_hw2$model <- "Holt-Winters_multiplicative"
    row.names(ac_hw2) <- NULL

  }
  #

  ac_holt1   <- data.frame(accuracy(fit_holt1))
  ac_holt1$model <- "Holt_simple"
  row.names(ac_holt1) <- NULL
  #
  ac_holt2   <- data.frame(accuracy(fit_holt2))
  ac_holt2$model <- "Holt_exponential"
  row.names(ac_holt2) <- NULL
  #
  ac_holt3   <- data.frame(accuracy(fit_holt3))
  ac_holt3$model <- "Holt_damped"
  row.names(ac_holt3) <- NULL
  #
  ac_holt4   <- data.frame(accuracy(fit_holt4))
  ac_holt4$model <- "Holt_exponential_damped"
  row.names(ac_holt4) <- NULL

  ac_auto_arima1     <- data.frame(accuracy(fit_auto_arima1))
  ac_auto_arima1$model <- "Auto_Arima"
  row.names(ac_auto_arima1) <- NULL

  ac_auto_arima2     <- data.frame(accuracy(fit_auto_arima2))
  ac_auto_arima2$model <- "Auto_Arima_No_Stepwise"
  row.names(ac_auto_arima2) <- NULL

  ##################################################################################
  #Step 3: Combine models and pick best one

  if (seasonal){

    accuracies <- rbind(ac_benchm1,ac_benchm2,ac_benchm3,ac_benchm4,
                        ac_lm1,ac_lm2,ac_ets,ac_ses,
                        ac_holt1,ac_holt2,ac_holt3,ac_holt4,ac_hw1,ac_hw2,
                        ac_auto_arima1,ac_auto_arima2,ac_tbats)
    all_models <-   list("meanf"    =    fit_benchm1 ,
                         "naive"         =    fit_benchm2 ,
                         "snaive"        =    fit_benchm3 ,
                         "rwf"           =    fit_benchm4 ,
                         "lm_with_trend" =    fit_lm1     ,
                         "lm_with_trend_and_season" = fit_lm2 ,
                         "ses"           =    fit_ses ,
                         "ets"           =    fit_ets ,
                         "Holt-Winters_additive" =  fit_hw1 ,
                         "Holt-Winters_multiplicative" = fit_hw2 ,
                         "Holt_simple"   =    fit_holt1 ,
                         "Holt_exponential"  =    fit_holt2 ,
                         "Holt_damped"       =    fit_holt3 ,
                         "Holt_exponential_damped" =    fit_holt4 ,
                         "Auto_Arima"        =    fit_auto_arima1 ,
                         "Auto_Arima_No_Stepwise" =    fit_auto_arima2,
                         "TBATS model" = fit_tbats)

  } else {
    accuracies <- rbind(ac_benchm1,ac_benchm2,ac_benchm4,
                        ac_lm1,
                        ac_holt1,ac_holt2,ac_holt3,ac_holt4,
                        ac_auto_arima1,ac_auto_arima2,ac_tbats)
    all_models <-   list("meanf"    =    fit_benchm1 ,
                         "naive"         =    fit_benchm2 ,
                         "rwf"           =    fit_benchm4 ,
                         "lm_with_trend" =    fit_lm1     ,
                         "Holt_simple"   =    fit_holt1 ,
                         "Holt_exponential"  =    fit_holt2 ,
                         "Holt_damped"       =    fit_holt3 ,
                         "Holt_exponential_damped" =    fit_holt4 ,
                         "Auto_Arima"        =    fit_auto_arima1 ,
                         "Auto_Arima_No_Stepwise" =    fit_auto_arima2,
                         "TBATS model" = fit_tbats)

  }

  if(is.null(accuracy_measure)){
    x2 <- c()
    for (i in 1:ncol(accuracies)){
      x1 <- accuracies[accuracies[,i] == min(accuracies[,i],na.rm = TRUE),"model"]
      x2 <- c(x2,x1)
    }
    selected_model <- sort(table(x2),decreasing = TRUE)[1]
  } else {
    col <- which(names(accuracies) == accuracy_measure )
    selected_model <- accuracies[accuracies[,col] == min(accuracies[,col],na.rm = TRUE),"model"]
    if (length(selected_model) > 1) {
      selected_model <- selected_model[3]
    }
  }

  return    <- list(selected = selected_model,
                    model=all_models[names(all_models) == names(selected_model)]) }

#'
#' @return stops if object not a ts class
#' @author Avi Blinder
#' @title Check Object class
#' @description Internal function that verifies the class of the object
#'  (should be time series)
#' @details internal function for verifying that the object belongs
#'  to class "time series"
#' @param x A timeseries object


check_object <- function(x){

  if (length(x) == 0 ) {
    cat ("missing input variable")
    stop("missing input")

  }

}

