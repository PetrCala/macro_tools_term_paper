# Source script for the termpaper for Tools for Modern Macroeconometrics, LS 22/23

####################### PACKAGE HANDLING ########################

#' Package loading function
#' 
#' Insert a vector/list of package names, install all missing ones,
#'  load all into workspace, and clean the environment
loadPackages <- function(package_list){
  # Install packages not yet installed
  installed_packages <- package_list %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("Installing package ", package_list[!installed_packages],"...", sep = ""))
    install.packages(package_list[!installed_packages])
  }
  # Package loading
  invisible(lapply(package_list, library, character.only = TRUE))
  print('All packages loaded successfully')
}

####################### DATA TRANSFORMATION ########################
#' Input an .xts object and adjust it for seasonality. Return
#' the adjusted object.
#' @param data - The .xts data object to be seasoned.
handleSeasonality <- function(data, series_name, verbose = F){
  new_dates <- index(data)
  data_ts <- ts_ts(data)
  data_seasonal <- decompose(data_ts) # $x, $seasonal, $trend, $random

  # Extract the seasonal componenets
  data_adjusted <- data_seasonal$x # Seasonally adjusted price index
  data_seasonal_fluctuations <- data_seasonal$seasonal # Seasonal fluctuations

  # Plot
  if (verbose){
    plot_main <- paste0(series_name, " seasonal fluctuations")
    plot.ts(data_seasonal_fluctuations, main = plot_main)
  }

  # To xts
  data_new <- xts(data_adjusted, order.by=new_dates)
  colnames(data_new) <- series_name
  # Return quietly
  invisible(data_new)
}

#' INCORRECT SPECIFICATIONS - DO NOT USE
#' Input a series and calculate the percentage changes. Return an xts object.
#' 
#' @param input_series - The series to transform
#' @param look_back [int] - Number of periods to look back. Set to 1 for monthly changes,
#'  to 4 for quarterly, and 12 for yearly.
getPercentageChanges <- function(input_series, series_name, look_back = 1){
  new_series <- xts(rep(NA,length(input_series)), order.by=index(input_series))
  new_dates <- c()
  for (i in 1:length(input_series)){
      if (i%%look_back==0){ # Desired iterations
          perc_change <- ((as.numeric(input_series[i]) / as.numeric(input_series[i-look_back]))  - 1) * 100 # Percentage change          }
          if (length(perc_change)!=0){ # Valid value
              new_series[i] <- perc_change
              new_date <- index(new_series[i])
              new_dates <- append(new_dates, new_date)
          }
      }
  }
  new_series <- na.omit(new_series[is.finite(new_series),])
  index(new_series) <- new_dates
  colnames(new_series) <- series_name
  return(new_series)
}
####################### STRUCTURAL VALIDATION ########################

#' Check structural integrity of the time series,
#' as well as stationarity, etc.
#' @param time_series - .xts object to analyze
#' @param series_name - name of the series, e.g. CPI
runStructuralChecks <- function(time_series, series_name, print_plots=T){
  print(paste0('Running structural checks for ', series_name))

  # Detect structural breaks using the breakpoints function
  print('Testing for breakpoints...')
  bp <- breakpoints(time_series ~ 1)

  # Test the stationarity of the segments between breakpoints
  statcheck(bp)

  # Run the augmented Dickey-Fuller test
  print(paste0('Running the augmented Dickey-Fuller test for ', series_name, '...'))
  adf_test <- adf.test(time_series, k = 1)
  print(adf_test)
  if(adf_test$p.value < 0.05) {
    print('The transformed series is stationary and has zero order of integration')
  } else {
    # Perform ADF test on the first difference of the series
    # pct_change <- getPercentageChanges(time_series, series_name)
    pct_diff <- na.omit(diff(time_series))
    adf_test_diff <- adf.test(pct_diff)
    if(adf_test_diff$p.value < 0.05) {
      print('The transformed series is integrated of order 1')
    } else {
      print('The transformed series is not stationary')
    }
  }
  if (print_plots){
    # Plot the periodogram and spectrum
    periodogram_title <- paste0('Periodogram of ', series_name)
    spectrum_title <- paste0('Spectrum of ', series_name)
    periodogram(time_series, main=periodogram_title)
    spectrum(time_series, main=spectrum_title)


    # Plot the ACF and PACF functions
    acf_title <- paste0('ACF of ', series_name)
    pacf_title <- paste0('PACF of ', series_name)
    acf(time_series, main=acf_title)
    pacf(time_series, main=pacf_title)
  }
}

####################### LINEAR MODEL ESTIMATION ########################

#' Specify a time series and return a data frame of p-values from the Box-Ljung test
#' for autocorrelation. Every combination of orders is considered up until the order 5.
getBoxLjung <- function(series){
    raw_data <- c()
    for (AR_order in 1:5) {
        for (MA_order in 1:5) {
            row <- c(AR_order, MA_order)
            model <- arima(series, order = c(AR_order, 0, MA_order))
            for (lag in seq(1,3, by = 1)) {
                p <- Box.test(model$residuals, type = "Ljung-Box", lag = lag)$p.value
                row <- append(row, p)
            }
            raw_data <- append(raw_data, row)
        }
    }
    p_vals <- matrix(raw_data, ncol = 5, byrow = TRUE)
    df <- data.frame(p_vals)
    colnames(df) <- c('AR order', 'MA order', '1 lag', '2 lags', '3 lags')
    return(df)
}

####################### FORECASTS ########################

#' Input a time series, the number of periods you want to forecast for
#' and make that forecast. Return the forecast in the form of an xts object
#' with the new periods. Automatically plots the results, unless silenced.
#' @param time_series - An .xts object of the series to forecast.
#' @param periods [int] - Number of periods to forecast ahead for
#' @param date_frequency [str] - Frequency of dates. Can be one of "day",
#'  "quarter", "year".
#' @param print_plots [bool] - If true, print out the forecast plot with CI.
forecastData <- function(time_series, periods, date_frequency, series_name, print_plots=T){
  # Validate input
  if (!date_frequency %in% c("month", "quarter", "year")){
    stop('Invalid day frequency')
  }

  # Get the forecast
  forecast_data <- forecast(time_series, h = periods)
  mean_forecast <- forecast_data$mean
  upper_ci <- forecast_data$upper[,2]
  lower_ci <- forecast_data$lower[,2]

  if (print_plots){
    plot_name <- paste0(series_name, " 2 years ahead forecast")
    plot.ts(time_series, main=plot_name, xlab="Time", ylab="Value")
    lines(mean_forecast, col="blue")
    lines(upper_ci, col="red")
    lines(lower_ci, col="red")
    legend("topright", legend=c("Mean Forecast", "95% CI"), col=c("blue", "red"), lty=1)
  }

  # Generate new dates
  last_available_date <- index(time_series)[length(time_series)]
  new_dates <- seq(last_available_date, by = date_frequency, length.out = periods)

  # New forecast data bundled into an xts object
  new_data <- xts(mean_forecast, order.by= new_dates)

  invisible(new_data)
}