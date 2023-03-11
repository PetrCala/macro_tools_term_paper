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

#' Check structural integrity of the time series,
#' as well as stationarity, etc.
#' @param time_series - .xts object to analyze
#' @param series_name - name of the series, e.g. CPI
runStructuralChecks <- function(time_series, series_name){
  print(paste0('Running structural checks for ', series_name))

  # Detect structural breaks using the breakpoints function
  # print('Testing for breakpoints...')
  # bp <- breakpoints(time_series ~ 1)

  # # Test the stationarity of the segments between breakpoints
  # statcheck(bp)

  # Run the augmented Dickey-Fuller test
  print(paste0('Running the augmented Dickey-Fuller test for ', series_name, '...'))
  adf_test <- adf.test(time_series, k = 1)
  print(adf_test)
  if(adf_test$p.value < 0.05) {
    print('The m-o-m transformed series is stationary and has zero order of integration')
  } else {
    # Perform ADF test on the first difference of the series
    adf_test_diff <- adf.test(diff(pct_change))
    if(adf_test_diff$p.value < 0.05) {
      print('The m-o-m transformed series is integrated of order 1')
    } else {
      print('The m-o-m transformed series is not stationary')
    }
  }

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