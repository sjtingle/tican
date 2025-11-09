#' Plot and analyse time-intensity data
#'
#' This function plots and analyzes time-intensity data, such as data from (contrast-enhanced)
#' ultrasound cineloops. Peak intensity and time to peak intensity are calculated from a smoothed
#' curve through the data (loess smoother). Area under the curve is calculated from the raw data
#' using the trapezium method for integration. Time to peak proportion (for example time to
#' 90 percent of peak) can also be calculated. If selected, WiR (wash in rate) is the maximum upslope before
#' the peak value. If selected, WoR (wash out rate) is the absolute value of the maximum downslope following
#' the peak value (with larger values representing higher wash out rate). Both WiR and WoR are calculated
#' based on the slope of the LOESS curve, as they are sensitive to noise in the raw data.
#'
#' Blue dashed lines represent peak and time to peak values. If selected, solid purple lines
#' represent WiR and WoR, and dashed green lines represent time to peak proportion.
#'
#' A plot of the data is generated and a dataframe with the results is returned.
#'
#' @importFrom graphics plot abline lines title
#' @importFrom stats loess
#'
#' @param data A dataframe with time and intensity values as columns.
#' @param timevar A character string (in quotes) with the dataframe column name for the time variable.
#' @param intensityvar A character string (in quotes) with dataframe column name for the intensity variable.
#' @param loess.span A number between 0 and 1, with larger values resulting in a smoother curve.
#' @param AUCmax A number - the maximum time that area under the curve is measured until.
#' @param peakproportion A number between 0 and 1 which is used in the time to peak proportion calculations.
#' @param plotresult TRUE or FALSE to determine whether a plot of the results is generated.
#' @param calc_wir TRUE or FALSE to determine whether WiR is calculated.
#' @param calc_wor TRUE or FALSE to determine whether WoR is calculated.
#' @param ... Additional arguments to be passed into the loess() function.
#'
#' @return A dataframe with the results. Depending on the plotresult argument can also
#' return a plot of the smoothed curve.
#' @export
#'
#' @examples
#'
#' # Example usage: Please see package vignettes on CRAN
#'

tic_analyse <- function(data,
                             timevar,
                             intensityvar,
                             loess.span=0.1,
                             AUCmax=NULL,
                             peakproportion=NULL,
                             calc_wir = FALSE,
                             calc_wor = FALSE,
                             plotresult=TRUE,
                             ...){ # ... allows any loess function arguments to be passed in

  # Check if specified columns exist in dataframe
  if(!(timevar %in% names(data))) {
    stop("Specified timevar not found in the dataframe")
  }

  if(!(intensityvar %in% names(data))) {
    stop("Specified intensityvar not found in the dataframe")
  }

  # Check if loess.span is numeric and within valid range (0, 1]
  if(!is.numeric(loess.span) || loess.span <= 0 || loess.span > 1) {
    stop("loess.span must be numeric and between 0 and 1")
  }

  # Check if peakproportion is numeric and between 0 and 1
  if(!is.null(peakproportion) && (!is.numeric(peakproportion) || peakproportion < 0 || peakproportion > 1)) {
    stop("peakproportion must be numeric and between 0 and 1")
  }

  # Check if AUCmax is numeric and positive
  if(!is.null(AUCmax) && (!is.numeric(AUCmax) || AUCmax <= 0)) {
    stop("AUCmax must be numeric and positive")
  }

  # pulling the data
  x <- data[[timevar]]
  y <- data[[intensityvar]]

  # Check if the columns are numeric
  if(!is.numeric(x) || !is.numeric(y)) {
    stop("The time variable and intensity variable must be numeric")
  }

  # Check for duplicate x values
  if(any(duplicated(x))) {
    stop("Duplicate time values detected. Please ensure the time variable has unique values.")
  }

  # make sure all in time order
  o <- order(x)
  x <- x[o]
  y <- y[o]

  # Fit loess smoother
  smoothed <- loess(y ~ x,
                    span = loess.span,# span is how closely the smoother fits the data
                    ...) # ... allows any loess functions to be passed in

  yfit <- smoothed$fitted # get a list of the predicted values (in essence the loess curve)

  #get loess curve values
  Peak_intensity <- max(yfit)
  Time_to_peak <- x[which(yfit == Peak_intensity)[1]]

  if(!is.null(peakproportion)){
    # Calculate the peak intensity proportion
    Peak_intensity_proportion <- Peak_intensity * peakproportion
    # calculates the smallest x value for which the loess curve crosses above
    # the proportional peak
    Time_to_peak_proportion <- x[which(yfit > Peak_intensity_proportion)[1]]
  }

  if(calc_wir){
    # for wash in rate:
    x_wir <- x[x <= Time_to_peak]
    yfit_wir <- yfit[x <= Time_to_peak]
    slopes_wir <- diff(yfit_wir) / diff(x_wir)
    wir <- max(slopes_wir)
    time_of_wir <- x_wir[which(slopes_wir == wir)[1] + 1]
  }

  if(calc_wor){
    x_wor <- x[x >= Time_to_peak]
    yfit_wor <- yfit[x >= Time_to_peak]
    slopes_wor <- diff(yfit_wor) / diff(x_wor)
    wor <- min(slopes_wor)
    time_of_wor <- x_wor[which(slopes_wor == wor)[1] + 1]
  }

  if(plotresult==TRUE){
    # Plotting
    plot(y ~ x, pch=19,cex=1.5, xlab=timevar, ylab=intensityvar) #plot actual points
    lines(x,yfit,col="red",lwd=3) #plot the loess curve
    abline(h = Peak_intensity, col = "blue", lty = 2, lwd=3)
    abline(v = Time_to_peak, col = "blue", lty = 2, lwd=3)

    if(!is.null(peakproportion)){
      abline(h = Peak_intensity_proportion, col = "darkgreen", lty = 2, lwd=3)
      abline(v = Time_to_peak_proportion, col = "darkgreen", lty = 2, lwd=3)
    }
    if(calc_wir){
      abline(a = yfit[which(x == time_of_wir)] - wir * time_of_wir,
             b = wir, col = "purple", lwd = 3)
    }

    if(calc_wor){
      abline(a = yfit[which(x == time_of_wor)] - wor * time_of_wor,
             b = wor, col = "purple", lwd = 3)
    }

    title(paste(timevar,intensityvar,sep=" - "))
  }

  #AUC is from the actual data, not loess curve, so is the actual AUC
  if(!is.null(AUCmax)){
    AUC_indices <- which(x < AUCmax)
  } else {
    AUC_indices <- 1:length(x)
  }

  x_AUC <- x[AUC_indices]
  y_AUC <- y[AUC_indices]
  AUC <- sum(diff(x_AUC) * (y_AUC[-1] + y_AUC[-length(y_AUC)]) / 2)

  # Creating dataframe

  data_name <- deparse(substitute(data))
  df <- data.frame(
    data = data_name,
    Peak_intensity = Peak_intensity,
    Time_to_peak = Time_to_peak,
    AUC = AUC
  )

  if(!is.null(peakproportion)){
    df$Time_to_peak_proportion <- Time_to_peak_proportion
  }
  if(calc_wir){
    df$WiR <- wir
  }
  if(calc_wor){
    df$WoR <- abs(wor)
  }

  return(df)
}
