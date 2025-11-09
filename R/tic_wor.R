#' Return wash out rate from time-intensity data
#'
#' This function returns the wash out rate from time-intensity curve data. Raw data
#' is smoothed using a loess smoother. Wash out rate is calculated as the absolute value of the most negative
#' slope of the loess curve after peak intensity is reached. A higher number
#' therefore refers to faster washout. It is recommended that
#' plotresult is set to TRUE in the first instance to visually confirm the analysis.
#'
#' @importFrom graphics plot abline lines title
#' @importFrom stats loess
#'
#' @param data A dataframe with time and intensity values as columns.
#' @param timevar A character string (in quotes) with the dataframe column name for the time variable.
#' @param intensityvar A character string (in quotes) with dataframe column name for the intensity variable.
#' @param loess.span A number between 0 and 1, with larger values resulting in a smoother curve.
#' @param plotresult TRUE or FALSE to determine whether a plot of the results is generated.
#' @param ... Additional arguments to be passed into the loess() function.
#'
#' @return The wash out rate (negative slope). If plotresult=TRUE also draws a plot.
#' @export
#'
#' @examples
#'
#' # Example usage: Please see package vignettes on CRAN
#'


tic_wor <- function(data,
                    timevar,
                    intensityvar,
                    loess.span = 0.1,
                    plotresult = FALSE,
                    ...) {

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

  # keep only after the peak
  x_cut <- x[x >= Time_to_peak]
  yfit_cut <- yfit[x >= Time_to_peak]

  # slopes after peak
  slopes <- diff(yfit_cut) / diff(x_cut)

  # most negative slope (steepest down)
  wor <- min(slopes)

  if(!plotresult) return(abs(wor))

  # time of wash-out slope (right endpoint of the segment)
  time_of_wor <- x_cut[which(slopes == wor)[1] + 1]

  # Plotting
  plot(y ~ x, pch=19,cex=1.5, xlab=timevar, ylab=intensityvar) #plot actual points
  lines(x,yfit,col="red",lwd=3) #plot the loess curve
  abline(h = Peak_intensity, col = "blue", lty = 2, lwd=3)
  abline(v = Time_to_peak, col = "blue", lty = 2, lwd=3)
  # purple line to show WoR
  abline(a = yfit[which(x == time_of_wor)] - wor * time_of_wor,
         b = wor, col = "purple", lwd = 3)
  title(paste(timevar, intensityvar, sep = " - "))

  return(abs(wor))
}
