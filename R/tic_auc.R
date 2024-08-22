#' Return area under curve from time-intensity data
#'
#' This function returns area under curve from raw time intensity curves using
#' the trapezoid method. It is recommended that plotresult is set to TRUE in the
#' first instance to visually confirm the analysis.
#'
#' A plot of the data is generated and a dataframe with the results is returned.
#'
#' @importFrom graphics abline lines title
#' @importFrom stats loess
#'
#' @param data A dataframe with time and intensity values as columns.
#' @param timevar A character string (in quotes) with the dataframe column name for the time variable.
#' @param intensityvar A character string (in quotes) with dataframe column name for the intensity variable.
#' @param AUCmax A number - the maximum time that area under the curve is measured until.
#' @param plotresult TRUE or FALSE to determine whether a plot of the results is generated.
#'
#' @return Area under the curve. Depending on the plotresult argument can also
#' return a plot of the raw data.
#' @export
#'
#' @examples
#'
#' # Example usage:
#'
#' # Generating simulated data
#' set.seed(123)
#' example_data <- data.frame(time = seq(0, 82, by = 0.25))
#' random_vals <- sample(1:10, nrow(example_data), replace = TRUE)
#' example_data$regionA_intensity <- log(example_data$time + 1) * 50 -
#'   example_data$time * 2 + random_vals
#' example_data$regionB_intensity <- log(example_data$time + 7, base = 10) *
#'   80 - example_data$time * 1.5 + random_vals
#'
#' # Example with defaults:
#'
#' tic_auc(data = example_data, timevar = "time", intensityvar = "regionA_intensity")
#'
#' # Example with additional arguments:
#'
#' tic_auc(data = example_data, timevar = "time", intensityvar = "regionA_intensity",
#'                    AUCmax = 30, plotresult = TRUE)
#'
#'

tic_auc <- function(data,
                             timevar,
                             intensityvar,
                             AUCmax=NULL,
                             plotresult=FALSE){

  # Check if specified columns exist in dataframe
  if(!(timevar %in% names(data))) {
    stop("Specified timevar not found in the dataframe")
  }

  if(!(intensityvar %in% names(data))) {
    stop("Specified intensityvar not found in the dataframe")
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

  if(plotresult==TRUE){
    # Plotting
    j <- order(x) #for plotting
    plot(y ~ x, pch=19,cex=1.5, xlab=timevar, ylab=intensityvar) #plot actual points
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

  return(AUC)
}
