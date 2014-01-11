#' Contains functions related to electronic filters.
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Electronic_filter}
#'
#' @author Nathan Campos \email{nathanpc@@dreamintech.net}

#' Calculates the cutoff frequency of a RC low-pass filter.
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Low-pass_filter}
#'
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @return Cutoff frequency (Hertz).
filter.rc.lowpass <- function (r, c) {
  return(1 / (2 * pi * r * c))
}

#' Calculates resistor value for a low-pass filter.
#' 
#' @param f Frequency (Hertz).
#' @param c Capacitance (Farads).
#' @return Resistance (Ohms).
filter.rc.lowpass.r <- function (f, c) {
  return(1 / (2 * pi * c * f))
}

#' Calculates capacitor value for a low-pass filter.
#' 
#' @param f Frequency (Hertz).
#' @param r Resistance (Ohms).
#' @return Capacitance (Farads).
filter.rc.lowpass.c <- function (f, r) {
  return(1 / (2 * pi * r * f))
}