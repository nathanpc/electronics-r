#' Capacitors charging and discharging.
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/RC_time_constant}
#' \url{http://en.wikipedia.org/wiki/Capacitor}
#'
#' @author Nathan Campos \email{nathanpc@@dreamintech.net}

#' Calculates the voltage in a capacitor at a exact time while it's charging.
#' 
#' @param t Time (Seconds).
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @return Voltage in the capacitor at the time \eqn{t}.
capacitor.charge <- function (t, vo, r, c) {
  return(vo * (1 - exp(-(t / (r * c)))))
}

#' Calculates the voltage in a capacitor at a exact time while it's discharging.
#' 
#' @param t Time (Seconds).
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @return Voltage in the capacitor at the time \eqn{t}.
capacitor.discharge <- function (t, vo, r, c) {
  return(vo * exp(-(t / (r * c))))
}

#' Retrieve a vector with the voltages in a capacitor during its entire
#' charge cycle.
#' 
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @param steps Time steps (Seconds).
#' @return A vector with the voltages in the capacitor.
capacitor.charge.values <- function (vo, r, c, steps = 1) {
  values <- c()
  i <- 0
  vt <- 0

  while (vt < vo) {
    vt <- capacitor.charge(i, vo, r, c)
    values <- c(values, vt)
    
    i = i + steps
  }

  return(values)
}

#' Retrieve a vector with the voltages in a capacitor during its entire
#' discharge cycle.
#' 
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @param steps Time steps (Seconds).
#' @return A vector with the voltages in the capacitor.
capacitor.discharge.values <- function (vo, r, c, steps = 1) {
  values <- c()
  i <- 0
  vt <- 0
  
  while (vt < vo) {
    vt <- capacitor.discharge(i, vo, r, c)
    values <- c(values, vt)
    
    i = i + steps
  }
  
  return(values)
}

#' Plot the capacitor charging.
#' 
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @param steps Time steps (Seconds).
capacitor.charge.plot <- function (vo, r, c, steps = 1) {
  # TODO: Use ggplot2.
  plot.default(capacitor.charge.values(vo, r, c, steps), type="l", col="blue")
}

#' Plot the capacitor discharging.
#' 
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @param steps Time steps (Seconds).
capacitor.discharge.plot <- function (vo, r, c, steps = 1) {
  # TODO: Use ggplot2.
  plot.default(capacitor.discharge.values(vo, r, c, steps), type="l", col="blue")
}

#' Calculates how much time it takes to charge a capacitor.
#' 
#' @param vo Initial voltage (Volts).
#' @param r Resistance (Ohms).
#' @param c Capacitance (Farads).
#' @return Time to charge the capacitor (Seconds).
capacitor.charge.duration <- function (vo, r, c) {
  return(vo / ((vo / r) / c));
}