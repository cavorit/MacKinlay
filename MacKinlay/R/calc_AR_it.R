#' @author Harald Fiedler
#' @description Interne Hilfsfunktion von analysiereFrimen()
#' @details Berechnet die abnormale Returns pro Firma und Tag.
#' @param Werden aus dem Wrapper durchgereicht.
#' @return Ein data.frame() wie in buildEventFrame(), aber in den Werten um den bedingten Erwartungswert bereinigt, also die Residuen enthaltend.
#' @title calc_AR_it
calc_AR_it <- function(Firma, 
                       estimationWindow, 
                       eventWindow,
                       Market){
  # Calculation of individual firms abnormal return
  fit <- lm(Firma[estimationWindow]~Market[estimationWindow])
  alpha <- fit$coefficients[1]
  beta <- fit$coefficients[2]
  MarketPrediction <- alpha + beta * Market[eventWindow]
  AbnormalReturn <- Firma[eventWindow] - MarketPrediction
  return(AbnormalReturn)
}


