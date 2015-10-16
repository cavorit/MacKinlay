#' @author Harald Fiedler
#' @description interne Funktion von analysiereFirmen()
#' @details Der Name ist selbstredend
#' @param Wird aus dem Wrapper durchgereicht
#' @return So wie ich das lese ist das wohl ein array von numerics
#' @title calc_varHat_epsilon_it
calc_varHat_epsilon_it <- function(Firma, 
                                   estimationWindow, 
                                   eventWindow,
                                   Market){
  fit <- lm(Firma[estimationWindow]~Market[estimationWindow])
  return(var(fit$residuals))
}  

