#' @author Harald FIedler
#' @title plotMarkt
#' @description Wrapper für EventPlot.
#' @details Plottet den Markt in seinem  Konfidenzband

plotMarkt <- function(Marktanalyse, DDay){
  # Marktanalyse ist ein Objekt, das von analysiereMarkt() erzeugt wird.
  CAR_quer_t <- Marktanalyse$CAR_quer_t
  critical_values_left_CAR_quer_t <- Marktanalyse$critical_values_left_CAR_quer_t
  critical_values_right_CAR_quer_t <- Marktanalyse$critical_values_right_CAR_quer_t
  
  
  EventPlot(abnormals = CAR_quer_t, 
            KIright = critical_values_left_CAR_quer_t, 
            KIleft = critical_values_right_CAR_quer_t, 
            left=DDay+1)
}