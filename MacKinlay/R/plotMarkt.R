#' @author Harald FIedler
#' @title plotMarkt
#' @description Wrapper für EventPlot.
#' @details Plottet den Markt in seinem  Konfidenzband

plotMarkt <- function(Marktanalyse, DDay=5){
  # Marktanalyse ist ein Objekt, das von analysiereMarkt() erzeugt wird.
  CAR_quer_t = Marktanalyse$CAR_quer_t
  critical_values_left_CAR_quer_t = Marktanalyse$critical_values_left_CAR_quer_t
  critical_values_right_CAR_quer_t = Marktanalyse$critical_values_right_CAR_quer_t
  left = DDay
  
  EventPlot(CAR_quer_t, critical_values_left_CAR_quer_t, critical_values_right_CAR_quer_t, left=left)
}