#' @author Harald FIedler
#' @title plotFirma
#' @description Wrapper für EventPlot.
#' @details Plottet einzelne Firmen in ihren individuellen Konfidenzbänder

plotFirma <- function(Firmenanalyse, FirmaISIN="FI0009010391", DDay = 5){
  # Firmenanalyse ist ein Objekt was von analysiereFirmen() erzeugt wird
  CAR_it <- Firmenanalyse[[2]][[1]]
  critical_values_left_CAR_it <- Firmenanalyse[[2]][[2]]
  critical_values_right_CAR_it <- Firmenanalyse[[2]][[3]]
  EventPlot(CAR_it[, "FI0009010391"], 
            critical_values_left_CAR_it[, FirmaISIN], 
            critical_values_right_CAR_it[ , FirmaISIN], 
            left=(-1)*DDay)
}