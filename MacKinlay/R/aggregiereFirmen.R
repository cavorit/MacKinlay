#' @author Harald Fiedler
#' @description Aggregiert die Ergebnisse auf Firmenebene
#' @details Aggregiert die Ergebnisse, die per analysiereFirmen() generiert werdne
#' @title aggregiereFirmen
#' @param Firmenanalyse Eine Liste, die mit analysiereFirmen() erzeugt wurde.
#' @return Eine Liste mit Statistiken und Teststatistiken. AR_quer_t, varHat_AR_quer_t, CAR_quer, varHat_Car_quuer_t sowie zwei Werte für die Konfidenzbänder

aggregiereFirmen <- function(Firmenanalyse, alpha){
  # Firmenananlyse ist ein Objekt, das von analysiereFirmen() erzeugt wird.
  AR_it <- Firmenanalyse[[1]][[1]]
  varHat_epislon_i <- Firmenanalyse[[1]][[4]] 
  
  #Doing aggregation of abnormal return over firms (p.24)
  
  # AR_quer_t
  AR_quer_t <- apply(AR_it, 1, mean)
  
  #varHat_AR_quer_t
  varHat_AR_quer_t <- (1/ncol(AR_it)^2) * sum(varHat_epislon_i)
  
  # Aggregating of abnormal returns over firms and then over time
  CAR_quer_t <- cumsum(AR_quer_t)
  #CAR_quer_t
  varHat_CAR_quer_t <- 1:length(CAR_quer_t) * (1/ncol(AR_it)^2) * sum(varHat_epislon_i) # Entspricht Formel (14) in (16) eingesetzt
  
  #varHat_CAR_quer_t
  critical_values_left_CAR_quer_t <-  qnorm(alpha, sd=sqrt(varHat_CAR_quer_t), mean=0) 
  critical_values_right_CAR_quer_t <-  qnorm(1-alpha, sd=sqrt(varHat_CAR_quer_t), mean=0) 
  
  ergebnis <- list(AR_quer_t=AR_quer_t, 
                   varHat_AR_quer_t=varHat_AR_quer_t,
                   CAR_quer_t=CAR_quer_t,
                   varHat_CAR_quer_t = varHat_CAR_quer_t,
                   critical_values_left_CAR_quer_t=critical_values_left_CAR_quer_t,
                   critical_values_right_CAR_quer_t=critical_values_right_CAR_quer_t)
  
  return(ergebnis)
}
