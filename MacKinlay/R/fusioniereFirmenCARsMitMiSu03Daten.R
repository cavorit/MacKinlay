#' @author Harald Fielder
#' @description fügt den SU03Daten die CAR hinzu
#' @details Die CAR_it werden an die SU03Daten gemerged mit t=letzter Tag des betrachteten EventWindows
#' @param Firmenanalyse list die aus analysiereFirmen() gebildet wird 
#' @param SU03Daten data.frame das man mit lese2016Q1MiSu03Daten(Event=eventdate) erhält
#' @title fusioniereFirmenCARsMitMiSu03Daten

fusioniereFirmenCARsMitMiSu03Daten <- function(Firmenanalyse, MiSu03Daten){
  
  CAR_iEnde <- Firmenanalyse$CAR_it$CAR_it[nrow(Firmenanalyse$CAR_it$CAR_it),]
  Y <- data.frame(ISIN=names(CAR_iEnde), CAR_iEnde = CAR_iEnde)
  rownames(Y) <- NULL
  
  DF <- merge(Y, MiSu03Daten)
  return(DF)  
}
