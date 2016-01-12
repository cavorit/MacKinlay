#'
#' @author Harald Fiedler
#' @description Stellt Datensätze bereit
#' @details Stellt unter ISIN die zu untersuchenden Firmenindex bereit und unter KURS alle Aktienkurse
#' @title loadSulbachDaten
#' @param Gruppe String der Länge 1. Entweder "A", "B", "C", "AandB", oder "AxorB". Es handelt sich dabei um Firmengruppen, die unterschiedlich mit Pensionsrisiken umgehen.
#' @param inProzent Boolean der Länge 1. Gibt an, ob die Analyse auf Basis absoluter Aktienkurse stattfindet, oder in relaitiven Prozentänderungen zum ersten Tag des Event-Windows  
#' 
loadSulzbachDaten <- function(Gruppe, inProzent){
  message("ISIN und KURS im Hauptspeicher")
  if (Gruppe=="A"){ISIN <<- ISIN_A}
  if (Gruppe=="B"){ISIN <<- ISIN_B}
  if (Gruppe=="C"){ISIN <<- ISIN_C}
  if (Gruppe=="AandB"){ISIN <<- ISIN_AandB}
  if (Gruppe=="AxorB"){ISIN <<- ISIN_AorB}  
  ifelse(inProzent, KURS <<- KURS2[, c(1:207, 209:586)], KURS <<- KURS1) # Firma 208 macht Probleme wegen fehlender Startwerte
  #rownames(KURS) <- 1:nrow(KURS)
  return(NULL)
}