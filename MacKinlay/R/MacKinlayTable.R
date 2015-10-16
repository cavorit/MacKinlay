#' @author Harald Fiedler
#' @description Erstellt Tabelle mit AR und CAR auf aggrtegierter Ebene
#' @title MacKinlayTable
#' @param Marktanalyse ist ein Objekt, das von agggregiereFirmen() erzeugt wurde
#' @param LaTeX boolescher Entschieder, ob xtable genutzt werden soll, oder das Ergebnis als data.frame gegeben werden soll
#' @return xtable-Objekt oder data.frame mit zwei Spalten und Fancy rownames

MacKinlayTable <- function(Marktanalyse, LaTeX=TRUE){
  # Marktanalayse eine liste, die von analysiereMarkt() erezugt wurde
  Signifik <- ( (Marktanalyse$CAR_quer_t > Marktanalyse$critical_values_right_CAR_quer_t) | Marktanalyse$CAR_quer_t < Marktanalyse$critical_values_left_CAR_quer_t)
  DF <- data.frame("AR_quer_t" = Marktanalyse$AR_quer_t, 
                   "CAR_quer_t" = Marktanalyse$CAR_quer_t, 
                   "Signifikanz-Test"= ifelse(Signifik, "*", " ")
  )
  rownames(DF) <- paste("Handelstag", rownames(DF))
  ifelse(LaTeX, return(xtable(DF, digits = 7)), return(DF))
}