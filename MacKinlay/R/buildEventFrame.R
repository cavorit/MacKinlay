#' @title buildEventFrame
#' @author Harald Fiedler
#' @details Eine interne Hilfsfunktion von analysiereFirmen(). 
#' @description Hilfsfuntkion.
#' @param Es werden lediglich Variabeln aus dem Wrapper durchgereicht.
#' @return Ein data.frame. Reduziert den Datensatz KURS auf die in ISIN indizierten Firmen. Dazu kommen noch boolesche Zugriffsschlüssel und ein zeitlicher Index.


buildEventFrame <- function(eventdate, Puffer, left, right, L_est, ISIN, KURS){
  Tau <- 1:nrow(KURS)-eventdate
  estimationWindow <- rep(FALSE, times=nrow(KURS))
  estimationWindow[(eventdate+left-L_est-Puffer):(eventdate+left-1-Puffer)] <- TRUE
  eventWindow <- rep(FALSE, times=nrow(KURS))
  eventWindow[(eventdate+left):(eventdate+right)] <- TRUE
  Market <- KURS$SXXP
  Firmen <- KURS[ , is.element(names(KURS), ISIN)  ]
  EventFrame <- data.frame(Tau, estimationWindow, eventWindow, Market, Firmen)
  return(EventFrame)
}
