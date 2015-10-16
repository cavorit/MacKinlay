#' @author Harald Fiedler
#' @description Diese Funktion analysiert die Daten und liefert Ergebnisse auf Ebene der individuellen Firmen.
#' @details Diese Funktion ist das Arbeitspferd des Pakets. Sie nimmt die Daten und berechnet ein Marktmodell, und gibt darauf basierend die abnormal Returns und andere Kennwerte aus. Die Analyse beschränkt sich dabei vollständig auf individuelle Firmen, führt also keinerlei Aggregierung der Ergebnisse durch. Für die Aggreggierung siehe analysiereMarkt().
#' @param Gruppe character-Vektor der Länge 1 aus c("A", "B", "C", "AandB", "AxorB"). Gibt an, welche Firmengruppe untersucht werden soll. "A" und "B" haben unterschiedlichen Umgang mit Pensionsrisiken. "C" hat keine Pensionen und ist eine Art Kontrollgruppe. Da manche Firmen über mehrere Jahe teilweise Strategie zwischen "A" und "B" gewechselt haben, ist "AandB" und "AxorB" evtl. acuh von Interesse.
#' @param eventdate eine Zahl aus c(260, 555, 646, 809, 958), die angibt, am wievielten Handelstag ein besonderes Event stattgefunden hat. 
#' @param Puffer numeric der Länge 1, üblicherweise aus c(0,50). Führ ggf. einen Space zwischen dem estimation Window und dem event Window ein. Defualt ist 50.
#' @param left gibt an, wie viele Tage vor dem Event das Event-Window beginnen soll.
#' @param right gibt an, wie viele Tage nach dem Event das Event-Window beginnen soll. Übliche Windows sind [-5, 5], [-1,1], aber auch [-3, 3]
#' @param L_est eine Zahl, die angibt, wie lange das Estimation-Window sein soll. Default ist 200.
#' @param alpha Das Konfidenzniveau, default ist 1%
#' @param inProzent Boolean der Länge 1, das angibt, ob die Aktienkurse in prozentualen Veränderungen (TRUE) oder in absoluten Beträgen untersucht werden sollen.
#' @title analysiereFirmen
#' @return Eine Liste von zwei Listen. Die Liste AR_it enthält die AR_it, die Konfidenzgrenzen oben, die Konfidenzgrenzen unten und die Schätzer für die Residuenvarianz. Die zweite Liste das gleiche für die Kummulanten.

analysiereFirmen <- function(Gruppe, eventdate, Puffer=50, left=-5, right=5, L_est=200, alpha=.01, inProzent=TRUE){
  loadSulzbachDaten(Gruppe=Gruppe, inProzent = inProzent)
  L_win = right + abs(left) + 1 # lenght event window
  EventFrame <- buildEventFrame(eventdate = eventdate, Puffer = Puffer, left = left, right = right, L_est = L_est, ISIN=ISIN, KURS=KURS)
  
  # Abnormal returns for individual firms 
  AR_it <- apply(EventFrame[, 5:ncol(EventFrame)], 2, calc_AR_it, 
                 # Params
                 estimationWindow = EventFrame$estimationWindow, 
                 eventWindow = EventFrame$eventWindow,
                 Market = EventFrame$Market)
  #AR_it hat Erwartungswert Null und unter der H_0 die geschätzte Varianz:
  varHat_epislon_i <- apply(EventFrame[, 5:ncol(EventFrame)], 2, calc_varHat_epsilon_it,
                            #Params
                            estimationWindow = EventFrame$estimationWindow, 
                            eventWindow = EventFrame$eventWindow,
                            Market = EventFrame$Market)
  #Schätzer basiert auf EstimationWindow
  critical_values_left <- qnorm(alpha, sd=sqrt(varHat_epislon_i), mean=0)
  critical_values_right <- qnorm(1-alpha, sd=sqrt(varHat_epislon_i), mean=0)
  
  ergebnisAR_it <- list(AR_it = AR_it, 
                        KIunten = critical_values_left,
                        KIoben = critical_values_right,
                        varHat_epislon_i = varHat_epislon_i
  )
  
  # Cummulated Abnormal returns for individual firms
  CAR_it <- apply(AR_it, 2, cumsum)
  #CAR_it hat Erwartungswert Null und unter der H_0 gilt, 
  # dass die Varianz die Summe der einzelnen, konstanten Var(e_i) ist, also
  # [Anzahl der Tage] x varHat_epislon_i (Schätzer)
  varHat_CAR_it <- outer(1:L_win, varHat_epislon_i, "*")
  critical_values_left_CAR_it <- qnorm(alpha, sd=sqrt(varHat_CAR_it), mean=0)
  critical_values_right_CAR_it <- qnorm(1-alpha, sd=sqrt(varHat_CAR_it), mean=0)
  # CAR_it>critical_values_right # zeigt, welche Firmen an welchen Tagen die KI nach oben brechen  
  
  ergebnisCAR_it <- list(CAR_it = CAR_it,
                         KIunten = critical_values_left_CAR_it,
                         KIoben = critical_values_right_CAR_it,
                         varHat_CAR_it = outer(1:L_win, varHat_epislon_i, "*"))   
  ergebnis <- list("AR_it"=ergebnisAR_it, "CAR_it"=ergebnisCAR_it)
  
  return(ergebnis)
}
