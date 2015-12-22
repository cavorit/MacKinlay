rm(list=ls())
library(MacKinlay)

Gruppe = "A"
Puffer = 50
eventdate = 555
left = -5
right = 5
L_est = 200
alpha = .05
inProzent = TRUE

Firmenanalyse <- analysiereFirmen(
                    Gruppe = Gruppe, 
                    Puffer = Puffer, # Space zwischen EstimWindow und EwentWindow 
                    eventdate = eventdate, 
                    left = left, 
                    right = right, 
                    L_est = L_est, 
                    alpha = alpha, 
                    inProzent = inProzent
                    # es werden in analysiereFirmen() nacheinander abgearbeitet
                    # * loadSulzbachDaten()
                    # * buildEventFrame()
                    # * calc_AR_it() via apply aufgerufen
                    # * calc_varHat_epsilon_it() via apply
                )
Firmenanalyse
str(Firmenanalyse)
Firmenanalyse$CAR_it$CAR_it


DDay = abs(left)
plotFirma(
          Firmenanalyse = Firmenanalyse, 
          FirmaISIN = "FI0009010391", 
          DDay=DDay
          )

Marktanalyse <- aggregiereFirmen(Firmenanalyse = Firmenanalyse, 
                                 alpha = alpha)
Marktanalyse
str(Marktanalyse)
Marktanalyse$AR_quer_t
plotMarkt(Marktanalyse = Marktanalyse,
          DDay = DDay)
MacKinlayTable(Marktanalyse = Marktanalyse, LaTeX = FALSE)

