rm(list=ls())
library(MacKinlay)

Gruppe = "AandB"
Puffer = 50
eventdate = c(260, 555, 646, 809, 958)[1]
left = -5
right = 5
L_est = 200
alpha = .05
inProzent = TRUE

#analysiereFirmen() step-by-step
# loadSulzbachDaten(Gruppe=Gruppe, inProzent = inProzent)
# L_win = right + abs(left) + 1
# EventFrame <- buildEventFrame(eventdate = eventdate, Puffer = Puffer, left = left, right = right, L_est = L_est, ISIN=ISIN, KURS=KURS)
# (EventFrame[255:265, c("Tau", "Market", "CH0038863350")])
# KURS[255:265, c("Date", "SXXP", "CH0038863350")]

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
          #FirmaISIN = ISIN_A[12],
          DDay=DDay
          )

par(mfrow=c(3,4))
for (i in 350:450){try(plotFirma(Firmenanalyse = Firmenanalyse, FirmaISIN = ISIN[i], DDay=DDay)); print(i)}
# 1 , 3, 9, 10, 15, 21, 25, 26, 34, 36, 45, 49, 50, 54, 55, 56, 57, 63, 65, 66, 68, 69
# 75, 77, 83, 85, 86, 87, 90, 
Marktanalyse <- aggregiereFirmen(Firmenanalyse = Firmenanalyse, 
                                 alpha = alpha)
Marktanalyse
str(Marktanalyse)
Marktanalyse$AR_quer_t
plotMarkt(Marktanalyse = Marktanalyse, 
          DDay = DDay)
MacKinlayTable(Marktanalyse = Marktanalyse, LaTeX = FALSE)

#### MiSU03

MiSu03Daten <- lese2016Q1MiSu03Daten(Event=eventdate)
head(MiSu03Daten)
DF <- fusioniereFirmenCARsMitMiSu03Daten(Firmenanalyse = Firmenanalyse, MiSu03Daten = MiSu03Daten)
head(DF)

# H2a:
summary(lm(DF$CAR_iEnde~DF$Corridor_per_TA))

# H2b: 
summary(lm(DF$CAR_iEnde~DF$ROA+DF$TDR+DF$CR+DF$Cap_Turn))
