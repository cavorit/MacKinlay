#' @author Harald Fiedler
#' @title lese2016Q1MiSu03Daten
#' @details Lädt die Daten ein als interne Daten
#' @description Lädt die Daten ein
#' @param Event numeric der Länge 1 aus c(260, 555, 646, 809, 958)
#' @return data.frame
#' 
#' 

lese2016Q1MiSu03Daten <- function(Event){
  if (!is.element(Event, c(260, 555, 646, 809, 958))){
    stop("Achtung, gültige Events sind aus c(260, 555, 646, 809, 958) zu nehmen")
  }
  
  Pfad <- system.file("extdata", package = "MacKinlay", paste0("Event", Event, ".csv"))
  DF <- read.csv2(file=Pfad, header=TRUE, sep=";")
  
  if (Event==260){
    message("Ich repariere falschen Dezimalpunkt im Eventblatt 260")
    Spalte <- as.character(DF$MV_common_shares) # z.B. [129] hat kein Dezimalzeichen 
    Spalte <- strsplit(Spalte, ",")
    Spalte[[129]] <- c(Spalte[[129]], "0")
    Spalte[[188]] <- c(Spalte[[188]], "0")
    Spalte[[174]] <- c(Spalte[[174]], "0")
    Spalte[[210]] <- c(Spalte[[210]], "0")
    Spalte[[320]] <- c(Spalte[[320]], "0")      
    Spalte[[330]] <- c(Spalte[[330]], "0")
    Spalte[[464]] <- c(Spalte[[464]], "0")
    Spalte[[471]] <- c(Spalte[[471]], "0")
    Spalte[[480]] <- c(Spalte[[480]], "0")
    Spalte[[484]] <- c(Spalte[[484]], "0")
    Spalte[[543]] <- c(Spalte[[543]], "0")
    Spalte[[519]] <- c("1674", "7") # harter Typo im Original-File
    Spalte[[565]] <- c("1024", "2") # harter Typo im Original-File
    Spalte <- lapply(Spalte, FUN=paste0, collapse=".")
    Spalte <- unlist(Spalte)
    Spalte <- as.numeric(Spalte)
    DF$MV_common_shares <- Spalte
  }
    
  return(DF)
}

