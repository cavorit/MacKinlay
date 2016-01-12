#' @author Harald Fiedler
#' @title EventPlot
#' @description In Anlehnung an Fried ein Konfidenzband und dazu die CARs.
#' @param abnormals (vermutlich) ein numeric array mit abnormal returns, die gezeichnet werden sollen.
#' @param KIleft die zugehörigen unteren KI-Schranken
#' @param KIright die zugehörigen oberen kritischen Werte
#' @param left gibt an, wie viele Tage vor dem kritischen Event das Fenster anfangen soll. Dieser Wert wird tatsöchlich nur genutzt, um eine Vertikale zu zeichnen, die den DDay markiert.
EventPlot <- function(abnormals, KIleft, KIright, left){
  T_ <- length(abnormals)
  plot(1:T_, abnormals, type='o', pch=4,  xlab="Tage", ylab="CAR", ylim= c(min(abnormals, KIleft, KIright), max(abnormals, KIleft, KIright)), col="blue")
  lines(1:T_, KIleft, col="green")
  lines(1:T_, KIright, col="green")
  abline(v=(left), col="blue")
}
