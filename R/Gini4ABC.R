Gini4ABC <- function(p, ABC){
# Gini = Gini4ABC(p,ABC)
# Gini index for an ABC curve
# 
# INPUT
# p,ABC              x/y coorninates of ABC curve ABC(p), p(end) == 1 or 100;
  # 
  # OUTPUT
  # Gini              gini index i.e. the integral over ABC(p) / 0.5  *100
  #                   given in percent i.e in [0..100]
  trapz <- function(x,y){
    idx = 2:length(x)
    return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
  }
  
  # prozentuieren
  p = p / tail(p,1)
  ABC = ABC / tail(ABC,1)
  
  # Flaeche unter ABC
  Area = trapz(p, ABC)
  
  Gini = Area*200 - 100
  return(Gini)
}