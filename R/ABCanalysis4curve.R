ABCanalysis4curve <- function(p, ABC, PlotIt){
  # V = ABCanalysis4curve(p,ABC,PlotIt)
  # calculate points A B C   bei gegebener ABC kurve
  # 
  #  INPUT
  #  [p,ABC]         sind die  Werte der ABC curve
  # 
  #  OPTIONAL
  #  PlotIt              ein plot der ABC Kurve mit errechneten Punkten
  # 
  #  OUTPUT
  # 
  #  [ABx,ABy]           coordinates of limiting point between set A and set B     (Pareto Point)
  #  [BCx,BCy]           coordinates of limiting point between set B and set C (Submarginal Point)
  #  [Bx,By]             B Point: dABC(Bx) == 1                                 (BreakEven Point)
  # berechne A B C Punkte. ABC Kurve ist gegeben (muss absteigend sortiert sein)
  
	
	EmpiricDeviation <- function(p, value){
  # calculates the deviations at positions p. A function graph p vs value is expected.
  # deviations = EmpiricDeviation(p, value)
  # INPUT
  # p       positions at which values are given, and deviations will be calculated
  # value   values at the positions in p
  # OUTPUT
  # deviations  calculated deviations for the positions at p
  # Author: FL
  
  dp = c()
  dvalue = c()
  for(i in 1:(length(value)-1)){
    dp[i] = p[i+1] - p[i]
    dvalue[i] = value[i+1] - value[i]
  }
  
  return(dp/dvalue)
}

  Effort = p
  Yield = ABC
  
  # Pareto Punkt: kleinster Abstand (0,1)
  ParetoPunktIndex = which.min(p^2 + ((1-ABC)^2))
  
  

  
  # BreakEven Punkt: Ableitung = 1
  dABC =  EmpiricDeviation(Effort,Yield)
  minValue = min(abs(dABC-1));
  BreakEvenPunktIndex = tail(which(abs(dABC-1) ==minValue),1)
  
  # Punkt AB
  AB = min(ParetoPunktIndex, BreakEvenPunktIndex)
  # Punkt zwischen AB und BC
  B = max(ParetoPunktIndex, BreakEvenPunktIndex)
  
  # Submarginal Punkt: minimaler Abstand obere Kante ueber AB
  SubmarginalPunktIndex = which.min((Effort-Effort[AB])^2 + (1- Yield)^2)
  
  # Punkt BC
  BC = SubmarginalPunktIndex
  
  if(PlotIt){
    ylab='fraction of sum of largest data'
    xlab='fraction of data'
    title='ABC Analysis'
    farb.col=c('blue',colors()[452],'green',colors()[175])
    farb.labels <- c(expression(italic("data")),expression(italic("identity")),expression(italic("uniform")),'')


    # der eigentliche plot
    plot(Effort,Yield, xlim=c(0,1),ylim=c(0,1),xaxs='i',yaxs='i',xlab=xlab,ylab=ylab,type='l',
         ,col=farb.col[1],main=title, lwd=2)

    # Vergleichsverteilungen
    pNorm = seq(from=0,by=0.01,to=1)
    A=0
    MaxX=1
    B = MaxX-A
    normdistr = (-0.5*B*pNorm^2+MaxX*pNorm)/(A+0.5*B)
    identdistr = pNorm

    points(pNorm,normdistr, type="l", col=farb.col[3]) # Normalverteilung
    points(pNorm,identdistr, type="l", col=farb.col[2]) # Gleichverteilung

    # diagonale
    points(c(0,1),c(1,0),type='l',lty=2,lwd=1,col=farb.col[4],asp=1)

    # Punkte einzeichnen
    points(Effort[AB], Yield[AB],pch=8,lwd=1.5,col='green',cex=1.5,asp=1)
    points(Effort[BC],Yield[BC],pch=8,lwd=1.5,col='blue',cex=1.5,asp=1)

    lines(c(0, Effort[AB], Effort[AB]), c(Yield[AB], Yield[AB], 0), col="red")
    lines(c(0, Effort[BC], Effort[BC]), c(Yield[BC], Yield[BC], 0), col="red")


    points(Effort,Yield,xlim=c(0,1),ylim=c(0,1),lwd=1,col=farb.col[1],main=title,type='l')
  }
  
  return(list(BreakEvenPunktIndex = BreakEvenPunktIndex,
              ParetoPunktIndex = ParetoPunktIndex,
              SubmarginalPunktIndex = SubmarginalPunktIndex,
              ABx = Effort[AB],
              ABy = Yield[AB],
              BCx = Effort[BC],
              BCy = Yield[BC],
              Bx = Effort[B],
              By = Yield[B]))
}
