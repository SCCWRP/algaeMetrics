

calcD18 <- function(metrics, all=FALSE){
  metrics$D18 <- 10*rowMeans(metrics[,c("SCALED.sed.tol.high","SCALED.low.P",
                                        "SCALED.N.het","SCALED.DO.50","SCALED.halo")])
  
  metrics$D18.QCmin<-  apply(metrics[,c("QC.sed.tol.PropValvesWithTraits",
                                        "QC.Ptrait.PropValvesWithTraits",
                                        "QC.N.het.PropValvesWithTraits",
                                        "QC.DO.PropValvesWithTraits",
                                        "QC.halo.PropValvesWithTraits")], 
                             1, min)
  if(all){
    metrics
  } else {
    metrics[, c("SampleID", "D18", "D18.QCmin")]
  }
}

D18 <- function(data, all=FALSE){
  
  metrics <- dia.metrics(data)
  calcD18(metrics, all)  
  
}