
calcH20 <- function(metrics, all=FALSE){
  metrics$H20 <- 10 * rowMeans(metrics[,c("SCALED.sed.tol.high", "SCALED.low.N",
                                          "SCALED.N.het","SCALED.DO.50",
                                          "SCALED.halo","SCALED.high.Cu.sp",
                                          "SCALED.high.DOC.sp","SCALED.low.TP.sp")])
  metrics$H20.QCmin <- apply(metrics[,c("QC.sed.tol.PropValvesWithTraits",
                                        "QC.Ptrait.PropValvesWithTraits",
                                        "QC.N.het.PropValvesWithTraits",
                                        "QC.DO.PropValvesWithTraits",
                                        "QC.halo.PropValvesWithTraits",
                                        "propBiovolChlor.QC.propBiovolWithTraits",
                                        "propBiovolChlor.QC.propBiovolWithTraits",
                                        "high.Cu.sp.QC.propBiovolWithTraits",
                                        "high.Cu.sp.QC.propTaxaWithTraits",
                                        "high.DOC.sp.QC.propBiovolWithTraits",
                                        "high.DOC.sp.QC.propTaxaWithTraits",
                                        "low.TP.sp.QC.propBiovolWithTraits",
                                        "low.TP.sp.QC.propTaxaWithTraits")],
                             1, min)
  
  if(all){
    metrics
  } else {
    metrics[, c("SampleID", "H20", "H20.QCmin")]
  }
}

H20 <- function(data, all=FALSE){
  
  metrics <- alg.metrics(data)
  calcH20(metrics, all)
  
}