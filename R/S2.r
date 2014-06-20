
#' Compute S2 IBI
#' 
#' Computes S2 IBI score and QC.
#' 
#' @param data User data merged with metadata, as generated by \code{\link{taxonInfo}}
#' @param all If false (default), return only SampleID, S2 IBI and QC. Else, all metrics are returned.
#
#' @export
#' @examples
#' data(algaeSamples)
#' preparedData <- taxonInfo(validator(algaeSamples))
#' S2(preparedData$cleanData)
#' 

S2 <- function(data, all=FALSE){
  
  metrics <- soft.metrics(data)
  calcS2(metrics, all)
  
}

calcS2 <- function(metrics, all=FALSE){
  metrics$S2 <- 10*rowMeans(metrics[, c("SCALED.propGreenCRUS","SCALED.meanZHR",
                                        "SCALED.high.Cu.sp","SCALED.high.DOC.sp",
                                        "SCALED.low.TP.sp","SCALED.nonref.sp")])
  metrics$S2.QCmin <- apply(metrics[,c("propBiovolChlor.QC.propBiovolWithTraits",
                                       "propBiovolChlor.QC.propBiovolWithTraits",
                                       "propBiovolZHR.QC.propBiovolWithTraits",
                                       "propBiovolZHR.QC.propTaxaWithTraits",
                                       "high.Cu.sp.QC.propBiovolWithTraits",
                                       "high.Cu.sp.QC.propTaxaWithTraits",
                                       "high.DOC.b.QC.propBiovolWithTraits",
                                       "high.DOC.b.QC.propTaxaWithTraits",
                                       "low.TP.sp.QC.propBiovolWithTraits",
                                       "low.TP.sp.QC.propTaxaWithTraits",
                                       "nonref.sp.QC.propBiovolWithTraits",
                                       "nonref.sp.QC.propTaxaWithTraits")],
                            1, min)
  
  if(all){
    metrics
  } else {
    metrics[, c("SampleID", "S2", "S2.QCmin")]
  }
}

