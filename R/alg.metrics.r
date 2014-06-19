
#' Generates all algae metrics 
#' 
#' Returns all metrics for soft algae and diatoms, along with relevant QC metrics
#' 
#
#' @export
#' @examples
#' data(algaeSamples)
#' preparedData <- taxonInfo(validator(algaeSamples))
#' alg.metrics(preparedData)


alg.metrics <- function(data){
  diatoms <- dia.metrics(data)
  soft <- soft.metrics(data)
  
  if(nrow(diatoms) == 0)
    soft
  else if(nrow(soft) == 0)
    diatoms
  else
  join(diatoms, soft, by=c("SampleID", "StationCode","SampleDate","Replicate"),
       type="full")
}
