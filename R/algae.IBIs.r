#' Generate IBI report
#' 
#' Returns a report containing D18, S2, and H20 IBIs, along with relevant QC metrics
#' 
#' @param data A data frame containing necessary algae data. See details
#' @param metadata A data frame contain taxonomic lookup data. If null (default), uses metadata distributed with this package
#' 
#' @export
#' @details
#' To be valid, your data should contain the following columns:
#'  \itemize{
#'  \item{StationCode: }{The name of the site where the sample was taken}
#'  \item{SampleDate: }{Date the sample was taken}
#'  \item{Replicate: }{Identifier for multiple samples at a given site}
#'  \item{SampleTypeCode: }{Acceptable values: Epiphyte, Macroalgae, Microalgae, Qualitative}
#'  \item{FinalID: }{Taxa names. These names must match exactly with a FinalID in the metadata, or the row will be discarded}
#'  \item{BAResult: }{Results for Integrated and Epiphyte fractions}
#'  \item{Result: }{Results for Macro/Microalgae algae}
#'  \item{ActualOrganismCount: }{Sample-wide count}
#' }
#' A SampleID column, while not required, is strongly encouraged. If missing, it will be generated as a concatenation
#' of columns StationCode, SampleDate, and Replicate.
#' 
#' @return 
#' A data frame containing the following:
#'  \itemize{
#'  \item{SampleID}{}
#'  \item{S2: }{S2 soft algae IBI score}
#'  \item{S2.QCmin: }{Lowest QC score among metrics used in S2}
#'  \item{D18: }{D18 diatom IBI score}
#'  \item{D18.QCmin: }{Lowest QC score among metrics used in D18}
#'  \item{DH20: }{H20 hybrid (soft algae and diatom) IBI score}
#'  \item{DH20.QCmin: }{Lowest QC score among metrics used in DH20}
#'  \item{DiatomSamplePresent: }{Indicates whether diatom count was non-zero}
#'  \item{SoftAlgaeSampleTypesPresent: }{List of fractions present in sample}
#'  \item{QualPresent: }{Indicates whether qualitative fraction was present}
#'  \item{DiatomCountFlag: }{Flags for low diatom count (threshold of 450)}
#'  \item{SoftAlgaeEntityCountFlag: }{Flags for low entity count (threshold of 225)}
#'  \item{PurgedTaxa: }{Number of rows discarded due to FinalID not matching any entry in metadata}
#' }
#' 
#' @examples
#' data(algaeSamples)
#' ibiReport <- algae.IBIs(algaeSamples)


algae.IBIs <- function(data, metadata = NULL){
  
  processed <- taxonInfo(validator(data), metadata)
  
  metrics <- alg.metrics(processed$cleanData)
  fun <- function(f,d)f(d)
  ibis <- lapply(c(calcS2, calcD18, calcH20), fun, metrics)
  report <- Reduce(merge, ibis)
  
  metrics$DiatomSamplePresent <- ifelse(is.na(metrics$totalDiatomCount) | metrics$totalDiatomCount == 0, "No", "Yes")
  metrics$SoftAlgaeSampleTypesPresent <- metrics$types
  metrics$QualPresent <- metrics$Qual
  metrics$DiatomCountFlag <- ifelse(is.na(metrics$totalDiatomCount) | metrics$totalDiatomCount < 450,
                                   "Inadequate","Adequate")
  metrics$SoftAlgaeEntityCountFlag <- ifelse(is.na(metrics$EntityCount), "Data missing", 
                                            ifelse(metrics$EntityCount < 225,"Inadequate","Adequate"))
  
  Reduce(merge, list(report, metrics[, c("SampleID", "DiatomSamplePresent",
                                         "SoftAlgaeSampleTypesPresent",
                                         "QualPresent", "DiatomCountFlag",
                                         "SoftAlgaeEntityCountFlag")], processed$purged))
}