
#' Algae taxon validation 
#' 
#' Checks that all FinalIDs are present in metadata. If not, the row is purged. Remainder are then merged together with metadata.
#' 
#' 
#' @param data A validated algae data frame
#' @param algaeMetaData A data frame contain taxonomic lookup data. If null (default), uses metadata distributed with this package
#' 
#' @return A list containing the cleaned data, a report of the number rows purged per sample, a table listing all taxa in all samples,
#' and whether it was present in the metadata.
#' 
#' @export
#' @examples
#' data(algaeSamples)
#' tinfo <- taxonInfo(validator(algaeSamples))
#' tinfo$taxaSummary

taxonInfo <- function(data, algaeMetaData = NULL){
  
  if(is.null(algaeMetaData))
    algaeMetaData <- read.csv(system.file("extdata", "algaeMetaData3.csv", package="algaeMetrics"),
                              stringsAsFactors=FALSE)
  
  data$FinalID <- str_trim(data$FinalID) 
  missing.taxa <- setdiff(data$FinalID, algaeMetaData$FinalID)
  
  if(is.null(data$SampleID))
    data$SampleID <- paste(data$StationCode, data$SampleDate, data$Replicate, sep="_")
  mID <- is.na(data$SampleID)
  if(any(mID))
    data$SampleID[mID] <- paste(data$StationCode[mID], data$SampleDate[mID], data$Replicate[mID], sep="_")
  
  npurged <- ddply(data, .(SampleID), function(x){
    data.frame(SampleID = unique(x$SampleID),
               PurgedTaxa = sum(x$FinalID %in% missing.taxa))
  })

  sumtable <- data.frame(FinalID = unique(data$FinalID))
  sumtable$PresentInMetadata <- ifelse(sumtable$FinalID %in% missing.taxa, FALSE, TRUE)
  
  data<- subset(data, !(FinalID %in% missing.taxa))
  list(cleanData = merge(data, algaeMetaData, all.x=T),
       purged = npurged,
       taxaSummary = sumtable)
}