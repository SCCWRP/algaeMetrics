#' Pull updated metadata
#' 
#' Pull updated metadata from Moss Landing Marine Labratory's data warehouse
#' 
#' @export
#' @return
#' A data frame containing updated metadata. This will not replace the internal database in this package.


grabMetaData <- function(){
  
  if(!require(RODBC, quietly = TRUE))
    stop("Required packaged RODBC not found.")
  
  query <- "SELECT BARM_AlgaeAttributes.AlgaeGroup, BARM_AlgaeAttributes.OrganismCode,
  BARM_AlgaeAttributes.FinalID, BARM_AlgaeAttributes.AlgaeList, BARM_AlgaeAttributes.Habit,
  BARM_AlgaeAttributes.Heterocystous, BARM_AlgaeAttributes.IndicatorClass_Cu, BARM_AlgaeAttributes.IndicatorClass_DOC,
  BARM_AlgaeAttributes.IndicatorClass_Ref, BARM_AlgaeAttributes.IndicatorClass_TN, BARM_AlgaeAttributes.IndicatorClass_TP,
  BARM_AlgaeAttributes.Motility, BARM_AlgaeAttributes.NitrogenUptakeMetabolism, BARM_AlgaeAttributes.OxygenRequirements,
  BARM_AlgaeAttributes.Salinity, BARM_AlgaeAttributes.Saprobity, BARM_AlgaeAttributes.TaxonomicGroup, BARM_AlgaeAttributes.TrophicState,
  BARM_AlgaeAttributes.Zygnemataceae, OrganismLookUp.Phylum, OrganismLookUp.Class, OrganismLookUp.[Order], OrganismLookUp.Family,
  OrganismLookUp.Genus, OrganismLookUp.TaxonomicLevelCode, OrganismLookUp.TaxonomicLevelName FROM BARM_AlgaeAttributes
  INNER JOIN OrganismLookUp ON BARM_AlgaeAttributes.FinalID = OrganismLookUp.FinalID;"
  
  connection <- suppressWarnings(try(odbcDriverConnect("DRIVER={SQL Native Client};
                      SERVER=205.155.75.83,2866;UID=Read_Only;
                      DATABASE=DW_Full;Pwd=Read_Only")))
  if(connection == -1)stop("Failed to connect to MLML. Check to make sure you have SQL Native Client installed")
  on.exit(odbcClose(connection))
  
  result <- sqlQuery(connection, query)
  result[result == ""] <- NA
  result
}





