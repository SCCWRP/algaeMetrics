

grabMetaData <- function(){
  
  if(!require(RODBC, quietly = TRUE))
    stop("Required packaged RODBC not found.")
  
  query <- "SELECT BARM_AlgaeAttributes.AlgaeGroup, BARM_AlgaeAttributes.OrganismCode, BARM_AlgaeAttributes.FinalID, BARM_AlgaeAttributes.AlgaeList, BARM_AlgaeAttributes.Habit, BARM_AlgaeAttributes.Heterocystous, BARM_AlgaeAttributes.IndicatorClass_Cu, BARM_AlgaeAttributes.IndicatorClass_DOC, BARM_AlgaeAttributes.IndicatorClass_Ref, BARM_AlgaeAttributes.IndicatorClass_TN, BARM_AlgaeAttributes.IndicatorClass_TP, BARM_AlgaeAttributes.Motility, BARM_AlgaeAttributes.NitrogenUptakeMetabolism, BARM_AlgaeAttributes.OxygenRequirements, BARM_AlgaeAttributes.Salinity, BARM_AlgaeAttributes.Saprobity, BARM_AlgaeAttributes.TaxonomicGroup, BARM_AlgaeAttributes.TrophicState, BARM_AlgaeAttributes.Zygnemataceae, dbo_OrganismLookUp.Phylum, dbo_OrganismLookUp.Class, dbo_OrganismLookUp.Order, dbo_OrganismLookUp.Family, dbo_OrganismLookUp.Genus, dbo_OrganismLookUp.TaxonomicLevelCode, dbo_OrganismLookUp.TaxonomicLevelName
  FROM BARM_AlgaeAttributes INNER JOIN dbo_OrganismLookUp ON BARM_AlgaeAttributes.FinalID = dbo_OrganismLookUp.FinalID;"
  
  connection <- odbcDriverConnect("Description=Reporting_Module;DRIVER={SQL Native Client};
                                  SERVER=205.155.75.83,2866;UID=Read_Only;APP=Reporting_Module;
                                  WSID=MyWSID;DATABASE=DW_Full;Password=MyPassword")
  on.exit(odbcClose(connection))
  
  result <- sqlQuery(connection, query)
  result
}





