

soft.metrics <- function(data){
  soft.df <- subset(data, AlgaeGroup=="SoftAlgae" & SampleTypeCode!="Integrated")
  
  if(nrow(soft.df) == 0)
    return(data.frame())
  
  soft.df <- ddply(soft.df,.(SampleID, StationCode, SampleDate, Replicate), function(x){
    
    totalSoftBiovolume <- sum(x$Result, na.rm=T)
    x$propBiovol <- x$Result/totalSoftBiovolume 
    uniqueTaxa <- length(unique(x$FinalID))
    
    propMetric <- function(name, col, val, counter="propBiovol"){
      sub <- x[!is.na(x[, col]), ]
      if(is.null(counter)){
        denom <- length(unique(sub$FinalID))
        metric <- length(unique(sub$FinalID[sub[, col] %in% val]))/denom
        QCprop <- sum(sub[, "propBiovol"], na.rm=TRUE)
      } else{
        denom <- sum(sub[, counter], na.rm=TRUE)
        metric <- sum(sub[sub[, col] %in% val, counter], na.rm=T)/denom
        QCprop <- denom
      }

      QCtaxa <- length(unique(sub$FinalID))/uniqueTaxa
      res <- data.frame(metric, QCprop, QCtaxa)
      names(res) <- paste0(name, c(".RAW", ".QC.propBiovolWithTraits", ".QC.propTaxaWithTraits"))
      res
      
      
    }
    
    # Formulaic Metrics
    x1 <- propMetric("high.DOC.b", "IndicatorClass_DOC", "high")
    x2 <- propMetric("nonref.b", "IndicatorClass_Ref", "NRF")
    x3 <- propMetric("propBiovolChlor", "TaxonomicGroup", "CL")
    x5 <- propMetric("high.Cu.sp", "IndicatorClass_Cu", "high", counter=NULL)
    x6 <- propMetric("high.DOC.sp", "IndicatorClass_DOC", "high", counter=NULL)
    x7 <- propMetric("nonref.sp", "IndicatorClass_Ref", "NRF", counter=NULL)
    x8 <- propMetric("low.TP.sp", "IndicatorClass_TP", "low", counter=NULL)
    
    metrics <- cbind(x1, x2, x3, x5, x6, x7, x8)
    
    # Rest of the metrics
    ZHR <- x[(!is.na(x$Zygnemataceae) | !is.na(x$Heterocystous) | !is.na(x$TaxonomicGroup)), ]

    ZHRprop <- sum(ZHR$propBiovol, na.rm=T)
    ZHRpresent <- ZHR[ZHR$Zygnemataceae=="Yes" | ZHR$Heterocystous =="Yes" | ZHR$TaxonomicGroup=="RD",]
  
    metrics$propBiovolZHR.RAW <- sum(ZHRpresent$propBiovol, na.rm=T)/ZHRprop
    metrics$propBiovolZHR.QC.propBiovolWithTraits <- ZHRprop
    metrics$propBiovolZHR.QC.propTaxaWithTraits <- length(unique(ZHR$FinalID))/uniqueTaxa

    metrics$propTaxaZHR.sp.RAW <- length(unique(ZHRpresent$FinalID))/uniqueTaxa
    metrics$meanZHR.RAW <- (metrics$propTaxaZHR.sp.RAW  + metrics$propBiovolZHR.RAW)/2
    
    metrics$totalBiovolGreen.RAW <- sum(x$propBiovol[x$TaxonomicGroup %in% c("CL","CA")], na.rm=TRUE)
    metrics$totalBiovolCRUS.RAW <- sum(x$propBiovol[x$FinalID %in% c("Cladophora glomerata",
                                                                     "Rhizoclonium hieroglyphicum",
                                                                     "Ulva flexuosa") | 
                                                      x$Genus =="Stigeoclonium"], na.rm=TRUE)
    metrics$propGreenCRUS.RAW <-ifelse(metrics$totalBiovolGreen.RAW == 0, 0,
                                       metrics$totalBiovolCRUS.RAW/metrics$totalBiovolGreen.RAW)
    biovolpresent <- x[!is.na(x$TaxonomicGroup) & !is.na(x$Genus), ]
    metrics$totalBiovolCRUS.QC.propBiovolWithTraits <- sum(biovolpresent$propBiovol, na.rm=TRUE)
    metrics$totalBiovolCRUS.QC.propTaxaWithTraits <- nrow(biovolpresent)/nrow(x)
    
    ## Few more QC ##
    
    metrics$types <- paste0(sort(unique(x$SampleTypeCode)), collapse="_")
    metrics$Qual <- ifelse("Qualitative" %in% x$SampleTypeCode,"Yes","No")
    metrics$EntityCount <- ifelse("ActualOrganismCount" %in% names(x),
                                  max(x$ActualOrganismCount, na.rm=T), NA)
    
    
    metrics
    
  })
  
  soft.df$SCALED.high.Cu.sp <- (10:0)[cut(soft.df$high.Cu.sp.RAW, c(0, 0.001, 0.040, 0.079, 0.119, 0.159, 0.198, 0.238, 0.278, 0.317, 
                                           0.357, Inf), right=FALSE)]
  
  soft.df$SCALED.high.DOC.sp <- (10:0)[cut(soft.df$high.DOC.sp.RAW, c(0, 0.136, 0.200, 0.264, 0.329, 0.393, 0.457, 0.521, 0.586, 0.650, 
                                           0.714, Inf), right=FALSE)]
  
  soft.df$SCALED.low.TP.sp <- (0:10)[cut(soft.df$low.TP.sp.RAW, c(-Inf, 0, 0.032, 0.064, 0.096, 0.128, 0.160, 0.192,
                                              0.224, 0.255, 0.287, Inf))]
  
  soft.df$SCALED.meanZHR <- (0:10)[cut(soft.df$meanZHR.RAW, c(-Inf, 0, 0.067, 0.134, 0.202, 0.269, 0.336, 0.403,
                                              0.470, 0.537, 0.605, Inf))]
  
  soft.df$SCALED.nonref.sp <- (10:0)[cut(soft.df$nonref.sp.RAW, c(0, 0.077, 0.120, 0.162, 0.205, 0.248, 0.291, 0.333, 0.376, 0.419, 
                                            0.462, Inf), right=FALSE)]
  
  soft.df$SCALED.high.DOC.b <- (10:0)[cut(soft.df$high.DOC.b.RAW, c(0, 0.001, 0.111, 0.222, 0.333, 0.444, 0.555, 0.667, 0.778, 0.889, 
                                            1, Inf), right=FALSE)]
  
  soft.df$SCALED.nonref.b <- (10:0)[cut(soft.df$nonref.b.RAW, c(0, 0.001, 0.111, 0.222, 0.333, 0.444, 0.555, 0.666, 0.777, 0.888, 
                                            0.999, Inf), right=FALSE)]
  
  soft.df$SCALED.propBiovolZHR <- (0:10)[cut(soft.df$propBiovolZHR.RAW, c(-Inf, 0,  0.103, 0.205, 0.308, 0.411, 0.513, 0.616,
                                              0.719, 0.821, 0.924, Inf))]
  
  soft.df$SCALED.propBiovolChlor <- (10:0)[cut(soft.df$propBiovolChlor.RAW, c(0, 0.001, 0.111, 0.222, 0.333, 0.444, 0.555, 0.666, 0.777, 0.888, 
                                           0.999, Inf), right=FALSE)]
  
  soft.df$SCALED.propGreenCRUS <- (10:0)[cut(soft.df$propGreenCRUS.RAW, c(0, 0.001, 0.111, 0.222, 0.333, 0.444, 0.555, 0.667, 0.778, 0.889, 
                                                                              1, Inf), right=FALSE)]
  
  soft.df
}

