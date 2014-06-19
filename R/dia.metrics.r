
dia.metrics <- function(data){
  dia.df <- subset(data, AlgaeGroup=="Diatom" & SampleTypeCode=="Integrated")
  
  if(nrow(dia.df) == 0)
    return(data.frame())
  
  dia.df <- ddply(dia.df,.(SampleID, StationCode, SampleDate, Replicate), function(x){
   
    # Count and proportion
    x$totalDiatomCount <- sum(x$BAResult, na.rm=T)
    x$propDiatoms <- x$BAResult/x$totalDiatomCount
    
    ### Metrics ###
    
    x$propAchMin <- sum(x$propDiatoms[x$FinalID=="Achnanthidium minutissimum"], na.rm=T)

    x_mh <- x[(!is.na(dia.df$Motility) & !is.na(dia.df$Habit)) ,]
    x_mh_sum <- sum(x_mh$propDiatoms, na.rm=T)
    x$QC.sed.tol.PropValvesWithTraits <- x_mh_sum
    x$RAW.sed.tol.high <- sum(x_mh$propDiatoms[x_mh$Motility=="HM" | x_mh$Habit=="P"], na.rm=T)/
      x_mh_sum
    
    x_s <- x[!is.na(x$Salinity) ,]
    x_s_sum <- sum(x_s$propDiatoms, na.rm=T)
    x$QC.halo.PropValvesWithTraits <- x_s_sum
    x$RAW.halo <- sum(x_s$propDiatoms[x_s$Salinity %in% c("BF","B")], na.rm=T)/
      x_s_sum
    
    x_sap <- x[!is.na(x$Saprobity) ,]
    x_sap_sum <- sum(x_sap$propDiatoms, na.rm=T)
    x$QC.saprobity.PropValvesWithTraits <- x_sap_sum
    x$RAW.sapro <- sum(x_sap$propDiatoms[x_sap$Saprobity %in% c("OS","BM")], na.rm=T)/
      x_sap_sum
    
    x_o <- x[!is.na(x$OxygenRequirements) ,]
    x_o_sum <- sum(x_o$propDiatoms, na.rm=T)
    x$QC.DO.PropValvesWithTraits <- x_o_sum
    x$RAW.DO.50 <- sum(x_o$propDiatoms[x_o$OxygenRequirements %in% c("DO_100","DO_75","DO_50")], na.rm=T)/
      x_o_sum
    x$RAW.DO.100 <- sum(x_o$propDiatoms[x_o$OxygenRequirements == "DO_100"], na.rm=T)/
      x_o_sum

    x_e <- x[!is.na(x$TrophicState),]
    x_e_sum <- sum(x_e$propDiatoms, na.rm=T)
    x$QC.TrophicState.PropValvesWithTraits <- x_e_sum
    x$RAW.eutro <- sum(x_e$propDiatoms[x_e$TrophicState %in% c("PH","E")], na.rm=T)/
      x_e_sum
    
    x_n <- x[!is.na(x$NitrogenUptakeMetabolism),]
    x_n_sum <- sum(x_n$propDiatoms, na.rm=T)
    x$QC.N.het.PropValvesWithTraits <- x_n_sum
    x$RAW.N.het <- sum(x_n$propDiatoms[x_e$NitrogenUptakeMetabolism %in% c("NHHONF","NHHONO")], na.rm=T)/
      x_n_sum
    
    x_i <- x[!is.na(x$IndicatorClass_TP),]
    x_i_sum <- sum(x_i$propDiatoms, na.rm=T)
    x$QC.Ptrait.PropValvesWithTraits <- x_i_sum
    x$RAW.low.P <- sum(x_i$propDiatoms[x_i$IndicatorClass_TP %in% c("low")])/x_i_sum

    x_tn <- x[!is.na(x$IndicatorClass_TN),]
    x_tn_sum <- sum(x_tn$propDiatoms, na.rm=T)
    x$QC.Ntrait.PropValvesWithTraits <- x_tn_sum
    x$RAW.low.N <- sum(x_tn$propDiatoms[x_tn$IndicatorClass_TN %in% c("low")])/x_tn_sum
    
    x_mot <- x[!is.na(x$Motility),]
    x_mot_sum <- sum(x_mot$propDiatoms, na.rm=T)
    x$QC.Motility.PropValvesWithTraits <- x_mot_sum
    x$RAW.highly.mot <- sum(x_mot$propDiatoms[x_mot$Motility %in% c("HM")])/x_mot_sum
    

    x
  })

  
  dia.df <- unique(dia.df[, c("SampleID", "StationCode","SampleDate","Replicate", "totalDiatomCount",
                              "propAchMin",
                              "QC.sed.tol.PropValvesWithTraits","RAW.sed.tol.high",
                              "QC.halo.PropValvesWithTraits","RAW.halo",
                              "QC.saprobity.PropValvesWithTraits", "RAW.sapro",
                              "QC.DO.PropValvesWithTraits", "RAW.DO.50",
                              "RAW.DO.100","QC.TrophicState.PropValvesWithTraits",
                              "RAW.eutro","QC.N.het.PropValvesWithTraits",
                              "RAW.N.het","QC.Ptrait.PropValvesWithTraits",
                              "RAW.low.P","QC.Ntrait.PropValvesWithTraits",
                              "RAW.low.N","QC.Motility.PropValvesWithTraits", 
                              "RAW.highly.mot")])
  
  # Score metrics
  dia.df$SCALED.AchMin <- (0:10)[cut(dia.df$propAchMin, c(-Inf, 0, 0.034, 0.068, 0.102, 0.136, 0.170, 0.204,
                                          0.239, 0.273, 0.307, Inf))]
  
  dia.df$SCALED.sed.tol.high <- (10:0)[cut(dia.df$RAW.sed.tol.high, c(0, 0.025, 0.076, 0.128, 0.179, 0.231, 0.282, 0.333, 0.385, 0.436, 
                      0.488, Inf), right=FALSE)]
  
  dia.df$SCALED.halo <- (10:0)[cut(dia.df$RAW.halo, c(0, 0.011, 0.069, 0.127,  0.185, 0.243, 0.301, 0.359, 0.417,  0.475, 
                      0.533, Inf), right=FALSE)]
  
  dia.df$SCALED.sapro <- (0:10)[cut(dia.df$RAW.sapro, c(-Inf, 0.221, 0.300, 0.380, 0.460, 0.539, 0.619,
                 0.699, 0.778,  0.858, 0.938, Inf))]
  
  dia.df$SCALED.DO.50 <- (0:10)[cut(dia.df$RAW.DO.50, c(-Inf, 0.632, 0.673, 0.713, 0.754, 0.794, 0.835, 0.875,
                 0.916, 0.956, 0.997, Inf))]
  
  dia.df$SCALED.DO.100 <- (0:10)[cut(dia.df$RAW.DO.100, c(-Inf, 0.023, 0.087, 0.152, 0.216, 0.281, 0.345, 0.410,
                 0.474, 0.539, 0.603, Inf))]
  
  dia.df$SCALED.eutro <- (10:0)[cut(dia.df$RAW.eutro, c(0, 0.289, 0.363, 0.437, 0.511, 0.585, 0.659, 0.732, 0.806, 0.880, 
                      0.954, Inf), right=FALSE)]
  
  dia.df$SCALED.N.het <- (10:0)[cut(dia.df$RAW.N.het, c(0, 0.008, 0.053, 0.109, 0.165, 0.221, 0.277, 0.333, 0.389,  0.445, 
                      0.512, Inf), right=FALSE)]
  
  dia.df$SCALED.low.P <- (0:10)[cut(dia.df$RAW.low.P , c(-Inf,  0.009, 0.090, 0.171, 0.252, 0.333, 0.414, 0.495,
                 0.576, 0.658,  0.739, Inf))]
  
  dia.df$SCALED.low.N <- (0:10)[cut(dia.df$RAW.low.N , c(-Inf,  0.009, 0.090, 0.177 , 0.260, 0.344, 0.428, 0.512,
                 0.595, 0.679,  0.763, Inf))]
  
  dia.df$SCALED.highly.mot <- (10:0)[cut(dia.df$RAW.highly.mot, c(0, 0.023, 0.066, 0.110, 0.154, 0.197, 0.241, 0.285, 0.328,  0.372, 
                      0.416, Inf), right=FALSE)]

  dia.df
}

