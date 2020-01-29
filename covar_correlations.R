#region-specific correlations
head(herbaceous_npp_6)
library("PerformanceAnalytics")
library(Hmisc)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

######sgs###########

sgs_correlations<-subset(herbaceous_npp_6,region=='shortgrass_steppe')
head(sgs_correlations)
sgs_vars <- c("x", "y", "year","mm","PET_AprJun","SWA_AprJu","SWAPETratio_AprJun","VWC1m_AprJu","WatYrTEMP", "WatYrTran","WatYrWDDa", "WatYrWDto")

myvars <- c("v1", "v2", "v3")
sgs_correlations_2 <- sgs_correlations[sgs_vars]

sgs_correlations_2 <- sgs_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprJun,SWA_AprJun,SWAPETratio_AprJun,VWC1m_AprJu,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(sgs_correlations_2)
?rcorr
sgs_cor<-rcorr(as.matrix(sgs_correlations_2[,4:12]), type="pearson")
sgs_correlations_output<-flattenCorrMatrix(sgs_cor$r, sgs_cor$P)
as.data.frame(sgs_correlations_output)
sgs_correlations_output.df<-as.data.frame(sgs_correlations_output)
head(sgs_correlations_output.df) #make into.csv
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/sgs/sgs.csv')

######hot_deserts#######
hot_deserts_correlations<-subset(herbaceous_npp_6,region=='hot_deserts')
head(hot_deserts_correlations)
hot_deserts_correlations$PET_AprSep<- hot_deserts_correlations$PET_AprJun + hot_deserts_correlations$PET_JulSep
hot_deserts_correlations$SWA_AprSep<- hot_deserts_correlations$SWA_AprJun + hot_deserts_correlations$SWA_JulSep
hot_deserts_correlations$VWC1m_AprSep<- hot_deserts_correlations$VWC1m_AprJu + hot_deserts_correlations$VWC1m_JulSe
hot_deserts_correlations$SWAPETratio_AprSep<- hot_deserts_correlations$SWAPETratio_AprJun + hot_deserts_correlations$SWAPETratio_JulSep

hot_deserts_correlations_2 <- hot_deserts_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprSep,SWA_AprSep,SWAPETratio_AprSep,VWC1m_AprSep,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(hot_deserts_correlations_2)

hot_deserts_cor<-rcorr(as.matrix(hot_deserts_correlations_2[,4:12]))
hot_deserts_correlations_output<-flattenCorrMatrix(hot_deserts_cor$r, hot_deserts_cor$P)
hot_deserts_correlations_output.df<-as.data.frame(hot_deserts_correlations_output)
head(hot_deserts_correlations_output.df)
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/hot_deserts/hot_deserts.csv')

######cold_deserts#######
cold_deserts_correlations<-subset(herbaceous_npp_6,region=='cold_deserts')
head(cold_deserts_correlations)

cold_deserts_correlations_2 <- cold_deserts_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprJun,SWA_AprJun,SWAPETratio_AprJun,VWC1m_AprJu,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(cold_deserts_correlations_2)

cold_deserts_cor<-rcorr(as.matrix(cold_deserts_correlations_2[,4:12]))
cold_deserts_correlations_output<-flattenCorrMatrix(cold_deserts_cor$r, cold_deserts_cor$P)
cold_deserts_correlations_output.df<-as.data.frame(cold_deserts_correlations_output)
head(cold_deserts_correlations_output.df)
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/cold_deserts/cold_deserts.csv')

######california annuals#########
california_annuals_correlations<-subset(herbaceous_npp_6,region=='california_annuals')
head(california_annuals_correlations)
california_annuals_correlations$PET_jan_june<- california_annuals_correlations$PET_AprJun + california_annuals_correlations$PET_JanMar
california_annuals_correlations$SWA_jan_june<- california_annuals_correlations$SWA_AprJun + california_annuals_correlations$SWA_JanMar
california_annuals_correlations$VWC1m_jan_june<- california_annuals_correlations$VWC1m_AprJu + california_annuals_correlations$VWC1m_JanMa
california_annuals_correlations$SWAPETratio_jan_june<- california_annuals_correlations$SWAPETratio_AprJun + california_annuals_correlations$SWAPETratio_JanMar

california_annuals_correlations_2 <- california_annuals_correlations %>% 
  dplyr::select(x, y, year,mm,PET_jan_june,SWA_jan_june,SWAPETratio_jan_june,VWC1m_jan_june,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(california_annuals_correlations_2)

california_annuals_cor<-rcorr(as.matrix(california_annuals_correlations_2[,4:12]))
california_annuals_correlations_output<-flattenCorrMatrix(california_annuals_cor$r, california_annuals_cor$P)
california_annuals_correlations_output.df<-as.data.frame(california_annuals_correlations_output)
head(california_annuals_correlations_output.df)
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/california_annuals/california_annuals.csv')

######northern mixed prairies##########
northern_mixed_prairies_correlations<-subset(herbaceous_npp_6,region=='northern_mixed_prairies')
head(northern_mixed_prairies_correlations)

northern_mixed_prairies_correlations_2 <- northern_mixed_prairies_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprJun,SWA_AprJun,SWAPETratio_AprJun,VWC1m_AprJu,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(northern_mixed_prairies_correlations_2)

northern_mixed_prairies_cor<-rcorr(as.matrix(northern_mixed_prairies_correlations_2[,4:12]))
northern_mixed_prairies_correlations_output<-flattenCorrMatrix(northern_mixed_prairies_cor$r, northern_mixed_prairies_cor$P)
northern_mixed_prairies_correlations_output.df<-as.data.frame(northern_mixed_prairies_correlations_output)
head(northern_mixed_prairies_correlations_output.df)
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/northern mixed prairies/northern_mixed.csv')