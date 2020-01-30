#region-specific correlations
library(tidyr)
library(dplyr)
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


#means
sgs_covar_means <- sgs_correlations_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(sgs_covar_means)
View(sgs_covar_means)
sgs_cor<-rcorr(as.matrix(sgs_covar_means[,4:12]), type="pearson")
sgs_correlations_output<-flattenCorrMatrix(sgs_cor$r, sgs_cor$P)
as.data.frame(sgs_correlations_output)
sgs_correlations_output.df<-as.data.frame(sgs_correlations_output)
head(sgs_correlations_output.df) #make into.csv
write.csv(sgs_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/sgs_spatial.csv')

#deviations
sgs_correlations_3<-sgs_correlations_2[-c(3)]
head(sgs_correlations_2)

#get yearly data into long
data_long_sgs <- gather(sgs_correlations_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_sgs)
#get mean data into long
data_long_sgs_2<-sgs_covar_means[-c(3)]
head(data_long_sgs_2)
data_long_sgs_means <- gather(data_long_sgs_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_sgs_means)

merge_means_annual_sgs<-merge(data_long_sgs_means,data_long_sgs,by=c('x','y','covariate'))
head(merge_means_annual_sgs)
merge_means_annual_sgs$annual_dev <- merge_means_annual_sgs$value.y - merge_means_annual_sgs$value.x
merge_means_annual_sgs_2<-merge_means_annual_sgs[-c(4,6)]
head(merge_means_annual_sgs_2)

#long to wide
data_wide_sgs <- spread(merge_means_annual_sgs_2, covariate, annual_dev)
head(data_wide_sgs)

data_wide_sgs_cor<-rcorr(as.matrix(data_wide_sgs[,4:12]), type="pearson")
data_wide_sgs_cor_output<-flattenCorrMatrix(data_wide_sgs_cor$r, data_wide_sgs_cor$P)
data_wide_sgs_cor_output.df<-as.data.frame(data_wide_sgs_cor_output)
head(data_wide_sgs_cor_output.df) #make into.csv
write.csv(data_wide_sgs_cor_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/sgs_temporal.csv')

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

#means
hot_deserts_covar_means <- hot_deserts_correlations_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(hot_deserts_covar_means)
View(hot_deserts_covar_means)

hot_deserts_cor<-rcorr(as.matrix(hot_deserts_covar_means[,4:12]))
hot_deserts_correlations_output<-flattenCorrMatrix(hot_deserts_cor$r, hot_deserts_cor$P)
hot_deserts_correlations_output.df<-as.data.frame(hot_deserts_correlations_output)
head(hot_deserts_correlations_output.df)
write.csv(hot_deserts_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/hot_deserts_spatial.csv')

#deviations

#get yearly data into long
data_long_hot_deserts <- gather(hot_deserts_correlations_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_hot_deserts)
#get mean data into long
data_long_hot_deserts_2<-hot_deserts_covar_means[-c(3)]
head(data_long_hot_deserts_2)
data_long_hot_deserts_means <- gather(data_long_hot_deserts_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_hot_deserts_means)

merge_means_annual_hot_deserts<-merge(data_long_hot_deserts_means,data_long_hot_deserts,by=c('x','y','covariate'))
head(merge_means_annual_hot_deserts)
merge_means_annual_hot_deserts$annual_dev <- merge_means_annual_hot_deserts$value.y - merge_means_annual_hot_deserts$value.x
merge_means_annual_hot_deserts_2<-merge_means_annual_hot_deserts[-c(4,6)]
head(merge_means_annual_hot_deserts_2)

#long to wide
data_wide_hot_deserts <- spread(merge_means_annual_hot_deserts_2, covariate, annual_dev)
head(data_wide_hot_deserts)

data_wide_hot_deserts_cor<-rcorr(as.matrix(data_wide_hot_deserts[,4:12]), type="pearson")
data_wide_hot_deserts_cor_output<-flattenCorrMatrix(data_wide_hot_deserts_cor$r, data_wide_hot_deserts_cor$P)
data_wide_hot_deserts_cor_output.df<-as.data.frame(data_wide_hot_deserts_cor_output)
head(data_wide_hot_deserts_cor_output.df) #make into.csv
write.csv(data_wide_hot_deserts_cor_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/hot_deserts_temporal.csv')


######cold_deserts#######
cold_deserts_correlations<-subset(herbaceous_npp_6,region=='cold_deserts')
head(cold_deserts_correlations)

cold_deserts_correlations_2 <- cold_deserts_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprJun,SWA_AprJun,SWAPETratio_AprJun,VWC1m_AprJu,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(cold_deserts_correlations_2)

#means
cold_deserts_covar_means <- cold_deserts_correlations_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(cold_deserts_covar_means)

cold_deserts_cor<-rcorr(as.matrix(cold_deserts_covar_means[,4:12]))
cold_deserts_correlations_output<-flattenCorrMatrix(cold_deserts_cor$r, cold_deserts_cor$P)
cold_deserts_correlations_output.df<-as.data.frame(cold_deserts_correlations_output)
head(cold_deserts_correlations_output.df)
write.csv(cold_deserts_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/cold_deserts_spatial.csv')

#deviations

#get yearly data into long
data_long_cold_deserts <- gather(cold_deserts_correlations_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_cold_deserts)
#get mean data into long
data_long_cold_deserts_2<-cold_deserts_covar_means[-c(3)]
head(data_long_cold_deserts_2)
data_long_cold_deserts_means <- gather(data_long_cold_deserts_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_cold_deserts_means)

merge_means_annual_cold_deserts<-merge(data_long_cold_deserts_means,data_long_cold_deserts,by=c('x','y','covariate'))
head(merge_means_annual_cold_deserts)
merge_means_annual_cold_deserts$annual_dev <- merge_means_annual_cold_deserts$value.y - merge_means_annual_cold_deserts$value.x
merge_means_annual_cold_deserts_2<-merge_means_annual_cold_deserts[-c(4,6)]
head(merge_means_annual_cold_deserts_2)

#long to wide
data_wide_cold_deserts <- spread(merge_means_annual_cold_deserts_2, covariate, annual_dev)
head(data_wide_cold_deserts)

data_wide_cold_deserts_cor<-rcorr(as.matrix(data_wide_cold_deserts[,4:12]), type="pearson")
data_wide_cold_deserts_cor_output<-flattenCorrMatrix(data_wide_cold_deserts_cor$r, data_wide_cold_deserts_cor$P)
data_wide_cold_deserts_cor_output.df<-as.data.frame(data_wide_cold_deserts_cor_output)
head(data_wide_cold_deserts_cor_output.df) #make into.csv
write.csv(data_wide_cold_deserts_cor_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/cold_deserts_temporal.csv')

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

#means
california_annuals_covar_means <- california_annuals_correlations_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(california_annuals_covar_means)

california_annuals_cor<-rcorr(as.matrix(california_annuals_covar_means[,4:12]))
california_annuals_correlations_output<-flattenCorrMatrix(california_annuals_cor$r, california_annuals_cor$P)
california_annuals_correlations_output.df<-as.data.frame(california_annuals_correlations_output)
head(california_annuals_correlations_output.df)
write.csv(california_annuals_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/cali_annuals_spatial.csv')

#deviations

#get yearly data into long
data_long_california_annuals <- gather(california_annuals_correlations_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_california_annuals)
#get mean data into long
data_long_california_annuals_2<-california_annuals_covar_means[-c(3)]
head(data_long_california_annuals_2)
data_long_california_annuals_means <- gather(data_long_california_annuals_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_california_annuals_means)

merge_means_annual_california_annuals<-merge(data_long_california_annuals_means,data_long_california_annuals,by=c('x','y','covariate'))
head(merge_means_annual_california_annuals)
merge_means_annual_california_annuals$annual_dev <- merge_means_annual_california_annuals$value.y - merge_means_annual_california_annuals$value.x
merge_means_annual_california_annuals_2<-merge_means_annual_california_annuals[-c(4,6)]
head(merge_means_annual_california_annuals_2)

#long to wide
data_wide_california_annuals <- spread(merge_means_annual_california_annuals_2, covariate, annual_dev)
head(data_wide_california_annuals)

data_wide_california_annuals_cor<-rcorr(as.matrix(data_wide_california_annuals[,4:12]), type="pearson")
data_wide_california_annuals_cor_output<-flattenCorrMatrix(data_wide_california_annuals_cor$r, data_wide_california_annuals_cor$P)
data_wide_california_annuals_cor_output.df<-as.data.frame(data_wide_california_annuals_cor_output)
head(data_wide_california_annuals_cor_output.df) #make into.csv
write.csv(data_wide_california_annuals_cor_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/cali_annuals_temporal.csv')


######northern mixed prairies##########
northern_mixed_prairies_correlations<-subset(herbaceous_npp_6,region=='northern_mixed_prairies')
head(northern_mixed_prairies_correlations)

northern_mixed_prairies_correlations_2 <- northern_mixed_prairies_correlations %>% 
  dplyr::select(x, y, year,mm,PET_AprJun,SWA_AprJun,SWAPETratio_AprJun,VWC1m_AprJu,WatYrTEMP, WatYrTran,WatYrWDDa, WatYrWDto)

head(northern_mixed_prairies_correlations_2)

#means
northern_mixed_prairies_covar_means <- northern_mixed_prairies_correlations_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(northern_mixed_prairies_covar_means)

northern_mixed_prairies_cor<-rcorr(as.matrix(northern_mixed_prairies_covar_means[,4:12]))
northern_mixed_prairies_correlations_output<-flattenCorrMatrix(northern_mixed_prairies_cor$r, northern_mixed_prairies_cor$P)
northern_mixed_prairies_correlations_output.df<-as.data.frame(northern_mixed_prairies_correlations_output)
head(northern_mixed_prairies_correlations_output.df)
write.csv(northern_mixed_prairies_correlations_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/northern_mixed_spatial.csv')

#deviations

#get yearly data into long
data_long_northern_mixed_prairies <- gather(northern_mixed_prairies_correlations_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_northern_mixed_prairies)
#get mean data into long
data_long_northern_mixed_prairies_2<-northern_mixed_prairies_covar_means[-c(3)]
head(data_long_northern_mixed_prairies_2)
data_long_northern_mixed_prairies_means <- gather(data_long_northern_mixed_prairies_2, covariate,value,mm:WatYrWDto, factor_key=TRUE)
head(data_long_northern_mixed_prairies_means)

merge_means_annual_northern_mixed_prairies<-merge(data_long_northern_mixed_prairies_means,data_long_northern_mixed_prairies,by=c('x','y','covariate'))
head(merge_means_annual_northern_mixed_prairies)
merge_means_annual_northern_mixed_prairies$annual_dev <- merge_means_annual_northern_mixed_prairies$value.y - merge_means_annual_northern_mixed_prairies$value.x
merge_means_annual_northern_mixed_prairies_2<-merge_means_annual_northern_mixed_prairies[-c(4,6)]
head(merge_means_annual_northern_mixed_prairies_2)

#long to wide
data_wide_northern_mixed_prairies <- spread(merge_means_annual_northern_mixed_prairies_2, covariate, annual_dev)
head(data_wide_northern_mixed_prairies)

data_wide_northern_mixed_prairies_cor<-rcorr(as.matrix(data_wide_northern_mixed_prairies[,4:12]), type="pearson")
data_wide_northern_mixed_prairies_cor_output<-flattenCorrMatrix(data_wide_northern_mixed_prairies_cor$r, data_wide_northern_mixed_prairies_cor$P)
data_wide_northern_mixed_prairies_cor_output.df<-as.data.frame(data_wide_northern_mixed_prairies_cor_output)
head(data_wide_northern_mixed_prairies_cor_output.df) #make into.csv
write.csv(data_wide_northern_mixed_prairies_cor_output.df,
          'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Regularization/Correlations/historical_climate_correlations_jan_29_2020/spatial vs. temporal/northern_mixed_temporal.csv')