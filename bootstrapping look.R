#transpiration versus precipitation models
library(dplyr)
import_npp<-
  readRDS('G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/historical_covariates_herbaceous_npp_1.rds')

head(import_npp)
import_npp_2 <- import_npp %>% 
  dplyr::select(x, y, year,mm, WatYrTran)

head(import_npp_2)

import_npp_2_means <- import_npp_2 %>%
  group_by(x, y) %>% 
  summarise_each(funs(mean))
head(import_npp_2_means)
import_npp_2_means_2<-import_npp_2_means[-c(3)]
import_npp_2_means_merged<-merge(import_npp_2_means_2,import_npp_2,by=c('x','y'))
head(import_npp_2_means_merged)
import_npp_2_means_merged$transp.dev<-import_npp_2_means_merged$WatYrTran.y-import_npp_2_means_merged$WatYrTran.x
import_npp_2_means_merged$mm.dev<-import_npp_2_means_merged$mm.y-import_npp_2_means_merged$mm.x

#merge with veg type data
npp_data<-
  readRDS('G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/herbaceous_npp_historical_1985_2015.rds')
head(npp_data)
npp_data_2<-npp_data[-c(5)]
head(npp_data_2)
import_npp_2_means_merged_2<-merge(npp_data_2,import_npp_2_means_merged,by=c('x','y','year'))
head(import_npp_2_means_merged_2)
summary(import_npp_2_means_merged_2)

#designate above and below regional MAP
list.coefficients.mean<-list()
#boostrapping model mm.dev and MaP
for(i in 1:1000)
{
  test.strat<-stratified(import_npp_2_means_merged_2, c("region"), 0.01)
  #test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  #test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  #test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  #test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  #test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    #test.strat.semiarid_steppe, test.strat.hot_deserts)
  
  #stratified_final_mean<-merge(test.strat, mean_production,by=c('x','y'))
  #print(stratified_final)
  
  stratified_final_lm_mean<-lm(npp~mm.y*mm.dev
                               ,test.strat)
  
  
  newcoef1 <- summary(stratified_final_lm_mean)$r.squared
  df.mean<-data.frame(newcoef1)
  df.mean$id = i
  list.coefficients.mean[[i]] <- data.frame(df.mean)
  
  
}

summary(stratified_final_lm_mean)
df.coefficients.mean <- do.call("rbind", list.coefficients.mean)
head(df.coefficients.mean)
df.coefficients.mean.2 <- cbind(rownames(df.coefficients.mean), data.frame(df.coefficients.mean, row.names=NULL))

colnames(df.coefficients.mean.2)  <- c("predictor","coefficient","run.id")

df.coefficients.mean.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub(':', '_', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub('-', '_', df.coefficients.mean.2$predictor)

df2_mean<-reshape(df.coefficients.mean.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(df2_mean)

#make id for precip
df2_mean$model<-'map_mm_dev'
df2_mean_map_mm_dev<-df2_mean
head(df2_mean_map_mm_dev)
summary(df2_mean)

#make id for trnasp
df2_mean$model<-'map_transp_dev'
df2_mean_transp_mm_dev<-df2_mean
head(df2_mean_transp_mm_dev)
summary(df2_mean)

#merge the two
merge_transp_mm<-rbind(df2_mean_transp_mm_dev,df2_mean_map_mm_dev)
head(merge_transp_mm)
library(ggplot2)
ggplot(merge_transp_mm,aes(merge_transp_mm$coefficient.,fill=model)) +
  geom_histogram(color='black',binwidth=0.001) +
  xlab('r-squared')