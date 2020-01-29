# shortgrass steppe herbaceous npp processing

#packages
library(dplyr)
library(reshape2)
library(raster)

#subset to cold deserts layer
sites_df_sgs <-sites_df %>%
  dplyr::filter(layer == 5)

head(sites_df_sgs)

#change to raster
sgs_raster<-rasterFromXYZ(sites_df_sgs)

plot(sgs_raster)

#make an extent for this ecoregion
extent_sgs<-extent(sgs_raster)
sgs_initial_mask_raster<-crop(herb_npp_50_threshold_2,extent_sgs)
plot(sgs_initial_mask_raster)

#mask out non-cold deserts pixels
sgs_initial_mask_raster_2<-mask(sgs_initial_mask_raster,sgs_raster)
plot(sgs_initial_mask_raster_2) #works

merge_sgs_1<-rasterToPoints(sgs_initial_mask_raster_2)
head(merge_sgs_1)
summary(merge_sgs_1)

#get temporal dataset down to scale of sgs
merge_sgs_2<-merge(merge_sgs_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_sgs_2)
View(merge_sgs_2)

#make year numeric
merge_sgs_2$year <- as.numeric(as.character(merge_sgs_2$year))

#test the means match up
test_means<-aggregate(npp.y~ x + y + npp.x,mean,data=merge_sgs_2)
View(test_means)
plot(npp.x~npp.y,data=test_means) # looks good

#clean up
merge_sgs_3<-merge_sgs_2[-c(3)]
head(merge_sgs_3)
View(merge_sgs_3)
summary(merge_sgs_3)
names(merge_sgs_3)[names(merge_sgs_3) == "npp.y"] <- "npp"

#merge with precipitation data
sgs_npp_mm<-merge(merge_sgs_3,df.WatYrPRECIP,by=c('x','y','year'))
head(sgs_npp_mm)
View(sgs_npp_mm)
summary(sgs_npp_mm)

#look at per-pixel mean RUE
mean_rue_sgs<-sgs_npp_mm %>%
  group_by(x, y) %>%
  summarise_each(funs(mean))

head(mean_rue_sgs)

mean_rue_sgs_2<-as.data.frame(mean_rue_sgs)
head(mean_rue_sgs_2)
plot(npp~mm,data=mean_rue_sgs_2)

mean_rue_sgs_2$pue <- mean_rue_sgs_2$npp/mean_rue_sgs_2$mm
summary(mean_rue_sgs_2)

#set threshold for masking
mean(mean_rue_sgs_2$pue) + 3*sd(mean_rue_sgs_2$pue) #0.64
mean(mean_rue_sgs_2$pue) - 3*sd(mean_rue_sgs_2$pue) #0.24

#isolate values greater than 3sd away for mean precip use efficiency
mean_rue_sgs_2_filtered <- mean_rue_sgs_2 %>%
  dplyr::filter(pue < 0.64 & pue > 0.24) 
summary(mean_rue_sgs_2_filtered)
mean_rue_sgs_2_filtered_2<-mean_rue_sgs_2_filtered[-c(3,4,5)]
head(mean_rue_sgs_2_filtered_2)

#merge npp-ppt datasets all years for dataset to generate slope
mean_rue_sgs_2_filtered_allyears<-merge(mean_rue_sgs_2_filtered_2,sgs_npp_mm,by=c("x","y"))
head(mean_rue_sgs_2_filtered_allyears)
summary(mean_rue_sgs_2_filtered_allyears)
mean_rue_sgs_2_filtered_allyears_2<-mean_rue_sgs_2_filtered_allyears[-c(3,4)]
head(mean_rue_sgs_2_filtered_allyears_2)

#generate temporal slopes
slope_temporal_sgs_herb <- mean_rue_sgs_2_filtered_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(slope_temporal_sgs_herb)
sgs_coef_only_herb<- slope_temporal_sgs_herb[ -c(3) ] #isolate coefficient so only slope is graphed
head(sgs_coef_only_herb)
slope_temporal_sgs_herb_2<-as.data.frame(sgs_coef_only_herb)
head(slope_temporal_sgs_herb_2)
summary(slope_temporal_sgs_herb_2)
hist(slope_temporal_sgs_herb_2$coef)

#set threshold for masking
mean(slope_temporal_sgs_herb_2$coef) + 3*sd(slope_temporal_sgs_herb_2$coef) #0.51
mean(slope_temporal_sgs_herb_2$coef) - 3*sd(slope_temporal_sgs_herb_2$coef) #0.05

#isolate values greater than 3sd away for mean precip use efficiency
slope_temporal_sgs_herb_2_filtered <- slope_temporal_sgs_herb_2 %>%
  dplyr::filter(coef < 0.51 & coef > 0.05) 
summary(slope_temporal_sgs_herb_2_filtered)

temporal_slope_sgs_filtered_allyears<-merge(slope_temporal_sgs_herb_2_filtered,sgs_npp_mm,by=c("x","y"))
head(temporal_slope_sgs_filtered_allyears)

#look at yearly precip use efficiency
temporal_slope_sgs_filtered_allyears$pue <- temporal_slope_sgs_filtered_allyears$npp/temporal_slope_sgs_filtered_allyears$mm
summary(temporal_slope_sgs_filtered_allyears)

#set threshold for masking
mean(temporal_slope_sgs_filtered_allyears$pue) + 3*sd(temporal_slope_sgs_filtered_allyears$pue) #0.82
mean(temporal_slope_sgs_filtered_allyears$pue) - 3*sd(temporal_slope_sgs_filtered_allyears$pue) #0.086

yearly_pue_anamolies_sgs <- temporal_slope_sgs_filtered_allyears %>%
  dplyr::filter(pue > 0.82 | pue < 0.086) 
summary(yearly_pue_anamolies_sgs)

yearly_pue_anamolies_sgs_final<-yearly_pue_anamolies_sgs[-c(3)]
summary(yearly_pue_anamolies_sgs_final)
hist(yearly_pue_anamolies_sgs_final$pue)
head(yearly_pue_anamolies_sgs_final)

#make raster for the odd values
yearly_pue_anamolies_sgs_final_2<-yearly_pue_anamolies_sgs_final[-c(3,4,5)]
odd_pue_values_sgs<-rasterFromXYZ(yearly_pue_anamolies_sgs_final_2)
plot(odd_pue_values_sgs)

#final masking raster
temporal_slope_sgs_filtered_allyears_raster<-rasterFromXYZ(slope_temporal_sgs_herb_2_filtered)
plot(temporal_slope_sgs_filtered_allyears_raster)
sgs_masking_extent_2<-crop(temporal_slope_sgs_filtered_allyears_raster,odd_pue_values_sgs)
sgs_masking<-mask(sgs_masking_extent_2,odd_pue_values_sgs,inverse=TRUE) #mask out pixels with bad pue
plot(sgs_masking)

sgs_masking_df<-rasterToPoints(sgs_masking)
summary(sgs_masking_df)

#merge for finally herbaceous NPP dataframe
sgs_final_herb_npp<-merge(sgs_masking_df,sgs_npp_mm,by=c('x','y'))
summary(sgs_final_herb_npp)
sgs_final_herb_npp$rue<-sgs_final_herb_npp$npp/sgs_final_herb_npp$mm
summary(sgs_final_herb_npp)

#final npp for sgs
sgs_final_herb_npp_2<-sgs_final_herb_npp[-c(3,7)]
head(sgs_final_herb_npp_2)
sgs_final_herb_npp_2$region <- 'shortgrass_steppe'

#additional stuff for regularization prep
sgs_reg_historical<-sgs_final_herb_npp_2
sgs_reg_historical_merged<-merge(sgs_reg_historical,df.wy_wide,by=c('x','y','year'))
head(sgs_reg_historical_merged)
sgs_reg_historical_merged<-sgs_reg_historical_merged[-c(6,8,9,10)]
summary(sgs_reg_historical_merged)
colnames(sgs_reg_historical_merged)  <- c("x","y","year",'npp','mm','temp')
saveRDS(sgs_reg_historical_merged, file = 'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/historical_precip_temp_sgs_test.rds')

#TO CROP WITH
for_cropping_sgs<-aggregate(npp~x+y,mean,data=sgs_reg_historical_merged)
head(for_cropping_sgs)
