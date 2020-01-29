#northern mixed prairies cleaning herbaceous NPP

#packages
library(dplyr)
library(reshape2)
library(raster)

#subset to cold deserts layer
sites_df_northern_mixed <-sites_df %>%
  dplyr::filter(layer == 4)

head(sites_df_northern_mixed)

#change to raster
northern_mixed_raster<-rasterFromXYZ(sites_df_northern_mixed)

plot(northern_mixed_raster)

#make an extent for this ecoregion
extent_northern_mixed<-extent(northern_mixed_raster)
northern_mixed_initial_mask_raster<-crop(herb_npp_50_threshold_2,extent_northern_mixed,snap='in')
northern_mixed_initial_mask_raster_cropped<-crop(northern_mixed_raster,extent(northern_mixed_initial_mask_raster))

plot(northern_mixed_initial_mask_raster_cropped)
plot(northern_mixed_initial_mask_raster)

#mask out non-northern mixed pixels
northern_mixed_initial_mask_raster_2<-mask(northern_mixed_initial_mask_raster,northern_mixed_initial_mask_raster_cropped)
plot(northern_mixed_initial_mask_raster_2) #works

merge_northern_mixed_1<-rasterToPoints(northern_mixed_initial_mask_raster_2)
head(merge_northern_mixed_1)
summary(merge_northern_mixed_1)

#get temporal dataset down to scale of northern mixed
merge_northern_mixed_2<-merge(merge_northern_mixed_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_northern_mixed_2)
View(merge_northern_mixed_2)

#make year numeric
merge_northern_mixed_2$year <- as.numeric(as.character(merge_northern_mixed_2$year))

#test the means match up
test_means<-aggregate(npp.y~ x + y + npp.x,mean,data=merge_northern_mixed_2)
View(test_means)
plot(npp.x~npp.y,data=test_means) # looks good

#clean up
merge_northern_mixed_3<-merge_northern_mixed_2[-c(3)]
head(merge_northern_mixed_3)
View(merge_northern_mixed_3)
summary(merge_northern_mixed_3)
names(merge_northern_mixed_3)[names(merge_northern_mixed_3) == "npp.y"] <- "npp"

#merge with precipitation data
northern_mixed_npp_mm<-merge(merge_northern_mixed_3,df.WatYrPRECIP,by=c('x','y','year'))
head(northern_mixed_npp_mm)
View(northern_mixed_npp_mm)
summary(northern_mixed_npp_mm)

#look at per-pixel mean RUE
mean_rue_northern_mixed<-northern_mixed_npp_mm %>%
  group_by(x, y) %>%
  summarise_each(funs(mean))

head(mean_rue_northern_mixed)

mean_rue_northern_mixed_2<-as.data.frame(mean_rue_northern_mixed)
head(mean_rue_northern_mixed_2)
plot(npp~mm,data=mean_rue_northern_mixed_2)

mean_rue_northern_mixed_2$pue <- mean_rue_northern_mixed_2$npp/mean_rue_northern_mixed_2$mm
summary(mean_rue_northern_mixed_2)

#set threshold for masking
mean(mean_rue_northern_mixed_2$pue) + 3*sd(mean_rue_northern_mixed_2$pue) #0.84
mean(mean_rue_northern_mixed_2$pue) - 3*sd(mean_rue_northern_mixed_2$pue) #0.17

#isolate values greater than 3sd away for mean precip use efficiency
mean_rue_northern_mixed_2_filtered <- mean_rue_northern_mixed_2 %>%
  dplyr::filter(pue < 0.84 & pue > 0.17) 
summary(mean_rue_northern_mixed_2_filtered)
mean_rue_northern_mixed_2_filtered_2<-mean_rue_northern_mixed_2_filtered[-c(3,4,5)]
head(mean_rue_northern_mixed_2_filtered_2)

#merge npp-ppt datasets all years for dataset to generate slope
mean_rue_northern_mixed_2_filtered_allyears<-merge(mean_rue_northern_mixed_2_filtered_2,northern_mixed_npp_mm,by=c("x","y"))
head(mean_rue_northern_mixed_2_filtered_allyears)
summary(mean_rue_northern_mixed_2_filtered_allyears)
mean_rue_northern_mixed_2_filtered_allyears_2<-mean_rue_northern_mixed_2_filtered_allyears[-c(3,4)]
head(mean_rue_northern_mixed_2_filtered_allyears_2)

#generate temporal slopes
slope_temporal_northern_mixed_herb <- mean_rue_northern_mixed_2_filtered_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(slope_temporal_northern_mixed_herb)
northern_mixed_coef_only_herb<- slope_temporal_northern_mixed_herb[ -c(3) ] #isolate coefficient so only slope is graphed
head(northern_mixed_coef_only_herb)
slope_temporal_northern_mixed_herb_2<-as.data.frame(northern_mixed_coef_only_herb)
head(slope_temporal_northern_mixed_herb_2)
summary(slope_temporal_northern_mixed_herb_2)
hist(slope_temporal_northern_mixed_herb_2$coef)

#set threshold for masking
mean(slope_temporal_northern_mixed_herb_2$coef) + 3*sd(slope_temporal_northern_mixed_herb_2$coef) #0.45
mean(slope_temporal_northern_mixed_herb_2$coef) - 3*sd(slope_temporal_northern_mixed_herb_2$coef) #-0.011

#isolate values greater than 3sd away for mean precip use efficiency
slope_temporal_northern_mixed_herb_2_filtered <- slope_temporal_northern_mixed_herb_2 %>%
  dplyr::filter(coef < 0.45 & coef > -0.011) 
summary(slope_temporal_northern_mixed_herb_2_filtered)

temporal_slope_northern_mixed_filtered_allyears<-merge(slope_temporal_northern_mixed_herb_2_filtered,northern_mixed_npp_mm,by=c("x","y"))
head(temporal_slope_northern_mixed_filtered_allyears)

#look at yearly precip use efficiency
temporal_slope_northern_mixed_filtered_allyears$pue <- temporal_slope_northern_mixed_filtered_allyears$npp/temporal_slope_northern_mixed_filtered_allyears$mm
summary(temporal_slope_northern_mixed_filtered_allyears)

#set threshold for masking
mean(temporal_slope_northern_mixed_filtered_allyears$pue) + 3*sd(temporal_slope_northern_mixed_filtered_allyears$pue) #1
mean(temporal_slope_northern_mixed_filtered_allyears$pue) - 3*sd(temporal_slope_northern_mixed_filtered_allyears$pue) #0.05

yearly_pue_anamolies_northern_mixed <- temporal_slope_northern_mixed_filtered_allyears %>%
  dplyr::filter(pue < 0.05 |pue > 1) 
summary(yearly_pue_anamolies_northern_mixed)

yearly_pue_anamolies_northern_mixed_final<-yearly_pue_anamolies_northern_mixed[-c(3)]
summary(yearly_pue_anamolies_northern_mixed_final)
hist(yearly_pue_anamolies_northern_mixed_final$pue)
head(yearly_pue_anamolies_northern_mixed_final)

#make raster for the odd values
yearly_pue_anamolies_northern_mixed_final_2<-yearly_pue_anamolies_northern_mixed_final[-c(3,4,5)]
odd_pue_values_northern_mixed<-rasterFromXYZ(yearly_pue_anamolies_northern_mixed_final_2)
plot(odd_pue_values_northern_mixed)

#final masking raster
temporal_slope_northern_mixed_filtered_allyears_raster<-rasterFromXYZ(slope_temporal_northern_mixed_herb_2_filtered)
plot(temporal_slope_northern_mixed_filtered_allyears_raster)
northern_mixed_masking_extent_2<-crop(temporal_slope_northern_mixed_filtered_allyears_raster,odd_pue_values_northern_mixed)
northern_mixed_masking<-mask(northern_mixed_masking_extent_2,odd_pue_values_northern_mixed,inverse=TRUE) #mask out pixels with bad pue
plot(northern_mixed_masking)

northern_mixed_masking_df<-rasterToPoints(northern_mixed_masking)
summary(northern_mixed_masking_df)

#merge for finally herbaceous NPP dataframe
northern_mixed_final_herb_npp<-merge(northern_mixed_masking_df,northern_mixed_npp_mm,by=c('x','y'))
summary(northern_mixed_final_herb_npp)
northern_mixed_final_herb_npp$rue<-northern_mixed_final_herb_npp$npp/northern_mixed_final_herb_npp$mm
summary(northern_mixed_final_herb_npp)

#final npp for cold deserts
northern_mixed_final_herb_npp_2<-northern_mixed_final_herb_npp[-c(3,7)]
head(northern_mixed_final_herb_npp_2)
northern_mixed_final_herb_npp_2$region <- 'northern_mixed_prairies'