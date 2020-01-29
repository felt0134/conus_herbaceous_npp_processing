#hot deserts cleaning herbaceous NPP

#packages
library(dplyr)
library(reshape2)
library(raster)

#subset to hot deserts layer
sites_df_hot_deserts <-sites_df %>%
  dplyr::filter(layer == 3)

head(sites_df_hot_deserts)

#change to raster
hot_deserts_raster<-rasterFromXYZ(sites_df_hot_deserts)

plot(hot_deserts_raster)

#make an extent for this ecoregion
extent_hot_deserts<-extent(hot_deserts_raster)
hot_deserts_initial_mask_raster<-crop(herb_npp_50_threshold_2,extent_hot_deserts)
plot(hot_deserts_initial_mask_raster)

#mask out non-hot deserts pixels
hot_deserts_initial_mask_raster_2<-mask(hot_deserts_initial_mask_raster,hot_deserts_raster)
plot(hot_deserts_initial_mask_raster_2) #works

merge_hot_deserts_1<-rasterToPoints(hot_deserts_initial_mask_raster_2)
head(merge_hot_deserts_1)
summary(merge_hot_deserts_1)
merge_hot_deserts_2<-merge(merge_hot_deserts_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_hot_deserts_2)
View(merge_hot_deserts_2)

#make year numeric
merge_hot_deserts_2$year <- as.numeric(as.character(merge_hot_deserts_2$year))

#test the means match up
test_means<-aggregate(npp.y~ x + y + npp.x,mean,data=merge_hot_deserts_2)
View(test_means)
plot(npp.x~npp.y,data=test_means) # looks good

#clean up
merge_hot_deserts_3<-merge_hot_deserts_2[-c(3)]
head(merge_hot_deserts_3)
View(merge_hot_deserts_3)
summary(merge_hot_deserts_3)
names(merge_hot_deserts_3)[names(merge_hot_deserts_3) == "npp.y"] <- "npp"

#merge with precipitation data
hot_deserts_npp_mm<-merge(merge_hot_deserts_3,df.WatYrPRECIP,by=c('x','y','year'))
head(hot_deserts_npp_mm)
View(hot_deserts_npp_mm)
summary(hot_deserts_npp_mm)

#look at per-pixel mean RUE
mean_rue_hot_deserts<-hot_deserts_npp_mm %>%
  group_by(x, y) %>%
  summarise_each(funs(mean))

head(mean_rue_hot_deserts)

mean_rue_hot_deserts_2<-as.data.frame(mean_rue_hot_deserts)
head(mean_rue_hot_deserts_2)
plot(npp~mm,data=mean_rue_hot_deserts_2)

mean_rue_hot_deserts_2$pue <- mean_rue_hot_deserts_2$npp/mean_rue_hot_deserts_2$mm
summary(mean_rue_hot_deserts_2)

#set threshold for masking
mean(mean_rue_hot_deserts_2$pue) + 3*sd(mean_rue_hot_deserts_2$pue) #0.6671725
mean(mean_rue_hot_deserts_2$pue) - 3*sd(mean_rue_hot_deserts_2$pue) #below minimum

#isolate values greater than 3sd away for mean precip use efficiency
mean_rue_hot_deserts_2_filtered <- mean_rue_hot_deserts_2 %>%
  dplyr::filter(pue < 0.67) 
summary(mean_rue_hot_deserts_2_filtered)
mean_rue_hot_deserts_2_filtered_2<-mean_rue_hot_deserts_2_filtered[-c(3,4,5)]
head(mean_rue_hot_deserts_2_filtered_2)

#merge npp-ppt datasets all years for dataset to generate slope
mean_rue_hot_deserts_2_filtered_allyears<-merge(mean_rue_hot_deserts_2_filtered_2,hot_deserts_npp_mm,by=c("x","y"))
head(mean_rue_hot_deserts_2_filtered_allyears)
summary(mean_rue_hot_deserts_2_filtered_allyears)
mean_rue_hot_deserts_2_filtered_allyears_2<-mean_rue_hot_deserts_2_filtered_allyears[-c(3,4)]
head(mean_rue_hot_deserts_2_filtered_allyears_2)

#generate temporal slopes
slope_temporal_hot_deserts_herb <- mean_rue_hot_deserts_2_filtered_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(slope_temporal_hot_deserts_herb)
hot_deserts_coef_only_herb<- slope_temporal_hot_deserts_herb[ -c(3) ] #isolate coefficient so only slope is graphed
head(hot_deserts_coef_only_herb)
slope_temporal_hot_deserts_herb_2<-as.data.frame(hot_deserts_coef_only_herb)
head(slope_temporal_hot_deserts_herb_2)
summary(slope_temporal_hot_deserts_herb_2)

#set threshold for masking
mean(slope_temporal_hot_deserts_herb_2$coef) + 3*sd(slope_temporal_hot_deserts_herb_2$coef) #0.42
mean(slope_temporal_hot_deserts_herb_2$coef) - 3*sd(slope_temporal_hot_deserts_herb_2$coef) #below minimum

#isolate values greater than 3sd away for mean precip use efficiency
slope_temporal_hot_deserts_herb_2_filtered <- slope_temporal_hot_deserts_herb_2 %>%
  dplyr::filter(coef < 0.42) 
summary(slope_temporal_hot_deserts_herb_2_filtered)

temporal_slope_hot_deserts_filtered_allyears<-merge(slope_temporal_hot_deserts_herb_2_filtered,hot_deserts_npp_mm,by=c("x","y"))
head(temporal_slope_hot_deserts_filtered_allyears)

#look at yearly precip use efficiency
temporal_slope_hot_deserts_filtered_allyears$pue <- temporal_slope_hot_deserts_filtered_allyears$npp/temporal_slope_hot_deserts_filtered_allyears$mm
summary(temporal_slope_hot_deserts_filtered_allyears)

#set threshold for masking
mean(temporal_slope_hot_deserts_filtered_allyears$pue) + 3*sd(temporal_slope_hot_deserts_filtered_allyears$pue) #1.99
mean(temporal_slope_hot_deserts_filtered_allyears$pue) - 3*sd(temporal_slope_hot_deserts_filtered_allyears$pue) #below minimum

yearly_pue_anamolies_hot_deserts<-filter(temporal_slope_hot_deserts_filtered_allyears,pue > 1.99)
yearly_pue_anamolies_hot_deserts_final<-yearly_pue_anamolies_hot_deserts[-c(3)]
summary(yearly_pue_anamolies_hot_deserts_final)
head(yearly_pue_anamolies_hot_deserts_final)

#make raster for the odd values
yearly_pue_anamolies_hot_deserts_final_2<-yearly_pue_anamolies_hot_deserts_final[-c(3,4,5)]
odd_pue_values_hot_deserts<-rasterFromXYZ(yearly_pue_anamolies_hot_deserts_final_2)
plot(odd_pue_values_hot_deserts)

#final masking raster
temporal_slope_hot_deserts_filtered_allyears_raster<-rasterFromXYZ(slope_temporal_hot_deserts_herb_2_filtered)
plot(temporal_slope_hot_deserts_filtered_allyears_raster)
hot_deserts_masking_extent_2<-crop(temporal_slope_hot_deserts_filtered_allyears_raster,odd_pue_values_hot_deserts)
hot_deserts_masking<-mask(hot_deserts_masking_extent_2,odd_pue_values_hot_deserts,inverse=TRUE) #mask out pixels with bad pue
plot(hot_deserts_masking)

hot_deserts_masking_df<-rasterToPoints(hot_deserts_masking)
summary(hot_deserts_masking_df)

#merge for finally herbaceous NPP dataframe
hot_deserts_final_herb_npp<-merge(hot_deserts_masking_df,hot_deserts_npp_mm,by=c('x','y'))
summary(hot_deserts_final_herb_npp)
hot_deserts_final_herb_npp$rue<-hot_deserts_final_herb_npp$npp/hot_deserts_final_herb_npp$mm
summary(hot_deserts_final_herb_npp)

#final npp for hot deserts
hot_deserts_final_herb_npp_2<-hot_deserts_final_herb_npp[-c(3,7)]
head(hot_deserts_final_herb_npp_2)
hot_deserts_final_herb_npp_2$region <- 'hot_deserts'