#cold deserts data processing
#cold deserts cleaning herbaceous NPP

#packages
library(dplyr)
library(reshape2)
library(raster)

#subset to cold deserts layer
sites_df_cold_deserts <-sites_df %>%
  dplyr::filter(layer == 2)

head(sites_df_cold_deserts)

#change to raster
cold_deserts_raster<-rasterFromXYZ(sites_df_cold_deserts)

plot(cold_deserts_raster)

#make an extent for this ecoregion
extent_cold_deserts<-extent(cold_deserts_raster)
cold_deserts_initial_mask_raster<-crop(herb_npp_50_threshold_2,extent_cold_deserts)
plot(cold_deserts_initial_mask_raster)

#mask out non-cold deserts pixels
cold_deserts_initial_mask_raster_2<-mask(cold_deserts_initial_mask_raster,cold_deserts_raster)
plot(cold_deserts_initial_mask_raster_2) #works

merge_cold_deserts_1<-rasterToPoints(cold_deserts_initial_mask_raster_2)
head(merge_cold_deserts_1)
summary(merge_cold_deserts_1)

#get temporal dataset down to scale of cold deserts
merge_cold_deserts_2<-merge(merge_cold_deserts_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_cold_deserts_2)
View(merge_cold_deserts_2)

#make year numeric
merge_cold_deserts_2$year <- as.numeric(as.character(merge_cold_deserts_2$year))

#test the means match up
test_means<-aggregate(npp.y~ x + y + npp.x,mean,data=merge_cold_deserts_2)
View(test_means)
plot(npp.x~npp.y,data=test_means) # looks good

#clean up
merge_cold_deserts_3<-merge_cold_deserts_2[-c(3)]
head(merge_cold_deserts_3)
View(merge_cold_deserts_3)
summary(merge_cold_deserts_3)
names(merge_cold_deserts_3)[names(merge_cold_deserts_3) == "npp.y"] <- "npp"

#merge with precipitation data
cold_deserts_npp_mm<-merge(merge_cold_deserts_3,precip_stack_df_melted_2,by=c('x','y','year'))
head(cold_deserts_npp_mm)
View(cold_deserts_npp_mm)
summary(cold_deserts_npp_mm)

#look at per-pixel mean RUE
mean_rue_cold_deserts<-cold_deserts_npp_mm %>%
  group_by(x, y) %>%
  summarise_each(funs(mean))

head(mean_rue_cold_deserts)

mean_rue_cold_deserts_2<-as.data.frame(mean_rue_cold_deserts)
head(mean_rue_cold_deserts_2)
plot(npp~mm,data=mean_rue_cold_deserts_2)

mean_rue_cold_deserts_2$pue <- mean_rue_cold_deserts_2$npp/mean_rue_cold_deserts_2$mm
summary(mean_rue_cold_deserts_2)

#set threshold for masking
mean(mean_rue_cold_deserts_2$pue) + 3*sd(mean_rue_cold_deserts_2$pue) #0.80
mean(mean_rue_cold_deserts_2$pue) - 3*sd(mean_rue_cold_deserts_2$pue)

#isolate values greater than 3sd away for mean precip use efficiency
mean_rue_cold_deserts_2_filtered <- mean_rue_cold_deserts_2 %>%
  dplyr::filter(pue < 0.80) 
summary(mean_rue_cold_deserts_2_filtered)
mean_rue_cold_deserts_2_filtered_2<-mean_rue_cold_deserts_2_filtered[-c(3,4,5)]
head(mean_rue_cold_deserts_2_filtered_2)

#merge npp-ppt datasets all years for dataset to generate slope
mean_rue_cold_deserts_2_filtered_allyears<-merge(mean_rue_cold_deserts_2_filtered_2,cold_deserts_npp_mm,by=c("x","y"))
head(mean_rue_cold_deserts_2_filtered_allyears)
summary(mean_rue_cold_deserts_2_filtered_allyears)
mean_rue_cold_deserts_2_filtered_allyears_2<-mean_rue_cold_deserts_2_filtered_allyears[-c(3,4)]
head(mean_rue_cold_deserts_2_filtered_allyears_2)

#generate temporal slopes
slope_temporal_cold_deserts_herb <- mean_rue_cold_deserts_2_filtered_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(slope_temporal_cold_deserts_herb)
cold_deserts_coef_only_herb<- slope_temporal_cold_deserts_herb[ -c(3) ] #isolate coefficient so only slope is graphed
head(cold_deserts_coef_only_herb)
slope_temporal_cold_deserts_herb_2<-as.data.frame(cold_deserts_coef_only_herb)
head(slope_temporal_cold_deserts_herb_2)
summary(slope_temporal_cold_deserts_herb_2)
hist(slope_temporal_cold_deserts_herb_2$coef)
#set threshold for masking
mean(slope_temporal_cold_deserts_herb_2$coef) + 3*sd(slope_temporal_cold_deserts_herb_2$coef) #0.35
mean(slope_temporal_cold_deserts_herb_2$coef) - 3*sd(slope_temporal_cold_deserts_herb_2$coef)

#isolate values greater than 3sd away for mean precip use efficiency
slope_temporal_cold_deserts_herb_2_filtered <- slope_temporal_cold_deserts_herb_2 %>%
  dplyr::filter(coef < 0.35 & coef > -0.11) 
summary(slope_temporal_cold_deserts_herb_2_filtered)

temporal_slope_cold_deserts_filtered_allyears<-merge(slope_temporal_cold_deserts_herb_2_filtered,cold_deserts_npp_mm,by=c("x","y"))
head(temporal_slope_cold_deserts_filtered_allyears)

#look at yearly precip use efficiency
temporal_slope_cold_deserts_filtered_allyears$pue <- temporal_slope_cold_deserts_filtered_allyears$npp/temporal_slope_cold_deserts_filtered_allyears$mm
summary(temporal_slope_cold_deserts_filtered_allyears)

#set threshold for masking
mean(temporal_slope_cold_deserts_filtered_allyears$pue) + 3*sd(temporal_slope_cold_deserts_filtered_allyears$pue) #0.81
mean(temporal_slope_cold_deserts_filtered_allyears$pue) - 3*sd(temporal_slope_cold_deserts_filtered_allyears$pue) #-0.2

yearly_pue_anamolies_cold_deserts<-filter(temporal_slope_cold_deserts_filtered_allyears,pue > 0.81)
summary(yearly_pue_anamolies_cold_deserts)

yearly_pue_anamolies_cold_deserts_final<-yearly_pue_anamolies_cold_deserts[-c(3)]
summary(yearly_pue_anamolies_cold_deserts_final)
head(yearly_pue_anamolies_cold_deserts_final)

#make raster for the odd values
yearly_pue_anamolies_cold_deserts_final_2<-yearly_pue_anamolies_cold_deserts_final[-c(3,4,5)]
odd_pue_values_cold_deserts<-rasterFromXYZ(yearly_pue_anamolies_cold_deserts_final_2)
plot(odd_pue_values_cold_deserts)

#final masking raster
temporal_slope_cold_deserts_filtered_allyears_raster<-rasterFromXYZ(slope_temporal_cold_deserts_herb_2_filtered)
plot(temporal_slope_cold_deserts_filtered_allyears_raster)
cold_deserts_masking_extent_2<-crop(temporal_slope_cold_deserts_filtered_allyears_raster,odd_pue_values_cold_deserts)
cold_deserts_masking<-mask(cold_deserts_masking_extent_2,odd_pue_values_cold_deserts,inverse=TRUE) #mask out pixels with bad pue
plot(cold_deserts_masking)

cold_deserts_masking_df<-rasterToPoints(cold_deserts_masking)
summary(cold_deserts_masking_df)

#merge for finally herbaceous NPP dataframe
cold_deserts_final_herb_npp<-merge(cold_deserts_masking_df,cold_deserts_npp_mm,by=c('x','y'))
summary(cold_deserts_final_herb_npp)
cold_deserts_final_herb_npp$rue<-cold_deserts_final_herb_npp$npp/cold_deserts_final_herb_npp$mm
summary(cold_deserts_final_herb_npp)

#final npp for cold deserts
cold_deserts_final_herb_npp_2<-cold_deserts_final_herb_npp[-c(3,7)]
head(cold_deserts_final_herb_npp_2)
cold_deserts_final_herb_npp_2$region <- 'cold_deserts'