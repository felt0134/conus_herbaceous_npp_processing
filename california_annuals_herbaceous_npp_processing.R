# california annual grasslands herbaceous npp processing

#packages
library(dplyr)
library(reshape2)
library(raster)

#subset to cold deserts layer
sites_df_cali <-sites_df %>%
  dplyr::filter(layer == 1)

head(sites_df_cali)

#change to raster
cali_raster<-rasterFromXYZ(sites_df_cali)

plot(cali_raster)

#make an extent for this ecoregion
extent_cali<-extent(cali_raster)
cali_initial_mask_raster<-crop(herb_npp_50_threshold_2,extent_cali)
plot(cali_initial_mask_raster)

#mask out non-california pixels
cali_initial_mask_raster_2<-mask(cali_initial_mask_raster,cali_raster)
plot(cali_initial_mask_raster_2) #works

merge_cali_1<-rasterToPoints(cali_initial_mask_raster_2)
head(merge_cali_1)
summary(merge_cali_1)

#get temporal dataset down to scale of cali
merge_cali_2<-merge(merge_cali_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_cali_2)
View(merge_cali_2)

#make year numeric
merge_cali_2$year <- as.numeric(as.character(merge_cali_2$year))

#test the means match up
test_means<-aggregate(npp.y~ x + y + npp.x,mean,data=merge_cali_2)
View(test_means)
plot(npp.x~npp.y,data=test_means) # looks good

#clean up
merge_cali_3<-merge_cali_2[-c(3)]
head(merge_cali_3)
View(merge_cali_3)
summary(merge_cali_3)
names(merge_cali_3)[names(merge_cali_3) == "npp.y"] <- "npp"

#merge with precipitation data
cali_npp_mm<-merge(merge_cali_3,df.WatYrPRECIP,by=c('x','y','year'))
head(cali_npp_mm)
View(cali_npp_mm)
summary(cali_npp_mm)

#look at per-pixel mean RUE
mean_rue_cali<-cali_npp_mm %>%
  group_by(x, y) %>%
  summarise_each(funs(mean))

head(mean_rue_cali)

mean_rue_cali_2<-as.data.frame(mean_rue_cali)
head(mean_rue_cali_2)
plot(npp~mm,data=mean_rue_cali_2)

mean_rue_cali_2$pue <- mean_rue_cali_2$npp/mean_rue_cali_2$mm
summary(mean_rue_cali_2)

#set threshold for masking
mean(mean_rue_cali_2$pue) + 3*sd(mean_rue_cali_2$pue) #1.32
mean(mean_rue_cali_2$pue) - 3*sd(mean_rue_cali_2$pue) #below minimum

#isolate values greater than 3sd away for mean precip use efficiency
mean_rue_cali_2_filtered <- mean_rue_cali_2 %>%
  dplyr::filter(pue < 1.32) 
summary(mean_rue_cali_2_filtered)
mean_rue_cali_2_filtered_2<-mean_rue_cali_2_filtered[-c(3,4,5)]
head(mean_rue_cali_2_filtered_2)

#merge npp-ppt datasets all years for dataset to generate slope
mean_rue_cali_2_filtered_allyears<-merge(mean_rue_cali_2_filtered_2,cali_npp_mm,by=c("x","y"))
head(mean_rue_cali_2_filtered_allyears)
summary(mean_rue_cali_2_filtered_allyears)
mean_rue_cali_2_filtered_allyears_2<-mean_rue_cali_2_filtered_allyears[-c(3,4)]
head(mean_rue_cali_2_filtered_allyears_2)

#generate temporal slopes
slope_temporal_cali_herb <- mean_rue_cali_2_filtered_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(slope_temporal_cali_herb)
cali_coef_only_herb<- slope_temporal_cali_herb[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only_herb)
slope_temporal_cali_herb_2<-as.data.frame(cali_coef_only_herb)
head(slope_temporal_cali_herb_2)
summary(slope_temporal_cali_herb_2)
hist(slope_temporal_cali_herb_2$coef)

#set threshold for masking
mean(slope_temporal_cali_herb_2$coef) + 3*sd(slope_temporal_cali_herb_2$coef) #0.46
mean(slope_temporal_cali_herb_2$coef) - 3*sd(slope_temporal_cali_herb_2$coef) # below minimum

#isolate values greater than 3sd away for mean precip use efficiency
slope_temporal_cali_herb_2_filtered <- slope_temporal_cali_herb_2 %>%
  dplyr::filter(coef < 0.46) 
summary(slope_temporal_cali_herb_2_filtered)

temporal_slope_cali_filtered_allyears<-merge(slope_temporal_cali_herb_2_filtered,cali_npp_mm,by=c("x","y"))
head(temporal_slope_cali_filtered_allyears)

#look at yearly precip use efficiency
temporal_slope_cali_filtered_allyears$pue <- temporal_slope_cali_filtered_allyears$npp/temporal_slope_cali_filtered_allyears$mm
summary(temporal_slope_cali_filtered_allyears)

#set threshold for masking
mean(temporal_slope_cali_filtered_allyears$pue) + 3*sd(temporal_slope_cali_filtered_allyears$pue) #1.91
mean(temporal_slope_cali_filtered_allyears$pue) - 3*sd(temporal_slope_cali_filtered_allyears$pue) #below minimum

yearly_pue_anamolies_cali <- temporal_slope_cali_filtered_allyears %>%
  dplyr::filter(pue > 1.91) 
summary(yearly_pue_anamolies_cali)

yearly_pue_anamolies_cali_final<-yearly_pue_anamolies_cali[-c(3)]
summary(yearly_pue_anamolies_cali_final)
hist(yearly_pue_anamolies_cali_final$pue)
head(yearly_pue_anamolies_cali_final)

#make raster for the odd values
yearly_pue_anamolies_cali_final_2<-yearly_pue_anamolies_cali_final[-c(3,4,5)]
odd_pue_values_cali<-rasterFromXYZ(yearly_pue_anamolies_cali_final_2)
plot(odd_pue_values_cali)

#final masking raster
temporal_slope_cali_filtered_allyears_raster<-rasterFromXYZ(slope_temporal_cali_herb_2_filtered)
plot(temporal_slope_cali_filtered_allyears_raster)
cali_masking_extent_2<-crop(temporal_slope_cali_filtered_allyears_raster,odd_pue_values_cali)
cali_masking<-mask(cali_masking_extent_2,odd_pue_values_cali,inverse=TRUE) #mask out pixels with bad pue
plot(cali_masking)

cali_masking_df<-rasterToPoints(cali_masking)
summary(cali_masking_df)

#merge for finally herbaceous NPP dataframe
cali_final_herb_npp<-merge(cali_masking_df,cali_npp_mm,by=c('x','y'))
summary(cali_final_herb_npp)
cali_final_herb_npp$rue<-cali_final_herb_npp$npp/cali_final_herb_npp$mm
summary(cali_final_herb_npp)

#final npp for cold deserts
cali_final_herb_npp_2<-cali_final_herb_npp[-c(3,7)]
head(cali_final_herb_npp_2)
cali_final_herb_npp_2$region <- 'california_annuals'