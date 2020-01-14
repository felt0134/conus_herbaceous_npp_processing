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
merge_hot_deserts_2<-merge(merge_hot_deserts_1,herbaceous_npp_merged_2,by=c('x','y'))
head(merge_hot_deserts_2)
View(merge_hot_deserts_2)

merge_hot_deserts_3<-merge_hot_deserts_2[-c(3)]
head(merge_hot_deserts_3)
