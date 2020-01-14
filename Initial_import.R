# importing and processing forage production
library(raster)
library(reshape2)
library(tidyr)
library(dplyr)

conus_npp <- dir('G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/Herbaceous net primary productivity data/landsat-6000reduced-partitioned-npp/landsat-6000reduced-partitioned-npp/', full.names = T)
conus_npp_2<-conus_npp[-1]

#create lists to store rasters
band.1.list<-list()
band.2.list<-list()

#loop to generate list of rasters of each band
for(i in conus_npp_2[1:33])
{
  
  band.1.list[[i]] <- raster(i,band=1)
  band.2.list[[i]] <- raster(i,band=2)
}

#change to raster stack for 

#band 1
stack_band_1<-raster::stack(band.1.list)
stack_band_1[stack_band_1>= 65535] <- NA #gets rid of junk values

#band 2
stack_band_2<-raster::stack(band.2.list)
stack_band_2[stack_band_2>= 65535] <- NA

#

#change to dataframe

#band 1
npp_stack_band_1_df<-rasterToPoints(stack_band_1)
head(npp_stack_band_1_df)
npp_stack_band_1_df_2 <- as.data.frame(npp_stack_band_1_df)
head(npp_stack_band_1_df_2)

#band 2
npp_stack_band_2_df<-rasterToPoints(stack_band_2)
head(npp_stack_band_2_df)
npp_stack_band_2_df_2 <- as.data.frame(npp_stack_band_2_df)
head(npp_stack_band_2_df_2)

#wide to long format

#band 1
npp_stack_band_1_df_melted <- melt(npp_stack_band_1_df_2, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
head(npp_stack_band_1_df_melted) #works

#band 2
npp_stack_band_2_df_melted <- melt(npp_stack_band_2_df_2, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
head(npp_stack_band_2_df_melted) 

#clean up the year column to just have year

#band 1
npp_stack_band_1_df_melted$year<-gsub('G..My.Drive.range.resilience.Sensitivity.CONUS_rangelands_NPP_Sensitivity.NPP_Soil_Moisture.Herbaceous.net.primary.productivity.data.landsat.6000reduced.partitioned.npp.landsat.6000reduced.partitioned.npp.landsat.6000.partitioned.npp.',
                                      '', npp_stack_band_1_df_melted$year)

head(npp_stack_band_1_df_melted) #works

npp_stack_band_1_df_melted$year<-gsub('.tif','', npp_stack_band_1_df_melted$year)

head(npp_stack_band_1_df_melted)#works

#band 2
npp_stack_band_2_df_melted$year<-gsub('G..My.Drive.range.resilience.Sensitivity.CONUS_rangelands_NPP_Sensitivity.NPP_Soil_Moisture.Herbaceous.net.primary.productivity.data.landsat.6000reduced.partitioned.npp.landsat.6000reduced.partitioned.npp.landsat.6000.partitioned.npp.',
                                      '', npp_stack_band_2_df_melted$year)

head(npp_stack_band_2_df_melted) 

npp_stack_band_2_df_melted$year<-gsub('.tif','', npp_stack_band_2_df_melted$year)

head(npp_stack_band_2_df_melted)

#merge band 1 and band 2 dataframes
herbaceous_npp_merged <- merge(npp_stack_band_2_df_melted,npp_stack_band_1_df_melted,by=c('x','y','year'))
head(herbaceous_npp_merged)
herbaceous_npp_merged$npp <- (herbaceous_npp_merged$value.x + herbaceous_npp_merged$value.y)/10

#remove value 1 and 2
herbaceous_npp_merged_2<-herbaceous_npp_merged[-c(4,5)]
head(herbaceous_npp_merged_2)

#map this out, see how it looks
herbaceous_npp_mean_uncleaned <- aggregate(npp~x+y,mean,data=herbaceous_npp_merged_2)
head(herbaceous_npp_mean_uncleaned) 

#look at distribution of data
summary(herbaceous_npp_mean_uncleaned)
hist(herbaceous_npp_mean_uncleaned$npp)

#step 1

#filter out all pixels where mean npp is zero
herbaceous_npp_mean_uncleaned_nozeros <-herbaceous_npp_mean_uncleaned %>%
  dplyr::filter(npp > 20) #for now target the minimum mean production as 20 g/m^2
summary(herbaceous_npp_mean_uncleaned_nozeros)
hist(herbaceous_npp_mean_uncleaned_nozeros$npp)
herbaceous_npp_mean_uncleaned_nozeros_raster<-rasterFromXYZ(herbaceous_npp_mean_uncleaned_nozeros)
plot(herbaceous_npp_mean_uncleaned_nozeros_raster)

# step 2
#uploading the raster count file: how many 30 m pixels used in aggregation
count<-'G:/My Drive/range-resilience/Sensitivity/Preliminary_work/Raster_Files/7km_masking_test/landsat-6000-npp-count.tif' #the count rastercount
raster_test <- raster(count)
plot(raster_test)

#get rid of junk data
raster_test[raster_test>= 65535] <- NA
raster_test[raster_test == -1] <-NA

#change to dataframe to subset sites more easily
npp_test_p = rasterToPoints(raster_test); npp_test_df = data.frame(npp_test_p)
head(npp_test_df)
summary(npp_test_df)

#calculating threshold
#50% of all pixels not used (53824 is the max # of pixels used)
.5*max(npp_test_df$landsat.6000.npp.count) #26912

#########create npp 50% masking raster########################## 
#CREATE DIFFERENT NPP RASTERS

#remove all cells with >50% 30m NA values
df_50 <-npp_test_df %>%
  dplyr::filter(landsat.6000.npp.count > 26912)
summary(df_50)
npp_test_raster_50<- rasterFromXYZ(df_50) #change back to raster
plot(npp_test_raster_50)
title('50% threshold')

#50% threshold
#mask out values that didn't meet the 50% theshold
herb_npp_50_threshold = crop(herbaceous_npp_mean_uncleaned_nozeros_raster, npp_test_raster_50) 
herb_npp_50_threshold_2 <-mask(herb_npp_50_threshold, npp_test_raster_50)
plot(herb_npp_50_threshold_2)

test_mask<-rasterFromXYZ(herbaceous_npp_merged_2)
plot(test_mask)
#now we are ready to to region-specific processing.