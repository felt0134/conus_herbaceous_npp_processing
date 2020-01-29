#merge NPP into one dataset
hd_nmp<-rbind(hot_deserts_final_herb_npp_2,northern_mixed_final_herb_npp_2)
head(hd_nmp)
hd_nmp_ca<-rbind(hd_nmp,cali_final_herb_npp_2)
hd_nmp_ca_cd<-rbind(hd_nmp_ca,cold_deserts_final_herb_npp_2)
all_regions_npp<-rbind(hd_nmp_ca_cd,sgs_final_herb_npp_2)
summary(all_regions_npp)
View(all_regions_npp)
unique(all_regions_npp$region)

#look at map
mean_rangeland_npp<-aggregate(npp~x+y,mean,data=all_regions_npp)
mean_rangeland_npp_raster<-rasterFromXYZ(mean_rangeland_npp)
plot(mean_rangeland_npp_raster)

#save as a dataframe
saveRDS(all_regions_npp,
        file = 'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/herbaceous_npp_historical_1985_2015.rds')

herbaceous_npp<-readRDS('G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/herbaceous_npp_historical_1985_2015.rds')
head(herbaceous_npp)
