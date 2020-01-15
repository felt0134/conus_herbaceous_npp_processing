#merge NPP into one dataset
hd_nmp<-rbind(hot_deserts_final_herb_npp_2,northern_mixed_final_herb_npp_2)
head(hd_nmp)
hd_nmp_ca<-rbind(hd_nmp,cali_final_herb_npp_2)
hd_nmp_ca_cd<-rbind(hd_nmp_ca,cold_deserts_final_herb_npp_2)
all_regions_npp<-rbind(hd_nmp_ca_cd,sgs_final_herb_npp_2)
summary(all_regions_npp)
View(all_regions_npp)

#look at map
mean_rangeland_npp<-aggregate(npp~x+y,mean,data=all_regions_npp)
mean_rangeland_npp_raster<-rasterFromXYZ(mean_rangeland_npp)
plot(mean_rangeland_npp_raster)
