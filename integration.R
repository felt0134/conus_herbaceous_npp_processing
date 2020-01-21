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


#G:\My Drive\range-resilience\Sensitivity\Preliminary_work\SoilMoisture_Data\SOILWAT_Output_Fall2019
soil_moisture_dir <- dir("G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019") 
soil_moisture_dir <- "G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019"
covariates<-soil_moisture_dir[-c(1)]
load(file.path('G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019/WY_Transp90_ALLregionsHIST.Rdata')) #loads file and name it annualSWA_OctDec I guess
as.data.frame(WatYrprecip)


list.covariates<-list()

for(i in covariates[1:24])
{
  
  list.covariates[[i]] <- get(load(file.path(soil_moisture_dir,i)))
  
}

readRDS()
head(list.covariates[2])
filenames_test<-list.files(soil_moisture_dir,pattern=".Rdata")

test.dir <- "G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019" #set working directory
test.file<-get(load(file.path(test.dir, "PET_AprJun_ALLregionsHIST.Rdata"),ex <- new.env()))
test.file.2<-get(load(file.path(test.dir, "WatYrPRECIP_ALLregionsHIST.Rdata"),stringsAsFactors=FALSE))
head(test.file)
head(test.file.2)
head(aggTABLE_allregions)
?readRDS
?load
names(test.file)
list.covariates[1]