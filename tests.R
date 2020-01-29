#Import of historcial climate covariates

library(tidyr)


#G:\My Drive\range-resilience\Sensitivity\Preliminary_work\SoilMoisture_Data\SOILWAT_Output_Fall2019
soil_moisture_dir_dir <- dir("G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019") 
covariates<-soil_moisture_dir_dir[-c(1)]
soil_moisture_dir <- "G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data/SOILWAT_Output_Fall2019" 
####potential evapotranspiration #####

pet_covariates<-covariates[c(1:4)]
pet.covariates.list<-list()

for(i in pet_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  pet.covariates.list[[i]] <- make_df
  
}

df.pet<- do.call("rbind", pet.covariates.list)
rm(pet.covariates.list)
df.pet$label <- row.names(df.pet)
rownames(df.pet) <- c()
head(df.pet)
df.pet$label <- substr(df.pet$label, 0, 10)
df.pet_wide <- spread(df.pet, label, value)
head(df.pet_wide)
rm(df.pet)
herbaceous_npp_2<-merge(herbaceous_npp,df.pet_wide,by=c('x','y','year')) #dataframe to merge on to
head(herbaceous_npp_2)
rm(df.pet_wide)
#head(df.pet_wide)
#View(df.pet)


####SWA########

swa_covariates<-covariates[c(5:8)]
swa.covariates.list<-list()

for(i in swa_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  swa.covariates.list[[i]] <- make_df
  
}

df.swa<- do.call("rbind", swa.covariates.list)
head(df.swa)
rm(swa.covariates.list)
df.swa$label <- row.names(df.swa)
rownames(df.swa) <- c()
df.swa$label <- substr(df.swa$label, 0, 10)
df.swa_wide <- spread(df.swa, label, value)
head(df.swa_wide)
rm(df.swa)
herbaceous_npp_3<-merge(herbaceous_npp_2,df.swa_wide,by=c('x','y','year'))
head(herbaceous_npp_3)
rm(herbaceous_npp_2)

####SWA PET ratio########

SWAPETratio_covariates<-covariates[c(9:12)]
SWAPETratio.covariates.list<-list()

for(i in SWAPETratio_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  SWAPETratio.covariates.list[[i]] <- make_df
  
}

df.SWAPETratio_covariates<- do.call("rbind", SWAPETratio.covariates.list)
head(df.SWAPETratio_covariates)
rm(SWAPETratio.covariates.list)
df.SWAPETratio_covariates$label <- row.names(df.SWAPETratio_covariates)
rownames(df.SWAPETratio_covariates) <- c()
df.SWAPETratio_covariates$label <- substr(df.SWAPETratio_covariates$label, 0, 18) # NEED TO MODIFY
df.SWAPETratio_wide <- spread(df.SWAPETratio_covariates, label, value)
rm(df.SWAPETratio_covariates)
head(df.SWAPETratio_wide)
herbaceous_npp_4<-merge(herbaceous_npp_3,df.SWAPETratio_wide,by=c('x','y','year'))
head(herbaceous_npp_4)
rm(herbaceous_npp_3)

####volumetric water content down to 1 meter ########

VWC1m_covariates<-covariates[c(13:16)]
VWC1m_covariates.list<-list()

for(i in VWC1m_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  VWC1m_covariates.list[[i]] <- make_df
  
}

df.VWC1m<- do.call("rbind", VWC1m_covariates.list)
head(df.VWC1m)
rm(VWC1m_covariates.list)
df.VWC1m$label <- row.names(df.VWC1m)
rownames(df.VWC1m) <- c()
df.VWC1m$label <- substr(df.VWC1m$label, 0, 11) # NEED TO MODIFY
df.VWC1m_wide <- spread(df.VWC1m, label, value)
head(df.VWC1m_wide)
rm(df.VWC1m)
herbaceous_npp_5<-merge(herbaceous_npp_4,df.VWC1m_wide,by=c('x','y','year'))
head(herbaceous_npp_5)
rm(herbaceous_npp_4)

#### water year precipitation used for NPP processing########

WatYrPRECIP_covariates<-covariates[c(17)]
WatYrPRECIP_covariates.list<-list()

for(i in WatYrPRECIP_covariates[1])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  WatYrPRECIP_covariates.list[[i]] <- make_df
  
}

df.WatYrPRECIP<-as.data.frame(WatYrPRECIP_covariates.list)
head(df.WatYrPRECIP)
rm(WatYrPRECIP_covariates.list)
names(df.WatYrPRECIP)<- gsub('WatYrPRECIP_ALLregionsHIST.Rdata.', '',names(df.WatYrPRECIP))
colnames(df.WatYrPRECIP)  <- c("x","y","year",'mm')
df.WatYrPRECIP$mm<-df.WatYrPRECIP$mm*10

####Other water year covariates ########

wy_covariates<-covariates[c(18:21)]
wy_covariates.list<-list()

for(i in wy_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  wy_covariates.list[[i]] <- make_df
  
}

df.wy<- do.call("rbind", wy_covariates.list)
head(df.wy)
rm(wy_covariates.list)
df.wy$label <- row.names(df.wy)
rownames(df.wy) <- c()
head(df.wy)
df.wy$label <- substr(df.wy$label, 0, 9) 
df.wy_wide <- spread(df.wy, label, value)
head(df.wy_wide)
rm(df.wy)

herbaceous_npp_6<-merge(herbaceous_npp_5,df.wy_wide,by=c('x','y','year'))
head(herbaceous_npp_6)
rm(herbaceous_npp_5)

#save file for now
saveRDS(herbaceous_npp_6,
        file = 'G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/historical_covariates_herbaceous_npp_1.rds')

####transpiration########

transp_covariates<-covariates[c(22,23,24)]
transp_covariates.list<-list()

for(i in transp_covariates[1:3])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  transp_covariates.list[[i]] <- make_df
  
}

df.transp<- do.call("rbind", transp_covariates.list)
head(df.transp)
rm(transp_covariates.list)
df.transp$label <- row.names(df.transp)
rownames(df.transp) <- c()
df.transp$label <- substr(df.wy$label, 0, 10) # NEED TO MODIFY
df.transp_wide <- spread(df.transp, label, value)
head(df.transp_wide)



test.transp <- get(load(file.path(soil_moisture_dir,"WY_Transp50_ALLregionsHIST.Rdata")))
str(test.transp)
View(test.transp)
cleanup_test_transp<-initial_cleanup(test.transp)
make_df_transp<-raster_link_function_x(cleanup_test_transp)
summary(cleanup_test_transp)
View(cleanup_test_transp)
head(test.transp)
