#Import of historcial climate covariates

library(tidyr)

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
df.pet$label <- substr(df.pet$label, 0, 10)
df.pet_wide <- spread(df.pet, label, value)
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
rm(swa.covariates.list)
df.swa$label <- row.names(df.swa)
rownames(df.swa) <- c()
df.swa$label <- substr(df.swa$label, 0, 10) #NEED TO MODIFY
df.swa_wide <- spread(df.swa, label, value)
rm(data_wide)

####SWA PET ratio########

SWAPETratio_covariates<-covariates[c(9:12)]
SWAPETratio.covariates.list<-list()

for(i in swa_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  SWAPETratio_covariates.list[[i]] <- make_df
  
}

df.SWAPETratio_covariates<- do.call("rbind", SWAPETratio_covariates.covariates.list)
rm(SWAPETratio_covariates.covariates.list)
df.SWAPETratio$label <- row.names(df.SWAPETratio)
rownames(df.SWAPETratio) <- c()
df.SWAPETratio$label <- substr(df.SWAPETratio$label, 0, 10) # NEED TO MODIFY
df.SWAPETratio_wide <- spread(df.SWAPETratio, label, value)


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
rm(VWC1m_covariates.list)
df.VWC1m$label <- row.names(df.VWC1m)
rownames(df.VWC1m) <- c()
df.VWC1m$label <- substr(df.VWC1m$label, 0, 10) # NEED TO MODIFY
df.VWC1m_wide <- spread(df.VWC1m, label, value)

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
rm(wy_covariates.list)
df.wy$label <- row.names(df.wy)
rownames(df.wy) <- c()
df.wy$label <- substr(df.wy$label, 0, 10) # NEED TO MODIFY
df.wy_wide <- spread(df.wy, label, value)


####transpiration########

transp_covariates<-covariates[c(18:21)]
transp_covariates.list<-list()

for(i in transp_covariates[1:4])
{
  
  test <- get(load(file.path(soil_moisture_dir,i)))
  cleanup_test<-initial_cleanup(test)
  make_df<-raster_link_function_x(cleanup_test)
  transp_covariates.list[[i]] <- make_df
  
}

df.transp<- do.call("rbind", transp_covariates.list)
rm(transp_covariates.list)
df.transp$label <- row.names(df.transp)
rownames(df.transp) <- c()
df.transp$label <- substr(df.wy$label, 0, 10) # NEED TO MODIFY
df.transp_wide <- spread(df.transp, label, value)