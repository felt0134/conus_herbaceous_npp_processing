#import of water-year precipitation data and ecoregion boundaries

library(raster)
library(plyr)
library(dplyr)
library(reshape2)

########### ecoregion-identifying raster import  #############
sites <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/climate_data_for_import/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

#raster math
raster_sites_rounded <- round(raster_sites/1000000)
plot(raster_sites_rounded) #see what it looks like

#Create a raster for the different regions, I guess this makes the colors more clear?
AFRI_Site_raster <- raster_sites_rounded - raster_sites*1000000
plot(AFRI_Site_raster)

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")

#ecoregion raster
raster_sites<-raster(sites)
plot(raster_sites)

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(raster_sites))
names(rastvals) <- "RegionSite"

#View just the values associated with each cell
#Note that the value in the raster includes both the region (in the millions digit; 1 to 5), and the siteID (in the other digits, range 1 to ~20,000 within each region)
values(raster_sites)

#Plot the raster
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- round(raster_sites/1000000)
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- raster_sites - raster_sites*1000000
plot(raster_sites)

#turn into dataframe
sites_p = rasterToPoints(raster_sites_rounded); sites_df = data.frame(sites_p)
head(sites_df)

#1 = california annuals
#2 = cold deserts
#3 = hot deserts
#4 = northern mixed grass parairies
#5 = shortgrass steppe

#climate variable directory
dir.AFRI_Historical <- "G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data" #set working directory
load(file.path(dir.AFRI_Historical, "aggTABLE_allregions.Rdata")) 
head(aggTABLE_allregions)

#######water year total precipitation########
load(file.path(dir.AFRI_Historical, "WatYrprecip19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(WatYrprecip)
View(WatYrprecip)

#prcoess the dataframe so it can be merged with the ecoregion raster
WatYrprecip <- WatYrprecip[,-102]
WatYrprecip$label <- row.names(WatYrprecip)

#remove the excess sites not in the core 5 rangeland types

#to remove
WatYrprecip$Regionname <- substr(WatYrprecip$label, 8, 9)
WatYrprecip <- dplyr::filter(WatYrprecip, Regionname != "De") #Remove excess site values
unique(WatYrprecip$Regionname)#it works!
WatYrprecip <- WatYrprecip[,-103]

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', WatYrprecip$label) )
Site <- as.integer(substr(WatYrprecip$label, 1, sitenumENDpos-1) )
Regionname <- substr(WatYrprecip$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
WatYrprecip$RegionSite <- Regionnum*1000000 + Site

WatYrprecip_joindat <- join(rastvals, WatYrprecip, by="RegionSite")
dim(WatYrprecip_joindat)
head(WatYrprecip)
View(WatYrprecip_joindat)
#get rid of years that don't relate to NPP data: isolate 1986-2015
WatYrprecip_joindat_2<-WatYrprecip_joindat[,-c(2:71)]
dim(WatYrprecip_joindat_2)
str(WatYrprecip_joindat_2)
View(WatYrprecip_joindat_2)

WatYrprecip_done <- list()
#1986
WatYrprecip_1986<- raster_sites
values(WatYrprecip_1986) <- WatYrprecip_joindat_2[,"1986"]
WatYrprecip_done[["WatYrprecip_1986"]] <-WatYrprecip_1986
#1987
WatYrprecip_1987<- raster_sites
values(WatYrprecip_1987) <- WatYrprecip_joindat_2[,"1987"]
WatYrprecip_done[["WatYrprecip_1987"]] <-WatYrprecip_1987
#1988
WatYrprecip_1988<- raster_sites
values(WatYrprecip_1988) <- WatYrprecip_joindat_2[,"1988"]
WatYrprecip_done[["WatYrprecip_1988"]] <-WatYrprecip_1988
#1989
WatYrprecip_1989<- raster_sites
values(WatYrprecip_1989) <- WatYrprecip_joindat_2[,"1989"]
WatYrprecip_done[["WatYrprecip_1989"]] <-WatYrprecip_1989
#1990
WatYrprecip_1990<- raster_sites
values(WatYrprecip_1990) <- WatYrprecip_joindat_2[,"1990"]
WatYrprecip_done[["WatYrprecip_1990"]] <-WatYrprecip_1990
#1991
WatYrprecip_1991<- raster_sites
values(WatYrprecip_1991) <- WatYrprecip_joindat_2[,"1991"]
WatYrprecip_done[["WatYrprecip_1991"]] <-WatYrprecip_1991
#1992
WatYrprecip_1992<- raster_sites
values(WatYrprecip_1992) <- WatYrprecip_joindat_2[,"1992"]
WatYrprecip_done[["WatYrprecip_1992"]] <-WatYrprecip_1992
#1993
WatYrprecip_1993<- raster_sites
values(WatYrprecip_1993) <- WatYrprecip_joindat_2[,"1993"]
WatYrprecip_done[["WatYrprecip_1993"]] <-WatYrprecip_1993
#1994
WatYrprecip_1994<- raster_sites
values(WatYrprecip_1994) <- WatYrprecip_joindat_2[,"1994"]
WatYrprecip_done[["WatYrprecip_1994"]] <-WatYrprecip_1994
#1995
WatYrprecip_1995<- raster_sites
values(WatYrprecip_1995) <- WatYrprecip_joindat_2[,"1995"]
WatYrprecip_done[["WatYrprecip_1995"]] <-WatYrprecip_1995
#1996
WatYrprecip_1996<- raster_sites
values(WatYrprecip_1996) <- WatYrprecip_joindat_2[,"1996"]
WatYrprecip_done[["WatYrprecip_1996"]] <-WatYrprecip_1996
#1997
WatYrprecip_1997<- raster_sites
values(WatYrprecip_1997) <- WatYrprecip_joindat_2[,"1997"]
WatYrprecip_done[["WatYrprecip_1997"]] <-WatYrprecip_1997
#1998
WatYrprecip_1998<- raster_sites
values(WatYrprecip_1998) <- WatYrprecip_joindat_2[,"1998"]
WatYrprecip_done[["WatYrprecip_1998"]] <-WatYrprecip_1998
#1999
WatYrprecip_1999<- raster_sites
values(WatYrprecip_1999) <- WatYrprecip_joindat_2[,"1999"]
WatYrprecip_done[["WatYrprecip_1999"]] <-WatYrprecip_1999
#2000
WatYrprecip_2000<- raster_sites
values(WatYrprecip_2000) <- WatYrprecip_joindat_2[,"2000"]
WatYrprecip_done[["WatYrprecip_2000"]] <-WatYrprecip_2000
#2001
WatYrprecip_2001<- raster_sites
values(WatYrprecip_2001) <- WatYrprecip_joindat_2[,"2001"]
WatYrprecip_done[["WatYrprecip_2001"]] <-WatYrprecip_2001
#2002
WatYrprecip_2002<- raster_sites
values(WatYrprecip_2002) <- WatYrprecip_joindat_2[,"2002"]
WatYrprecip_done[["WatYrprecip_2002"]] <-WatYrprecip_2002
#2003
WatYrprecip_2003<- raster_sites
values(WatYrprecip_2003) <- WatYrprecip_joindat_2[,"2003"]
WatYrprecip_done[["WatYrprecip_2003"]] <-WatYrprecip_2003
#2004
WatYrprecip_2004<- raster_sites
values(WatYrprecip_2004) <- WatYrprecip_joindat_2[,"2004"]
WatYrprecip_done[["WatYrprecip_2004"]] <-WatYrprecip_2004
#2005
WatYrprecip_2005<- raster_sites
values(WatYrprecip_2005) <- WatYrprecip_joindat_2[,"2005"]
WatYrprecip_done[["WatYrprecip_2005"]] <-WatYrprecip_2005
#2006
WatYrprecip_2006<- raster_sites
values(WatYrprecip_2006) <- WatYrprecip_joindat_2[,"2006"]
WatYrprecip_done[["WatYrprecip_2006"]] <-WatYrprecip_2006
#2007
WatYrprecip_2007<- raster_sites
values(WatYrprecip_2007) <- WatYrprecip_joindat_2[,"2007"]
WatYrprecip_done[["WatYrprecip_2007"]] <-WatYrprecip_2007
#2008
WatYrprecip_2008<- raster_sites
values(WatYrprecip_2008) <- WatYrprecip_joindat_2[,"2008"]
WatYrprecip_done[["WatYrprecip_2008"]] <-WatYrprecip_2008
#2009
WatYrprecip_2009<- raster_sites
values(WatYrprecip_2009) <- WatYrprecip_joindat_2[,"2009"]
WatYrprecip_done[["WatYrprecip_2009"]] <-WatYrprecip_2009
#2010
WatYrprecip_2010<- raster_sites
values(WatYrprecip_2010) <- WatYrprecip_joindat_2[,"2010"]
WatYrprecip_done[["WatYrprecip_2010"]] <-WatYrprecip_2010
#2011
WatYrprecip_2011<- raster_sites
values(WatYrprecip_2011) <- WatYrprecip_joindat_2[,"2011"]
WatYrprecip_done[["WatYrprecip_2011"]] <-WatYrprecip_2011
#2012
WatYrprecip_2012<- raster_sites
values(WatYrprecip_2012) <- WatYrprecip_joindat_2[,"2012"]
WatYrprecip_done[["WatYrprecip_2012"]] <-WatYrprecip_2012
#2013
WatYrprecip_2013<- raster_sites
values(WatYrprecip_2013) <- WatYrprecip_joindat_2[,"2013"]
WatYrprecip_done[["WatYrprecip_2013"]] <-WatYrprecip_2013
#2014
WatYrprecip_2014<- raster_sites
values(WatYrprecip_2014) <- WatYrprecip_joindat_2[,"2014"]
WatYrprecip_done[["WatYrprecip_2014"]] <-WatYrprecip_2014
#2015
WatYrprecip_2015<- raster_sites
values(WatYrprecip_2015) <- WatYrprecip_joindat_2[,"2015"]
WatYrprecip_done[["WatYrprecip_2015"]] <-WatYrprecip_2015

#all years stacked, ready for cropping for each site
WatYrprecip_stack <-stack(WatYrprecip_done)
plot(WatYrprecip_stack)

# turn this raster stack into a dataframe that can be merged with NPP later on...

precip_stack_df<-rasterToPoints(WatYrprecip_stack)
head(precip_stack_df)

precip_stack_df_2 <- as.data.frame(precip_stack_df)
head(precip_stack_df_2)

#melt to long
precip_stack_df_melted <- melt(precip_stack_df_2, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
head(precip_stack_df_melted) 

precip_stack_df_melted$year<-gsub('WatYrprecip_','', precip_stack_df_melted$year)

head(precip_stack_df_melted)
precip_stack_df_melted$mm<-precip_stack_df_melted$value*10 #convert from cm to mm
summary(precip_stack_df_melted)
precip_stack_df_melted_2<-precip_stack_df_melted[-4]
head(precip_stack_df_melted_2)
summary(precip_stack_df_melted_2)
precip_stack_df_melted_2$year <- as.numeric(as.character(precip_stack_df_melted_2$year))
View(precip_stack_df_melted_2)


##test code to automate###########

for(i in 3:ncol(WatYrprecip_joindat_2)) {
  WatYrprecip<- raster_sites
  values(WatYrprecip) <- WatYrprecip_joindat_2[,'i']
  WatYrprecip_done[[i]] <- WatYrprecip
  # do stuff with row
}

for(i in 3:ncol(WatYrprecip_joindat_2)) {

  WatYrprecip_done[[i]] <- WatYrprecip_joindat_2[,i]
  # do stuff with row
}

WatYrprecip_done_2 <- list()

for(i in WatYrprecip_done[1:33])

  {
  values(WatYrprecip) <- values(WatYrprecip_done)[i]
  #WatYrprecip_done_2[[i]] <- WatYrprecip
  #band.2.list[[i]] <- raster(i,band=2)
}


#1986
WatYrprecip_1986<- raster_sites
values(WatYrprecip_1986) <- WatYrprecip_joindat_2[,"1987"]
WatYrprecip_done[["WatYrprecip_1986"]] <- WatYrprecip_1986
View(WatYrprecip_1986)
plot(WatYrprecip_1986)

WatYrprecip_joindat_2[1986]

for(i in WatYrprecip_joindat_2[1986:2015])
{
  
  band.1.list[[i]] <- raster(i,band=1)
  band.2.list[[i]] <- raster(i,band=2)
}
