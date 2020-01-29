#FUNCTIONS
library(dmm)
library(varhandle)

########regions###########
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
View(rastvals)
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

#function just to get the last four characters of the labels
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#function for initial cleanup
initial_cleanup<- function(x) {
  
  #test.df.sgs<-as.data.frame(sgs.covariates.list[1]) #need to fix this
  
  #clean up
  rownames(x) <- c()
  names(x) <- substrRight(names(x),4)
  x$label<-x$site
  x<-x[-1]
  #View(test.df.sgs)
  
  #integrate with
  x$Regionname <- substr(x$label, 8, 9)
  x <- dplyr::filter(x, Regionname != "De") #Remove excess site values
  #unique(test.df.sgs$Regionname)
  #View(test.df.sgs_2)
  x_2 <- x[,-103]
  x_3<-unfactor(x_2)
  
  #Creating unique IDs
  sitenumENDpos = as.integer(regexpr('_', x_3$label) )
  Site <- as.integer(substr(x_3$label, 1, sitenumENDpos-1) )
  Regionname <- substr(x_3$label, 8, 9)
  Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
  x_3$RegionSite <- Regionnum*1000000 + Site
  
  x_3_joindat <- join(rastvals, x_3, by="RegionSite")
  #head(test.df.sgs_2)
  #View(test.df.sgs_2_joindat)
  x_3_joindat_2<-x_3_joindat[,-c(2:71)]
  #View(test.df.sgs_2_joindat_2)
  
  
  return(x_3_joindat_2)
}

#function to integrate climate data with the raster
raster_link_function_x<-function(x) {

temp.list <- list()
#1986
x_1986<- raster_sites
values(x_1986) <- x[,"1986"]
temp.list[["x_1986"]] <-x_1986
#1987
x_1987<- raster_sites
values(x_1987) <- x[,"1987"]
temp.list[["x_1987"]] <-x_1987
#1988
x_1988<- raster_sites
values(x_1988) <- x[,"1988"]
temp.list[["x_1988"]] <-x_1988
#1989
x_1989<- raster_sites
values(x_1989) <- x[,"1989"]
temp.list[["x_1989"]] <-x_1989
#1990
x_1990<- raster_sites
values(x_1990) <- x[,"1990"]
temp.list[["x_1990"]] <-x_1990
#1991
x_1991<- raster_sites
values(x_1991) <- x[,"1991"]
temp.list[["x_1991"]] <-x_1991
#1992
x_1992<- raster_sites
values(x_1992) <- x[,"1992"]
temp.list[["x_1992"]] <-x_1992
#1993
x_1993<- raster_sites
values(x_1993) <- x[,"1993"]
temp.list[["x_1993"]] <-x_1993
#1994
x_1994<- raster_sites
values(x_1994) <- x[,"1994"]
temp.list[["x_1994"]] <-x_1994
#1995
x_1995<- raster_sites
values(x_1995) <- x[,"1995"]
temp.list[["x_1995"]] <-x_1995
#1996
x_1996<- raster_sites
values(x_1996) <- x[,"1996"]
temp.list[["x_1996"]] <-x_1996
#1997
x_1997<- raster_sites
values(x_1997) <- x[,"1997"]
temp.list[["x_1997"]] <-x_1997
#1998
x_1998<- raster_sites
values(x_1998) <- x[,"1998"]
temp.list[["x_1998"]] <-x_1998
#1999
x_1999<- raster_sites
values(x_1999) <- x[,"1999"]
temp.list[["x_1999"]] <-x_1999
#2000
x_2000<- raster_sites
values(x_2000) <- x[,"2000"]
temp.list[["x_2000"]] <-x_2000
#2001
x_2001<- raster_sites
values(x_2001) <- x[,"2001"]
temp.list[["x_2001"]] <-x_2001
#2002
x_2002<- raster_sites
values(x_2002) <- x[,"2002"]
temp.list[["x_2002"]] <-x_2002
#2003
x_2003<- raster_sites
values(x_2003) <- x[,"2003"]
temp.list[["x_2003"]] <-x_2003
#2004
x_2004<- raster_sites
values(x_2004) <- x[,"2004"]
temp.list[["x_2004"]] <-x_2004
#2005
x_2005<- raster_sites
values(x_2005) <- x[,"2005"]
temp.list[["x_2005"]] <-x_2005
#2006
x_2006<- raster_sites
values(x_2006) <- x[,"2006"]
temp.list[["x_2006"]] <-x_2006
#2007
x_2007<- raster_sites
values(x_2007) <- x[,"2007"]
temp.list[["x_2007"]] <-x_2007
#2008
x_2008<- raster_sites
values(x_2008) <- x[,"2008"]
temp.list[["x_2008"]] <-x_2008
#2009
x_2009<- raster_sites
values(x_2009) <- x[,"2009"]
temp.list[["x_2009"]] <-x_2009
#2010
x_2010<- raster_sites
values(x_2010) <- x[,"2010"]
temp.list[["x_2010"]] <-x_2010
#2011
x_2011<- raster_sites
values(x_2011) <- x[,"2011"]
temp.list[["x_2011"]] <-x_2011
#2012
x_2012<- raster_sites
values(x_2012) <- x[,"2012"]
temp.list[["x_2012"]] <-x_2012
#2013
x_2013<- raster_sites
values(x_2013) <- x[,"2013"]
temp.list[["x_2013"]] <-x_2013
#2014
x_2014<- raster_sites
values(x_2014) <- x[,"2014"]
temp.list[["x_2014"]] <-x_2014
#2015
x_2015<- raster_sites
values(x_2015) <- x[,"2015"]
temp.list[["x_2015"]] <-x_2015

x_stack <-stack(temp.list) #stack all the stored raster

#made into dataframe
x_stack_df<-rasterToPoints(x_stack)
head(x_stack_df)

x_stack_df_2 <- as.data.frame(x_stack_df)
head(x_stack_df_2)

x_stack_df_melted <- melt(x_stack_df_2, 
                          id.vars = c("x", "y"),
                          variable.name = "year") #melt to long format

head(x_stack_df_melted)


x_stack_df_melted$year<-gsub('x_','', x_stack_df_melted$year)


return(x_stack_df_melted)


}

