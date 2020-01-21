#testing historical covariate workflow for SGS

pet_covariates<-covariates[c(17)]
pet.covariates.list<-list()

for(i in pet_covariates[1])
{
  
  pet.covariates.list[[i]] <- get(load(file.path(soil_moisture_dir,i)))
  
}

head((sgs.covariates.list[1]))

#practice
rownames(test.df.sgs) <- c()
names(test.df.sgs) <- substrRight(names(test.df.sgs),4)

#turn into dataframe
test.df.sgs<-as.data.frame(sgs.covariates.list[1])
View(test.df.sgs)
head(test.df.sgs)
?substring
test.df.sgs.2<-test.df.sgs[1]
View(test.df.sgs.2)
test.df.sgs.2$label <- row.names(test.df.sgs)

Strings.

#function just to get the last four characters of the labels
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#clean up
rownames(test.df.sgs) <- c()
names(test.df.sgs) <- substrRight(names(test.df.sgs),4)
View(test.df.sgs)
test.df.sgs<-as.data.frame(sgs.covariates.list[1])

#integrate with
test.df.sgs$Regionname <- substr(test.df.sgs$site, 8, 9)
test.df.sgs <- dplyr::filter(test.df.sgs, Regionname != "De") #Remove excess site values
unique(test.df.sgs$Regionname)
View(test.df.sgs_2)
test.df.sgs_2 <- test.df.sgs[,-103]

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', test.df.sgs_2$site) )
Site <- as.integer(substr(test.df.sgs_2$site, 1, sitenumENDpos-1) )
Regionname <- substr(test.df.sgs_2$site, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
test.df.sgs_2$RegionSite <- Regionnum*1000000 + Site

test.df.sgs_2_joindat <- join(rastvals, test.df.sgs_2, by="RegionSite")
head(test.df.sgs_2)
View(test.df.sgs_2_joindat)
test.df.sgs_2_joindat_2<-test.df.sgs_2_joindat[,-c(3:72)]
View(test.df.sgs_2_joindat_2)
#stopped here...ned to figure how to automate the original code...
