#testing historical covariate workflow for SGS

sgs_covariates<-covariates[c(1,5,9,13)]
sgs.covariates.list<-list()

for(i in sgs_covariates[1:4])
{
  
  sgs.covariates.list[[i]] <- get(load(file.path(soil_moisture_dir,i)))
  
}

head((sgs.covariates.list[1]))

#practice
rownames(test.df.sgs) <- c()
names(test.df.sgs) <- substrRight(names(test.df.sgs),4)
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
test.df.sgs<-as.data.frame(sgs.covariates.list[1])

#integrate with
test.df.sgs$Regionname <- substr(test.df.sgs$site, 8, 9)
test.df.sgs <- dplyr::filter(test.df.sgs, Regionname != "De") #Remove excess site values
unique(test.df.sgs$Regionname)

