
test.df.sgs.function<-as.data.frame(pet.covariates.list[1]) 
head(test.df.sgs.function)
View(test.df.sgs.function)
str(test.df.sgs.function)
initial_cleanup<- function(x) {

  #test.df.sgs<-as.data.frame(sgs.covariates.list[1]) #need to fix this
  
  #clean up
  #rownames(test.df.sgs.function) <- c()
  #test.df.sgs.function <- sapply(test.df.sgs.function, as.numeric)
  names(test.df.sgs.function) <- substrRight(names(test.df.sgs.function),4)
  test.df.sgs.function$label<-test.df.sgs.function$site
  test.df.sgs.function<-test.df.sgs.function[-1]
  #View(test.df.sgs.function)
  
  #integrate with
  test.df.sgs.function$Regionname <- substr(test.df.sgs.function$label, 8, 9)
  test.df.sgs.function <- dplyr::filter(test.df.sgs.function, Regionname != "De") #Remove excess site values
  #unique(test.df.sgs.function$Regionname)
  #View(test.df.sgs.function)
  test.df.sgs.function_2 <- test.df.sgs.function[,-103]
  View(test.df.sgs.function_2)
  View(test.df.sgs.function_2_numeric)
  test.df.sgs.function_2_numeric <- as.numeric(factor(as.matrix(test.df.sgs.function_2)))
  #Creating unique IDs
  sitenumENDpos = as.integer(regexpr('_', test.df.sgs.function_2$label) )
  Site <- as.integer(substr(test.df.sgs.function_2$label, 1, sitenumENDpos-1) )
  Regionname <- substr(test.df.sgs.function_2$label, 8, 9)
  Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
  test.df.sgs.function_2$RegionSite <- Regionnum*1000000 + Site
  
  test.df.sgs.function_2_joindat <- join(rastvals, test.df.sgs.function_2, by="RegionSite")
  #head(test.df.sgs_2)
  #View(test.df.sgs.function_2_joindat)
  dim(test.df.sgs.function_2_joindat)
  test.df.sgs.function_2_joindat_3<-test.df.sgs.function_2_joindat[,-c(2:71)]
  #View(test.df.sgs.function_2_joindat_3)
  summary(test.df.sgs.function_2_joindat_3)
  
  return(test.df.sgs.function_2_joindat_3)
}



summary(WatYrprecip)
summary(test.df.sgs.function_2)

View(test.df.sgs.function)
test_cleanup<-initial_cleanup(test.df.sgs.function)
View(test_cleanup[,"1987"])
View(test_cleanup[,"RegionSite"])
View(WatYrprecip_joindat_2[,"RegionSite"])
View(WatYrprecip_joindat_2[,"1987"])
test_cleanup_step_2 <-raster_link_function_x(test_cleanup)
stack_test_2 <-stack(test_cleanup_step_2)
plot(stack_test_2)