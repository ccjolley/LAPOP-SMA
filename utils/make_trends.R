library(foreign) # needed to import STATA files
library(plyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(RColorBrewer)
library(stringi)

setwd("C:/Users/Craig/Dropbox/SMA/VSFS")

###############################################################################
# Data cleaning: Create a data frame called lapop.trends containing all 
# indicators that are consistent across countries and years.
###############################################################################
yrs <- c('2004','2006','2008','2010','2012','2014')
fnames <- sapply(c('GTM','HND','SLV'),function(x) paste(yrs,x,sep ='-'))
rownames(fnames) <- yrs
fnames[1:5,] <- paste(fnames[1:5,],'.dta',sep='')
fnames[6,] <- paste(fnames[6,],'.csv',sep='')

# this isn't the most compact way to do this, but as long as the datasets
# are heterogeneous it sort of makes sense
lapop.2004.GTM <- read.dta(fnames['2004','GTM'],convert.factors=FALSE)
lapop.2004.HND <- read.dta(fnames['2004','HND'],convert.factors=FALSE)
lapop.2004.SLV <- read.dta(fnames['2004','SLV'],convert.factors=FALSE)
lapop.2006.GTM <- read.dta(fnames['2006','GTM'],convert.factors=FALSE)
lapop.2006.HND <- read.dta(fnames['2006','HND'],convert.factors=FALSE)
lapop.2006.SLV <- read.dta(fnames['2006','SLV'],convert.factors=FALSE)
lapop.2008.GTM <- read.dta(fnames['2008','GTM'],convert.factors=FALSE)
lapop.2008.HND <- read.dta(fnames['2008','HND'],convert.factors=FALSE)
lapop.2008.SLV <- read.dta(fnames['2008','SLV'],convert.factors=FALSE)
lapop.2010.GTM <- read.dta(fnames['2010','GTM'],convert.factors=FALSE)
lapop.2010.HND <- read.dta(fnames['2010','HND'],convert.factors=FALSE)
lapop.2010.SLV <- read.dta(fnames['2010','SLV'],convert.factors=FALSE)
lapop.2012.GTM <- read.dta(fnames['2012','GTM'],convert.factors=FALSE)
lapop.2012.HND <- read.dta(fnames['2012','HND'],convert.factors=FALSE)
lapop.2012.SLV <- read.dta(fnames['2012','SLV'],convert.factors=FALSE)
lapop.2014.GTM <- read.csv(fnames['2014','GTM'])
lapop.2014.HND <- read.csv(fnames['2014','HND'])
lapop.2014.SLV <- read.csv(fnames['2014','SLV'])

# before we can check for consistent indicators, we need to make them all lowercase
names(lapop.2004.GTM) <- tolower(names(lapop.2004.GTM))
names(lapop.2004.HND) <- tolower(names(lapop.2004.HND))
names(lapop.2004.SLV) <- tolower(names(lapop.2004.SLV))
names(lapop.2006.GTM) <- tolower(names(lapop.2006.GTM))
names(lapop.2006.HND) <- tolower(names(lapop.2006.HND))
names(lapop.2006.SLV) <- tolower(names(lapop.2006.SLV))
names(lapop.2008.GTM) <- tolower(names(lapop.2008.GTM))
names(lapop.2008.HND) <- tolower(names(lapop.2008.HND))
names(lapop.2008.SLV) <- tolower(names(lapop.2008.SLV))
names(lapop.2010.GTM) <- tolower(names(lapop.2010.GTM))
names(lapop.2010.HND) <- tolower(names(lapop.2010.HND))
names(lapop.2010.SLV) <- tolower(names(lapop.2010.SLV))
names(lapop.2012.GTM) <- tolower(names(lapop.2012.GTM))
names(lapop.2012.HND) <- tolower(names(lapop.2012.HND))
names(lapop.2012.SLV) <- tolower(names(lapop.2012.SLV))
names(lapop.2014.GTM) <- tolower(names(lapop.2014.GTM))
names(lapop.2014.HND) <- tolower(names(lapop.2014.HND))
names(lapop.2014.SLV) <- tolower(names(lapop.2014.SLV))

# add years 
lapop.2004.GTM$year <- 2004; lapop.2004.HND$year <- 2004; lapop.2004.SLV$year <- 2004
lapop.2006.GTM$year <- 2006; lapop.2006.HND$year <- 2006; lapop.2006.SLV$year <- 2006
lapop.2008.GTM$year <- 2008; lapop.2008.HND$year <- 2008; lapop.2008.SLV$year <- 2008
lapop.2010.GTM$year <- 2010; lapop.2010.HND$year <- 2010; lapop.2010.SLV$year <- 2010
lapop.2012.GTM$year <- 2012; lapop.2012.HND$year <- 2012; lapop.2012.SLV$year <- 2012
lapop.2014.GTM$year <- 2014; lapop.2014.HND$year <- 2014; lapop.2014.SLV$year <- 2014

# The idnum field wasn't used in 2004 (or in 2006 in SLV); this can be handy 
# if I need to join the complete dataset with individual ones again.
set.seed(12345)
lapop.2004.GTM$idnum <- stri_rand_strings(nrow(lapop.2004.GTM),60)
lapop.2004.SLV$idnum <- stri_rand_strings(nrow(lapop.2004.SLV),60)
lapop.2006.SLV$idnum <- stri_rand_strings(nrow(lapop.2006.SLV),60)
lapop.2004.HND$idnum <- stri_rand_strings(nrow(lapop.2004.HND),60)
# sanity check
# nrow(lapop.2004.GTM)+nrow(lapop.2004.SLV)+nrow(lapop.2006.SLV)+nrow(lapop.2004.HND)
# length(unique(c(lapop.2004.GTM$idnum,lapop.2004.SLV$idnum,
#                 lapop.2006.SLV$idnum,lapop.2004.HND$idnum)))


# I <3 recursive functions
intersection <- function(x, y, ...){
  if (missing(...)) intersect(x, y)
  else intersect(x, intersection(y, ...))
}

trend.all <- intersection(names(lapop.2004.GTM),names(lapop.2004.HND),names(lapop.2004.SLV),
                          names(lapop.2006.GTM),names(lapop.2006.HND),names(lapop.2006.SLV),
                          names(lapop.2008.GTM),names(lapop.2008.HND),names(lapop.2008.SLV),
                          names(lapop.2010.GTM),names(lapop.2010.HND),names(lapop.2010.SLV),
                          names(lapop.2012.GTM),names(lapop.2012.HND),names(lapop.2012.SLV),
                          names(lapop.2014.GTM),names(lapop.2014.HND),names(lapop.2014.SLV))

lapop.trends <- rbind(lapop.2004.GTM[trend.all],lapop.2004.HND[trend.all],lapop.2004.SLV[trend.all],
                      lapop.2006.GTM[trend.all],lapop.2006.HND[trend.all],lapop.2006.SLV[trend.all],
                      lapop.2008.GTM[trend.all],lapop.2008.HND[trend.all],lapop.2008.SLV[trend.all],
                      lapop.2010.GTM[trend.all],lapop.2010.HND[trend.all],lapop.2010.SLV[trend.all],
                      lapop.2012.GTM[trend.all],lapop.2012.HND[trend.all],lapop.2012.SLV[trend.all],
                      lapop.2014.GTM[trend.all],lapop.2014.HND[trend.all],lapop.2014.SLV[trend.all])
# In 2004-2012, missing values are conveyed by 'NA'. In 2014, they used 888888 
# and 988888; these will do very bad things if you leave them in!
# Some years also used 8 and 9 as no-response codes; be more careful with those...
is.na(lapop.trends)[lapop.trends==888888] <- TRUE
is.na(lapop.trends)[lapop.trends==988888] <- TRUE
is.na(lapop.trends)[lapop.trends==999999] <- TRUE
is.na(lapop.trends)[lapop.trends==88] <- TRUE

# clean up
rm(lapop.2004.GTM,lapop.2004.SLV,lapop.2004.HND,
   lapop.2006.GTM,lapop.2006.SLV,lapop.2006.HND,
   lapop.2008.GTM,lapop.2008.SLV,lapop.2008.HND,
   lapop.2010.GTM,lapop.2010.SLV,lapop.2010.HND,
   lapop.2012.GTM,lapop.2012.SLV,lapop.2012.HND,
   lapop.2014.GTM,lapop.2014.SLV,lapop.2014.HND,
   fnames,trend.all,yrs,intersection)

setwd("C:/Users/Craig/Dropbox/SMA/VSFS/LAPOP-SMA")

#TODO: lapop.trends really needs lat/long information.
# Looks like 'pais' is the only geo indicator that has the same name every year.
# prov: HND,SLV from 2008 on, GTM from 2010
# GTM 2004 uses gprov, 2006 uses guadept, guamunicipio, 2008 uses provincia
# To make matters worse, I can't count on the same numeric codes being
# used every year, and I only have codebooks for 2014.

# # Guatemala - departments
# sort(unique(lapop.2004.GTM$gprov)) # numbered 1-22
# sort(unique(lapop.2006.GTM$guadept)) # 11 is missing - questionnaire lists depts but not munis.
# sort(unique(lapop.2008.GTM$provincia)) # numbered 201-222
# sort(unique(lapop.2010.GTM$prov)) # same as 2008
# sort(unique(lapop.2012.GTM$prov)) # same as 2008, missing 219
# sort(unique(lapop.2010.GTM$prov)) # same as 2008
# # Guatelama - municipalities
# sort(unique(lapop.2004.GTM$gcant)) # 1-27, 50
# sort(unique(lapop.2006.GTM$guamunicipio)) # 1-90
# sort(unique(lapop.2008.GTM$municipio)) # 1-95
# sort(unique(lapop.2010.GTM$municipio)) # 20101-22212
# sort(unique(lapop.2012.GTM$municipio)) # 101-2211
# sort(unique(lapop.2014.GTM$municipio)) # same as 2014
# 
# # El Salvador - departments
# sort(unique(lapop.2004.SLV$edepa)) # 1-14
# sort(unique(lapop.2006.SLV$elsdept)) # 1-14
# sort(unique(lapop.2008.SLV$prov)) # 1-14
# sort(unique(lapop.2010.SLV$prov)) # 1-14
# sort(unique(lapop.2012.SLV$prov)) # 301-314
# sort(unique(lapop.2014.SLV$prov)) # 301-314
# # El Salvador - municipalities
# sort(unique(lapop.2004.SLV$emuni)) # 1-65
# sort(unique(lapop.2006.SLV$elsmunicipio)) # 1-66
# sort(unique(lapop.2008.SLV$municipio)) # 1-66
# sort(unique(lapop.2010.SLV$municipio)) # 1-66
# sort(unique(lapop.2012.SLV$municipio)) # 3011-31416
# sort(unique(lapop.2014.SLV$municipio)) # 30101-31416
# 
# # Honduras - departments
# sort(unique(lapop.2004.HND$hdepa)) # 1-18
# sort(unique(lapop.2006.HND$hondpt)) # 1-22
# sort(unique(lapop.2008.HND$prov)) # 401-422
# sort(unique(lapop.2010.HND$prov)) # 401-422
# sort(unique(lapop.2012.HND$prov)) # 401-422, missing 414-417
# sort(unique(lapop.2014.HND$prov)) # 401-422, missing 414-417
# # Honduras - municipalities
# sort(unique(lapop.2004.HND$hmuni)) # 1-26
# sort(unique(lapop.2006.HND$honmunicipio)) # 101-2204
# sort(unique(lapop.2008.HND$municipio)) # 101-2204
# sort(unique(lapop.2010.HND$honmunicipio)) # 101-2204
# sort(unique(lapop.2010.HND$municipio)) # 401-493
# # Honduras - aldeas
# sort(unique(lapop.2004.HND$hmuni2)) # 1-1810
# sort(unique(lapop.2006.HND$hondistrito)) # 1-163
# sort(unique(lapop.2008.HND$hondistrito)) # 1-163
# sort(unique(lapop.2010.HND$hondistrito)) # 101001-2204163
# sort(unique(lapop.2012.HND$hondistrito)) # 10101-180752
# sort(unique(lapop.2014.HND$hondistrito)) # 10201-180752
# 
# # I can't guarantee any consistency across datasets here.
# 
# 