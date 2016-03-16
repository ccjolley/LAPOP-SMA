library(foreign) # needed to import STATA files
library(plyr)
library(ggplot2)

setwd("C:/Users/Craig/Dropbox/SMA/VSFS")

# NOTE: The read.dta() function in R's "foreign" package only
# works with Stata versions 5-12; Stata 13 came out in 2013
# and it looks like that is what was used for the 2014 LAPOP
# data. Fortunately, USAID hosts a CSV version of the same data.

urls <- matrix(nrow=6,ncol=3)
colnames(urls) <- c('GTM','HND','SLV')
rownames(urls) <- c('2004','2006','2008','2010','2012','2014')
urls['2004','GTM'] <- "http://datasets.americasbarometer.org/datasets/1111117573guatemala%202004%20export%20version.dta"
urls['2004','HND'] <- "http://datasets.americasbarometer.org/datasets/1997861390honduras%202004%20export%20version.dta"
urls['2004','SLV'] <- "http://datasets.americasbarometer.org/datasets/1762554388el%20salvador%202004%20export%20version.dta"
urls['2006','GTM'] <- "http://datasets.americasbarometer.org/datasets/784250406guatemala_%20lapop_final%202006%20data%20set%20092906.dta"
urls['2006','HND'] <- "http://datasets.americasbarometer.org/datasets/1631591650honduras_lapop_final%202006%20data%20set%20092906.dta"
urls['2006','SLV'] <- "http://datasets.americasbarometer.org/datasets/223466081el%20salvador_lapop_final%202006%20data%20set%20092906.dta"
urls['2008','GTM'] <- "http://datasets.americasbarometer.org/datasets/130872853guatemala_lapop_dims_2008_final_data_set_v10.dta"
urls['2008','HND'] <- "http://datasets.americasbarometer.org/datasets/1645121539honduras_lapop_dims_2008_final_data_set_v10.dta"
urls['2008','SLV'] <- "http://datasets.americasbarometer.org/datasets/1698154699el_salvador_lapop_dims_2008_final_lapop08_v10.dta"
urls['2010','GTM'] <- "http://datasets.americasbarometer.org/datasets/305797627Guatemala_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20V3.dta"
urls['2010','HND'] <- "http://datasets.americasbarometer.org/datasets/1418722138Honduras_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta"
urls['2010','SLV'] <- "http://datasets.americasbarometer.org/datasets/316969358El%20Salvador_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta"
urls['2012','GTM'] <- "http://datasets.americasbarometer.org/datasets/2041873797Guatemala%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta"
urls['2012','HND'] <- "http://datasets.americasbarometer.org/datasets/42768395Honduras%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta"
urls['2012','SLV'] <- "http://datasets.americasbarometer.org/datasets/1126250629ElSalvador%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta"
urls['2014','GTM'] <- "http://www.usaid.gov/opengov/developer/datasets/Guatemala_Data_LAPOP_americasbarometer_2014.csv"
urls['2014','HND'] <- "http://www.usaid.gov/opengov/developer/datasets/Honduras_Data_LAPOP_americasbarometer_2014.csv"
urls['2014','SLV'] <- "http://www.usaid.gov/opengov/developer/datasets/ElSalvador_Data_LAPOP_americasbarometer_2014.csv"

fnames <- sapply(colnames(urls),function(x) paste(rownames(urls),x,sep ='-'))
rownames(fnames) <- rownames(urls)
fnames[1:5,] <- paste(fnames[1:5,],'.dta',sep='')
fnames[6,] <- paste(fnames[6,],'.csv',sep='')

# download everything -- STATA files need to be in binary mode
f <- data.frame(u=as.vector(t(urls)),f=as.vector(t(fnames)),stringsAsFactors=FALSE)
lapply(1:15,function(x) download.file(f$u[x],f$f[x],method='auto',mode='wb'))
lapply(16:18,function(x) download.file(f$u[x],f$f[x],method='auto',mode='w'))

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
lapop.2010.GTM$year <- 2010; lapop.2010.HND$year <- 2010; lapop.2004.SLV$year <- 2010
lapop.2012.GTM$year <- 2012; lapop.2012.HND$year <- 2012; lapop.2012.SLV$year <- 2012
lapop.2014.GTM$year <- 2014; lapop.2014.HND$year <- 2014; lapop.2014.SLV$year <- 2014

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

# create a new data frame containing only consistent indicators

lapop.trends <- rbind(lapop.2004.GTM[trend.all],lapop.2004.HND[trend.all],lapop.2004.SLV[trend.all],
                      lapop.2006.GTM[trend.all],lapop.2006.HND[trend.all],lapop.2006.SLV[trend.all],
                      lapop.2008.GTM[trend.all],lapop.2008.HND[trend.all],lapop.2008.SLV[trend.all],
                      lapop.2010.GTM[trend.all],lapop.2010.HND[trend.all],lapop.2010.SLV[trend.all],
                      lapop.2012.GTM[trend.all],lapop.2012.HND[trend.all],lapop.2012.SLV[trend.all],
                      lapop.2014.GTM[trend.all],lapop.2014.HND[trend.all],lapop.2014.SLV[trend.all])
# In 2004-2012, missing values are conveyed by 'NA'. In 2012, they used 888888 
# and 988888; these will do very bad things if you leave them in!
# Some years also used 8 and 9 as no-response codes; be more careful with those...
is.na(lapop.trends)[lapop.trends==888888] <- TRUE
is.na(lapop.trends)[lapop.trends==988888] <- TRUE
is.na(lapop.trends)[lapop.trends==999999] <- TRUE
