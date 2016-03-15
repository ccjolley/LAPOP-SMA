# Compile some statistics on bribes 

library(plyr)

setwd("C:/Users/Craig/Dropbox/SMA/VSFS/LAPOP-SMA")
lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
common <- Reduce(intersect,list(names(lapop.2014.GTM),names(lapop.2014.SLV),
                                names(lapop.2014.HND)))
lapop.2014.all <- rbind(lapop.2014.GTM[,common],lapop.2014.SLV[,common],
                        lapop.2014.HND[,common])

bribe_vars <- c('exc2','exc6','exc20','exc11','exc13','exc14','exc15','exc16')
bribe_text <- c('Police','Gov. employee','Soldier','Muni. document processing',
                'At work','Courts','Health Services','School')

bribe_data <- lapop.2014.all[,c('pais',bribe_vars)]
bribe_data[bribe_data > 800000] <- NA

avg <- ddply(bribe_data,'pais',function(x) colMeans(x,na.rm=TRUE))
pais_text <- c('','Guatemala','El Salvador','Honduras')
avg$pais <- pais_text[avg$pais]

low <- ddply(bribe_data,'pais',function(x)
  apply(x[,-1],2,function(x) t.test(x)$conf.int[1]))

high <- ddply(bribe_data,'pais',function(x)
  apply(x[,-1],2,function(x) t.test(x)$conf.int[2]))

exc18.hnd <- lapop.2014.HND$exc18
exc18.hnd[exc18.hnd > 800000] <- NA
mean(exc18.hnd,na.rm=TRUE)

exc18.slv <- lapop.2014.SLV$exc18
exc18.slv[exc18.slv > 800000] <- NA
mean(exc18.slv,na.rm=TRUE)

colMeans(!is.na(bribe_data))
colSums(bribe_data,na.rm=TRUE)
