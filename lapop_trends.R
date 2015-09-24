library(foreign) # needed to import STATA files
library(plyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/Craig/Dropbox/SMA/VSFS")

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
just3 <- lapop.trends[which(lapop.trends$year==2014 | lapop.trends$year==2004 | lapop.trends$year==2010),]

# use these functions to make error bars with binomial test
binom.low <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[1] }
binom.high <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[2] }

# Watch out: In 2014, cp5 (tried to solve a community problem) was on a scale of 1-4 (4=never). 
# Earlier years just used 1=yes, 2=no, so it looks like there was a big jump in 2008.

# d5 (homosexuals running for office) is trending upward

# exc2 (cops asking for bribes) maybe on the rise?
# exc13 (paying bribes at work) seems to be dropping
# exc14 (bribes to courts) definitely on the rise

# ed (years of education) might be trending up
# r3 (fridge in home) trending up, as is r6 (washing machine)
# r12 (drinking water) moving up a little, as is r14 (indoor bathroom)

# r4 (landline in home) dropping since 2006
r4 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r4) & r4==1),
             n=sum(!is.na(r4) & (r4==0 | r4==1)))
r4$mean <- r4$x / r4$n
r4$ymin <- mapply(binom.low,r4$x,r4$n)
r4$ymax <- mapply(binom.high,r4$x,r4$n)
ggplot(data=r4,aes(x=year,y=mean)) +
  geom_line(size=1.5,color='red') +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax,width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2014,2)) +
  annotate('text',label=str_wrap("r4: Landline telephone in home",width=32),
           size=6,x=2003,y=0.45,hjust=0,vjust=0) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# r4a (cellphones) is the strongest trend I've seen 
r4a <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r4a) & r4a==1),
             n=sum(!is.na(r4a) & (r4a==0 | r4a==1)))
r4a$mean <- r4a$x / r4a$n
r4a$ymin <- mapply(binom.low,r4a$x,r4a$n)
r4a$ymax <- mapply(binom.high,r4a$x,r4a$n)
ggplot(data=r4a,aes(x=year,y=mean)) +
  geom_line(size=1.5,color='red') +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax,width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2014,2)) +
  annotate('text',label=str_wrap("r4a: Cellular telephone in home",width=32),
           size=6,x=2003,y=0.8,hjust=0,vjust=0) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# r15 (computers) also not bad
r15 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r15) & r15==1),
             n=sum(!is.na(r15) & (r15==0 | r15==1)))
r15$mean <- r15$x / r15$n
r15$ymin <- mapply(binom.low,r15$x,r15$n)
r15$ymax <- mapply(binom.high,r15$x,r15$n)
ggplot(data=r15,aes(x=year,y=mean)) +
  geom_line(size=1.5,color='red') +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax,width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2014,2)) +
  annotate('text',label=str_wrap("r15: Computer in home",width=32),
           size=6,x=2003,y=0.25,hjust=0,vjust=0) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())



# cp6 (religious attendance) climbing since 2008
# 1=weekly, 2=once or twice monthly, 3=once or twice annualy, 4=never
cp6 <- ddply(lapop.trends,~year,summarize,mean=mean(cp6,na.rm=TRUE),sd=sd(cp6,na.rm=TRUE))
cp6$ymax=cp6$mean+cp6$sd
cp6$ymin=cp6$mean-cp6$sd
ggplot(data=cp6,aes(x=year,y=mean)) +
  geom_line(size=1.5,color='red') +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax)) +
  scale_x_continuous(breaks=seq(2004,2014,2)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
# Taking averages and standard deviations here is a little misleading
ggplot(just3,aes(x=cp6)) +
  geom_density(aes(group=factor(year),color=factor(year),fill=factor(year),alpha=0.2)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
# This is a little clearer -- the number of people who have been attending church weekly
# has been rising, while the number who never went at all increased in 2010 and has
# declined subsequently. 
# TODO: New time series for the four different categories to show how the breakdown has changed.


# jc10 (coup justified if crime is high) also climbing since 2008
# b12 (trust in armed forces) might be trending up a little 
# b32 (trust in municipal govt) dropping for last three
# m1 (presidential job performance) might be improving a little
# e5 decreases every year (approval of participating in legal demonstration)
# d1 (right of govt critics to vote) dropped 2010-2012
# d2 (approval of critics' peaceful demonstrations) also dropping
# as is d3 (critics running for office), d4 (making speeches)

# Overall, there seems to a decrease in center-right political views over time
ggplot(just3,aes(x=l1)) +
  geom_density(aes(group=factor(year),color=factor(year),fill=factor(year),alpha=0.2)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
# There are aslo differences between countries: Guatemala (pais=2) tends to be more centrist,
# El Salvador (pais=3) more polarized, and Honduras (pais=4) leans right, averaged across all years.
ggplot(lapop.trends,aes(x=l1)) +
  geom_density(aes(group=factor(pais),color=factor(pais),fill=factor(pais),alpha=0.2)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Religious affiliation (b20)
# Policing (b18)
# Govt job performance (m1)
# Intention to go abroad (q14)
# Community involvement (cp4a,cp5,cp6,cp7,cp8)
# Corruption (exc2,exc7,exc11,exc13,exc14,exc15,exc16)

trend.gtm <- setdiff(intersection(names(lapop.2004.GTM),names(lapop.2006.GTM),
                                  names(lapop.2008.GTM),names(lapop.2010.GTM),
                                  names(lapop.2012.GTM),names(lapop.2014.GTM)),trend.all)
trend.hnd <- setdiff(intersection(names(lapop.2004.HND),names(lapop.2006.HND),
                                  names(lapop.2008.HND),names(lapop.2010.HND),
                                  names(lapop.2012.HND),names(lapop.2014.HND)),trend.all)
trend.slv <- setdiff(intersection(names(lapop.2004.SLV),names(lapop.2006.SLV),
                                  names(lapop.2008.SLV),names(lapop.2010.SLV),
                                  names(lapop.2012.SLV),names(lapop.2014.SLV)),trend.all)



