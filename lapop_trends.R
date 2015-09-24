library(foreign) # needed to import STATA files
library(plyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(RColorBrewer)

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

###############################################################################
# Plotting functions: Use these to make similar-looking plots for different
# indicators
###############################################################################

# use these functions to make error bars with binomial test
binom.low <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[1] }
binom.high <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[2] }

# use this to make plots of a binomial variable (one that takes only 0/1 values)
binom_plot <- function(f,label='',label_y=0) {
  # f is a data frame that must contain the following columns:
  #   year = year of observations
  #   x = number of 1's observed that year
  #   n = total observations in that year
  f$mean <- f$x / f$n
  if (label_y == 0 ) {
    label_y <- 1.1*max(f$mean)
  }
  f$ymin <- mapply(binom.low,f$x,f$n)
  f$ymax <- mapply(binom.high,f$x,f$n)
  ggplot(data=f,aes(x=year,y=mean)) +
    geom_line(size=1.5,color='red') +
    geom_point(size=5) +
    geom_errorbar(aes(ymin=ymin,ymax=ymax,width=0.5)) +
    scale_x_continuous(breaks=seq(2004,2014,2)) +
    annotate('text',label=str_wrap(label,width=32),
             size=8,x=2003,y=label_y,hjust=0,vjust=0) +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank(),
          axis.title.x=element_blank()) 
}

# use this to make bar plots of a categorical indicator with >2 categories
bar_plot <- function(f) {
  # data frame f needs to contain a column x
  # plot distribution of x (no time resolution here)
  ggplot(f,aes(factor(x))) + 
    geom_bar() +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank(),
          axis.title.x=element_blank()) 
}

multi_lines <- function(f) {
  # data frame f needs to contain a year column
  # all other columns will become the names of data series that will be 
  # plotted together
  m <- melt(f,id.vars='year')
  n <- length(unique(m$variable))
  col <- brewer.pal(n,"Dark2")  
  ggplot(m,aes(x=year,y=value,group=variable,color=variable)) + 
    geom_line(size=1.5) +
    scale_color_manual(values=col) +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank()) +
    xlab("Year") 
}

###############################################################################
# Time-series plots for individual indicators -- still need to finish 
# some of these.
###############################################################################

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
binom_plot(r4,'r4: Landline telephone in home')

# r4a (cellphones) is the strongest trend I've seen 
r4a <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r4a) & r4a==1),
             n=sum(!is.na(r4a) & (r4a==0 | r4a==1)))
binom_plot(r4a,'r4a: Cellular telephone in home',0.8)

# r15 (computers) also not bad
r15 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r15) & r15==1),
             n=sum(!is.na(r15) & (r15==0 | r15==1)))
binom_plot(r15,'r15: Computer in home',0.23)

# cp6 (religious attendance) climbing since 2008
# 1=weekly, 2=once or twice monthly, 3=once or twice annualy, 4=never
cp6 <- ddply(lapop.trends,~year,summarize,
             weekly=sum(cp6==1,na.rm=TRUE) / sum(cp6 < 5,na.rm=TRUE),
             monthly=sum(cp6==2,na.rm=TRUE) / sum(cp6 < 5,na.rm=TRUE),
             annual=sum(cp6==3,na.rm=TRUE) / sum(cp6 < 5,na.rm=TRUE),
             never=sum(cp6==4,na.rm=TRUE) / sum(cp6 < 5,na.rm=TRUE))
multi_lines(cp6)
# The number of occasional churchgoers has been pretty constant (and low);
# it looks like the population of weekly churchgoers has been growing while
# non-attenders have been dropping since 2008. What changed in 2008?

# jc10 (coup justified if crime is high) also climbing since 2008
# 1=yes, 2=no
jc10 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(jc10) & jc10==2),
            n=sum(!is.na(jc10) & (jc10==1 | jc10==2)))
binom_plot(jc10,'jc10: Military coup is justified if crime is high',0.6)

# b12 (trust in armed forces) might be trending up a little 
# b32 (trust in municipal govt) dropping for last three
# m1 (presidential job performance) might be improving a little
# e5 decreases every year (approval of participating in legal demonstration)
# d1 (right of govt critics to vote) dropped 2010-2012
# d2 (approval of critics' peaceful demonstrations) also dropping
# as is d3 (critics running for office), d4 (making speeches)

# l1: political ideology
bar_plot(data.frame(x=na.omit(lapop.trends$l1)))
# Three logical groupings: left, center, right
l1 <- ddply(lapop.trends,~year,summarize,
            left=sum(l1<5,na.rm=TRUE) / sum(l1<11,na.rm=TRUE),
            center=sum(l1==5 | l1==6,na.rm=TRUE) / sum(l1<11,na.rm=TRUE),
            right=sum(l1>6,na.rm=TRUE) / sum(l1<11,na.rm=TRUE))
multi_lines(l1)

# Religious affiliation (b20)

# Trust in National Police (b18): 1=none, 7=lots
b18 <- data.frame(x=na.omit(lapop.trends$b18))
bar_plot(b18)

# Govt job performance (m1)
# Intention to go abroad (q14)
# Community involvement (cp4a,cp5,cp6,cp7,cp8)
# Corruption (exc2,exc7,exc11,exc13,exc14,exc15,exc16)

###############################################################################
# There are also a few indicators that were used across all years in 
# individual countries, but not in all three. Here they are, in case they're 
# also interesting.
###############################################################################

trend.gtm <- setdiff(intersection(names(lapop.2004.GTM),names(lapop.2006.GTM),
                                  names(lapop.2008.GTM),names(lapop.2010.GTM),
                                  names(lapop.2012.GTM),names(lapop.2014.GTM)),trend.all)
trend.hnd <- setdiff(intersection(names(lapop.2004.HND),names(lapop.2006.HND),
                                  names(lapop.2008.HND),names(lapop.2010.HND),
                                  names(lapop.2012.HND),names(lapop.2014.HND)),trend.all)
trend.slv <- setdiff(intersection(names(lapop.2004.SLV),names(lapop.2006.SLV),
                                  names(lapop.2008.SLV),names(lapop.2010.SLV),
                                  names(lapop.2012.SLV),names(lapop.2014.SLV)),trend.all)


