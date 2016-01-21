source('make_trends.R')



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

# exc2 (cops asking for bribes) maybe on the rise?
# 2012-2014 definitely higher than 2006-2010, which were higher than 2004
exc2 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(exc2) & exc2==1),
            n=sum(!is.na(exc2) & (exc2==0 | exc2==1)))
binom_plot(exc2,'exc2: Asked for a bribe by a police officer?',0.12)
# exc13 (paying bribes at work) seems to be dropping; definitely
# lower now than 2004-2008
exc13 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(exc13) & exc13==1),
              n=sum(!is.na(exc13) & (exc13==0 | exc13==1)))
binom_plot(exc13,'exc13: Asked to pay a bribe at work?',0.03)
# exc14 (bribes to courts) definitely on the rise
# or maybe not -- error bars all overlap
exc14 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(exc14) & exc14==1),
               n=sum(!is.na(exc14) & (exc14==0 | exc14==1)))
binom_plot(exc14,'exc14: Asked to pay a bribe to the courts?',0.12)
sum(is.na(lapop.trends$exc14)) / nrow(lapop.trends) # 92% of people didn't answer

# ed (years of education) might be trending up
bar_plot(data.frame(x=na.omit(lapop.trends$ed)))
# Look at trend in mean years of education
ed <- ddply(lapop.trends,~year,summarize,
            y=mean(ed,na.rm=TRUE),
            hi=t.test(ed)$conf.int[2],
            lo=t.test(ed)$conf.int[1])
label <- "ed: Years of education (mean)"
ggplot(data=ed,aes(x=year,y=y)) +
  geom_line(size=1.5,color='goldenrod') +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lo,ymax=hi,width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2014,2)) +
  annotate('text',label=str_wrap(label,width=32),
           size=8,x=2003,y=8,hjust=0,vjust=0) +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) 
# Is this really telling us anything interesting?
ed <- lapop.trends[lapop.trends$year %in% c(2004,2008,2014),c('ed','year')]
ed$year <- as.character(ed$year)
col3 <- brewer.pal(3,"Dark2")
ggplot(ed,aes(x=ed,group=year,color=year)) +
  geom_density(size=2) +
  scale_color_brewer(type="qual") +
  theme_classic() +
  scale_x_continuous(limits=c(0,max(lapop.trends$ed,na.rm=TRUE))) +
  xlab("Years of education")+
  theme(text=element_text(size=20),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) 
# This makes it clearer what is happening -- in 2004 there were more people
# with only an elementary-school education level; the major growth was
# in the number of people with post-primary education.
ed <- ddply(lapop.trends,~year,summarize,
            yrs0_6=sum(ed<7,na.rm=TRUE) / sum(ed<88,na.rm=TRUE),
            yrs7_12=sum(ed>6 & ed<13,na.rm=TRUE) / sum(ed<88,na.rm=TRUE),
            more=sum(ed>12,na.rm=TRUE) / sum(ed<88,na.rm=TRUE))
multi_lines(ed) # this looks terrible

# r3 (fridge in home) trending up
r3 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r3) & r3==1),
            n=sum(!is.na(r3) & (r3==0 | r3==1)))
binom_plot(r3,'r3: Refrigerator in home',0.65)
# as is r6 (washing machine)
r6 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r6) & r6==1),
            n=sum(!is.na(r6) & (r6==0 | r6==1)))
binom_plot(r6,'r6: Washing machine in home',0.18)
# r12 (drinking water) moving up a little
r12 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r12) & r12==1),
            n=sum(!is.na(r12) & (r12==0 | r12==1)))
binom_plot(r12,'r12: Drinking water in home',0.8)
#as is r14 (indoor bathroom)
r14 <- ddply(lapop.trends,~year,summarize,x=sum(!is.na(r14) & r14==1),
             n=sum(!is.na(r14) & (r14==0 | r14==1)))
binom_plot(r14,'r14: Indoor bathroom in home',0.6)


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
bar_plot(data.frame(x=na.omit(lapop.trends$e5)))
# This reminds me a bit of the political ideology chart -- lots concentrated at either end
e5 <- ddply(lapop.trends,~year,summarize,
            disapprove=sum(e5<4,na.rm=TRUE) / sum(e5<11,na.rm=TRUE),
            meh=sum(e5>3 & e5<8,na.rm=TRUE) / sum(e5<11,na.rm=TRUE),
            approve=sum(e5>7,na.rm=TRUE) / sum(e5<11,na.rm=TRUE))
multi_lines(e5)
# People who are actively approving of political demonstrations seem to be 
# losing ground to those who are either wishy-washy or disapproving


# d1 (right of govt critics to vote) dropped 2010-2012
bar_plot(data.frame(x=na.omit(lapop.trends$d1)))
d1 <- ddply(lapop.trends,~year,summarize,
            disapprove=sum(d1<5,na.rm=TRUE) / sum(d1<11,na.rm=TRUE),
            meh=sum(d1>4 & d1<7,na.rm=TRUE) / sum(d1<11,na.rm=TRUE),
            approve=sum(d1>6,na.rm=TRUE) / sum(d1<11,na.rm=TRUE))
multi_lines(d1)
# d2 (approval of critics' peaceful demonstrations) also dropping
bar_plot(data.frame(x=na.omit(lapop.trends$d2)))
d2 <- ddply(lapop.trends,~year,summarize,
            disapprove=sum(d2<5,na.rm=TRUE) / sum(d2<11,na.rm=TRUE),
            meh=sum(d2>4 & d2<7,na.rm=TRUE) / sum(d2<11,na.rm=TRUE),
            approve=sum(d2>6,na.rm=TRUE) / sum(d2<11,na.rm=TRUE))
multi_lines(d2)
# as is d3 (critics running for office)
bar_plot(data.frame(x=na.omit(lapop.trends$d3)))
d3 <- ddply(lapop.trends,~year,summarize,
            disapprove=sum(d3<5,na.rm=TRUE) / sum(d3<11,na.rm=TRUE),
            meh=sum(d3>4 & d3<7,na.rm=TRUE) / sum(d3<11,na.rm=TRUE),
            approve=sum(d3>6,na.rm=TRUE) / sum(d3<11,na.rm=TRUE))
multi_lines(d3)
#d4 (making speeches)
bar_plot(data.frame(x=na.omit(lapop.trends$d4)))
d4 <- ddply(lapop.trends,~year,summarize,
            disapprove=sum(d4<5,na.rm=TRUE) / sum(d4<11,na.rm=TRUE),
            meh=sum(d4>4 & d4<7,na.rm=TRUE) / sum(d4<11,na.rm=TRUE),
            approve=sum(d4>6,na.rm=TRUE) / sum(d4<11,na.rm=TRUE))
multi_lines(d4)
# In all cases, people took a disapproving turn around 2010


# l1: political ideology
bar_plot(data.frame(x=na.omit(lapop.trends$l1)))
# Three logical groupings: left, center, right
l1 <- ddply(lapop.trends,~year,summarize,
            left=sum(l1<5,na.rm=TRUE) / sum(l1<11,na.rm=TRUE),
            center=sum(l1==5 | l1==6,na.rm=TRUE) / sum(l1<11,na.rm=TRUE),
            right=sum(l1>6,na.rm=TRUE) / sum(l1<11,na.rm=TRUE))
multi_lines(l1)

# Trust in National Police (b18): 1=none, 7=lots
b18 <- data.frame(x=na.omit(lapop.trends$b18))
bar_plot(b18)
b18 <- ddply(lapop.trends,~year,summarize,
            low=sum(b18<3,na.rm=TRUE) / sum(b18<8,na.rm=TRUE),
            medium=sum(b18>2 & b18<6,na.rm=TRUE) / sum(b18<8,na.rm=TRUE),
            high=sum(b18>5,na.rm=TRUE) / sum(b18<8,na.rm=TRUE))
multi_lines(b18)

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


