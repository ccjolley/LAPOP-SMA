source('make_indices.R')

mil10c <- lapop.2014.all[,c('mil10c','www1','ed','l1','pol1','mil3','for6b',
                            'for7b','mil10e','it1','q5b','q2')]
mil10c$fear <- fear_all
mil10c[mil10c > 800000] <- NA
table(mil10c$mil10c) / nrow(mil10c)
# only about 9% consider the Iranian govt "very" or "somewhat" trustworthy
sum(is.na(mil10c$mil10c))/nrow(mil10c) # 30% didn't answer the question at all

summary(lm(data=mil10c,mil10c ~ fear)) # very significant
# people who mistrust Iran less fearful
summary(lm(data=mil10c,mil10c ~ www1)) 
# significant and positive -- infrequent internet users less trusting
summary(lm(data=mil10c,mil10c ~ ed)) # not significant
summary(lm(data=mil10c,mil10c ~ l1))
# significant and positive -- right-wing views = less trusting
summary(lm(data=mil10c,www1 ~ l1))
# significant and positive -- right wing uses internet less
summary(lm(data=mil10c,mil10c ~ pol1))
# significant and positive -- less interest in politics = less trusting
summary(lm(data=mil10c,www1 ~ pol1))
# very significant and positive -- freqent internet users more political

# So your typical Iran-hater is an infrequent internet user with
# right-wing views who isn't very interested in politics. This would
# lead us to expect the left-leaning, pro-Iran 8% to be making an 
# appearance in social media.

summary(lm(data=mil10c,fear ~ pol1))
# politically-interested people more fearful
summary(lm(data=mil10c,fear ~ www1))
# frequent internet users more fearful
summary(lm(data=mil10c,fear ~ l1)) # no correlation
summary(lm(data=mil10c,fear ~ ed)) # no correlation

# The typical fearful person is a frequent internet user who is
# educated, politically-interested and more trusting of Iran.

summary(lm(data=mil10c,mil10c ~ mil3)) # no correlation
summary(lm(data=mil10c,mil10c ~ for6b)) # no correlation
# significant and negative -- more US influence = less Iran trust
summary(lm(data=mil10c,mil10c ~ for7b)) # no correlation
summary(lm(data=mil10c,mil10c ~ mil10e)) 
# significant and positive -- less US trust = less Iran trust

# more correlations?
summary(lm(mil10c$mil10c ~ w_all)) # nope
summary(lm(mil10c$mil10c ~ tr_all)) # yes -- negative.
summary(lm(mil10c$mil10c ~ aut_all)) # not really
summary(lm(mil10c$mil10c ~ ca_all)) # nope
# significant and negative -- people with less trust in Iran
# have less trust in their own government...?
summary(lm(mil10c$mil10c ~ lapop.2014.all$ur)) # also not
summary(lm(mil10c$mil10c ~ lapop.2014.all$sex)) # also not
summary(lm(data=mil10c,mil10c ~ q2)) # significant and positive -- older people less trusting
summary(lm(data=mil10c,fear ~ q2)) # significant and negative -- older people less fearful
summary(lm(data=mil10c,mil10c ~ it1)) #nope
summary(lm(data=mil10c,mil10c ~ q5b)) #slight -- people to whom religion is important are less trusting

summary(lm(mil10c$mil10c ~ as.numeric(lapop.2014.all$q3c==1))) # not Catholics (also ruled out the others)

# components of fear index
fearcomp <- lapop.2014.all[,c(fear_common,'mil10c')]
fearcomp[fearcomp > 800000] <-  NA
summary(lm(data=fearcomp,mil10c ~ vic44))$coef[2,4]
sapply(fear_common,function(x) 
  summary(lm(data=fearcomp[,c('mil10c',x)],mil10c~.))$coef[2,4])
# strong correlations with aoj17, diso8, dis10, diso18, diso16, diso17
# all vary in the direction you would expect (more mistrust ~ less concern),
# but diso18 seems to be the strongest.

library(ggplot2)
library(plyr)

avgplot <- function(data,x,y,x_lab='',y_lab='',sep=NA) {
  df <- data[,c(x,y)]
  names(df) <- c('x','y')
  df2 <- ddply(df,'x',summarize,mean=mean(y,na.rm=TRUE),
        low=t.test(y)$conf.int[1],high=t.test(y)$conf.int[2])
  df2$x[is.na(df2$x)] <- 'NA'
  if (x_lab=='') {
    x_lab <- x
  }
  if (y_lab == '') {
    y_lab <- y
  }
  p <- ggplot(df2,aes(x=x,y=mean)) +
       geom_point(size=10,color='skyblue') +
       geom_errorbar(aes(ymin=low,ymax=high),width=0.1) +
       theme_classic() +
       xlab(x_lab) +
       ylab(y_lab) +
       theme(text=element_text(size=20))
  if (!is.na(sep)) {
    y_sep <- 0.5*(df2[df2$x==sep[1],'low'] + df2[df2$x==sep[2],'high'])
    p <- p + geom_hline(yintercept=y_sep,color='red')
  }
  p
}

avgplot(mil10c,'mil10c','fear','Trust in Iran (1 = high)','Fear index',sep=c(3,4))
# What we can actually say with confidence is that people who find the Iranian 
# government "not at all trustworthy" are less fearful than everyone else

avgplot(mil10c,'mil10c','www1','Trust in Iran (1 = high)','Internet usage (1 = Daily)',sep=c(4,3))
# We really can't say with confidence whether Iran-haters use the internet less
# than Iran-lovers
avgplot(mil10c,'www1','mil10c')

avgplot(mil10c,'mil10c','l1','Trust in Iran (1 = high)','Ideology (1=left, 10=right)',sep=c(4,3))
# people who really don't trust Iran are further to the right

mil10c$pol1 <- 4 - mil10c$pol1
avgplot(mil10c,'mil10c','pol1','Trust in Iran (1 = high)','Interest in politics (3=a lot, 0=none)',sep=c(4,3))
# people who really don't trust Iran are less interested in politics

avgplot(mil10c,'pol1','fear')
avgplot(mil10c,'mil10c','q5b') # not really

mil10c$tr <- tr_all
avgplot(mil10c,'mil10c','tr',sep=c(3,4))
# Even though low trust in the Iranian government and high trust in one's
# own correlate with less fear, people with less trust in the Iranians
# also trust their own government less.

avgplot(mil10c,'mil10c','q2','Trust in Iran (1 = high)','Age (years)',sep=c(4,3))

bars=data.frame(x=mil10c$mil10c)
bars[is.na(bars$x),'x'] <- 'NA'
n <- nrow(bars)
ggplot(data=bars,aes(x=x)) +
  geom_bar(stat='bin',fill='skyblue') +
  theme_classic() +
  geom_text(size=10,vjust=-0.2,stat="bin", 
            aes(label = ..count.., y=..count..)) +
  theme(text=element_text(size=30),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        axis.line.x=element_blank()) 

##################################################################################

setwd("C:/Users/Craig/Dropbox/SMA/VSFS/Presentation")

library(XLConnect)
irn1 <- readWorksheetFromFile('Iran.xlsx',1)
names(irn1) <- c('Date','All_NT','Iran','Iran_fraction')
ggplot(data=irn1,aes(x=Date,y=Iran)) +
  geom_point(color='tomato',alpha=0.5) +
  geom_smooth(method='loess',size=1.5) +
  theme_classic() +
  scale_y_continuous(limits=c(0,500)) +
  theme(text=element_text(size=20),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        axis.line.x=element_blank()) 

irn2 <- readWorksheetFromFile('Iran.xlsx',2)

percent_to_num <- function(x) {
  0.01*as.numeric(sub("%", "", x))
}
irn2$Iran.positive <- percent_to_num(irn2$Iran.positive)
irn2$Iran.negative <- percent_to_num(irn2$Iran.negative)
irn2[is.na(irn2)] <- 0

stacked <- data.frame(Date=irn2$Analysis.Date,
                      all_neg=irn2$All.negative,
                      all_neut=irn2$All.negative+irn2$All.neutral,
                      all_pos=irn2$All.negative+irn2$All.neutral+irn2$All.positive,
                      irn_neg=irn2$Iran.negative,
                      irn_neut=irn2$Iran.negative + irn2$Iran.neutral,
                      irn_pos=irn2$Iran.negative + irn2$Iran.neutral+irn2$Iran.positive)
stacked <- stacked[stacked$irn_pos > 0 & stacked$all_pos > 0,]
stacked <- stacked[,c("Date","all_neg","all_neut","irn_neg","irn_neut")]

library(reshape)
m <- melt(stacked,id.vars='Date')
ggplot(m,aes(x=Date,y=value,group=variable,color=variable)) +
  geom_point(size=0.1,alpha=0.3) + 
  geom_smooth(method='loess',size=1.5) +
  theme_classic()+
  theme(text=element_text(size=20),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        legend.position='none') 

###############################################################################
# Let's do this a little more systematically. First, Honduras-specific
###############################################################################
source('classify_vars.R')
library(mice)
library(MASS)
h <- lapop.2014.HND[,c(unused_common,unused_hnd)]
is.na(h[h>800000]) <- TRUE
#h$fear_idx <- fear_hnd # skip the fear index for now
h$ca_idx <- ca_hnd 
h$tr_idx <- tr_hnd
h$w_idx <- w_hnd
h$crit_idx <- crit_hnd
h$aut_idx <- aut_hnd
idxs <- c('ca_idx','tr_idx','w_idx','crit_idx','aut_idx')


cor_data <- function(d,my_var,bin_vars,ord_vars,idx_vars,unord_vars) {
  # takes a data frame d with all of the variables that are to be correlated
  # with the fear index, and returns a new data frame with all of the 
  # high-correlation variables, including dummy variables
  cor_bin <- ldply(bin_vars, function(x) ord_bin(d,my_var,x))
  cor_ord <- ldply(ord_vars, function(x) ord_ord(d,my_var,x))
  cor_idx <- ldply(idx_vars, function(x) ord_ord(d,my_var,x))
  cor_unord <- ldply(unord_vars, function(x) ord_unord(d,my_var,x))
  cor_all <- rbind(cor_bin,cor_ord,cor_unord,cor_idx)
  cor_all <- cor_all[order(cor_all$pval),]
  unord_vars <- ldply(cor_unord$var2, function(x)
    data.frame(var=strsplit(x,'_')[[1]][1],
               val=as.numeric(strsplit(x,'_')[[1]][2]),
               str=x,
               stringsAsFactors=FALSE))
  for (i in 1:nrow(unord_vars)) {
    d[,unord_vars$str[i]] <- as.numeric(d[,unord_vars$var[i]] == unord_vars$val[i])
  }
  res <- d[,cor_all$var2]
  res[,my_var] <- d[,my_var]
  res
}
  
h2 <- cor_data(h,'mil10c',c(bin_common,bin_hnd),c(ord_common,ord_hnd),idxs,
                   c(unord_common,unord_hnd))

pm_h <- 1 - diag(ncol(h2))
pm_h[,which(names(h2)=='mil10c')] <- 0
h_imp <- mice(h2,printFlag=F,predictorMatrix=pm_h)

# Get variables by stepwise regression

get_vars <- function(x,var) {
  # gets variables that appear consistently in stepwise regression
  # x should be an object returned by MICE
  lm1 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,1)))
  lm2 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,2)))
  lm3 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,3)))
  lm4 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,4)))
  lm5 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,5)))
  step1 <- stepAIC(lm1,trace=F) 
  step2 <- stepAIC(lm2,trace=F) 
  step3 <- stepAIC(lm3,trace=F) 
  step4 <- stepAIC(lm4,trace=F)
  step5 <- stepAIC(lm5,trace=F) 
  counts <- as.data.frame(table(c(names(coef(step1)),names(coef(step2)),
                                  names(coef(step3)),names(coef(step4)),
                                  names(coef(step5)))),stringsAsFactors=F)
  counts[counts$Freq==5,'Var1']
}

gv <- get_vars(h_imp,'mil10c')

h_form <- paste('mil10c ~ ',paste(gv[-1],collapse =' + '))
h.reg <- with(data=h_imp,exp=lm(as.formula(h_form)))
tmp <- summary(pool(h.reg))
tmp[order(tmp[,5]),c(1,5)]

library(ggplot2)
library(stringr)
source('lm_plot.R')
replace_text <- data.frame(label=c('gi1','mil10a','honqt4','cpss1','pol1',
                                   'mil7','for5_1','aut_idx','for6b'),
                           long=c('Knows name of US president','Mistrusts Chinese govt',
                                  'State enterprises not transparent',
                                  'Plays sports','Interested in politics',
                                  'Armed forces should combat crime',
                                  'Best model: China','Authoritarianism',
                                  'Influence of China in country'),
                           stringsAsFactors=FALSE)
label_offsets <- data.frame(label=c('for5_1','aut_idx','for6b'),
                            x=c(0.1,0.1,0.1),y=c(0,0,0))
flip_us <- c('gi1','cpss1','pol1','for6b')
plotme_hnd <- lm_plot(h.reg,h2,'Mistrust in Iran: Honduras',p_max=0.01,text=replace_text,
                      offsets=label_offsets,flip=flip_us,width=20)

###############################################################################
# Now the entire region
###############################################################################

r <- lapop.2014.all[,unused_common]
is.na(r[r>800000]) <- TRUE
r$ca_idx <- ca_all 
r$tr_idx <- tr_all
r$w_idx <- w_all
r$crit_idx <- crit_all
r$aut_idx <- aut_all
idxs <- c('ca_idx','tr_idx','w_idx','crit_idx','aut_idx')

r2 <- cor_data(r,'mil10c',bin_common,ord_common,idxs,unord_common)

pm_r <- 1 - diag(ncol(r2))
pm_r[,which(names(r2)=='mil10c')] <- 0
r_imp <- mice(r2,printFlag=F,predictorMatrix=pm_r)

gv_r <- get_vars(r_imp,'mil10c')

r_form <- paste('mil10c ~ ',paste(gv_r[-1],collapse =' + '))
r.reg <- with(data=r_imp,exp=lm(as.formula(r_form)))
tmp <- summary(pool(r.reg))
tmp[order(tmp[,5]),c(1,5)]

replace_text <- data.frame(label=c('mil10a','pol1',
                                   'mil7','for5_11','for6b',
                                   'infrax','sexi','for4_1','l1',
                                   'gi4','gi1','env1_1','d6','q2y'),
                           long=c('Mistrusts Chinese govt',
                                  'Interested in politics',
                                  'Armed forces should combat crime',
                                  'Best model: Venezuela',
                                  'Influence of China in country',
                                  'Slow police response time',
                                  'Female interviewer',
                                  'China most influential in 10 yrs',
                                  'Left-wing politics',
                                  'Knows length of presidential term',
                                  'Knows name of US president',
                                  'Protect environment',
                                  'Pro-gay marriage','Younger'),
                           stringsAsFactors=FALSE)
label_offsets <- data.frame(label=c('l1','for4_1','mil7','gi1','sexi'),
                            x=c(0.1,-0.1,0,0,0.3),y=c(0,-0.05,0.02,0.03,0))
flip_us <- c('gi1','gi4','pol1','for6b','l1')
plotme_all <- lm_plot(r.reg,r2,'Mistrust in Iran: Regional',p_max=0.01,text=replace_text,
                      offsets=label_offsets,flip=flip_us,width=20)
#plotme_all <- lm_plot(r.reg,r2,'Mistrust in Iran: Regional',p_max=0.01)
