# Load required packages

library(mice)
library(plyr)

# Load data

lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
lapop.2014.HND$vic1exta[lapop.2014.HND$vic1exta == 999999] <-  0
lapop.2014.GTM$vic1exta[lapop.2014.GTM$vic1exta == 999999] <- 0
lapop.2014.SLV$vic1exta[lapop.2014.SLV$vic1exta == 999999] <- 0
common <- Reduce(intersect,list(names(lapop.2014.GTM),names(lapop.2014.SLV),
                                names(lapop.2014.HND)))
lapop.2014.all <- rbind(lapop.2014.GTM[,common],lapop.2014.SLV[,common],
                        lapop.2014.HND[,common])

# This function will do all our work for us

make_idx <- function(data,vars,sgn=1,scale=FALSE,seed=12345) {
  # Given a dataset 'data' containing variables 'vars', construct a
  # composite index from the first principal component after multiple
  # imputation. 
  # Set sgn=-1 to reverse the direction of the index.
  # Set scale=TRUE to use scaling during the PCA calculation
  set.seed(seed)
  my_data <- data[,vars]
  is.na(my_data[my_data>800000]) <- TRUE
  my_imp <- mice(my_data,printFlag=F)
  my_pr <- lapply(1:5,function(x) 
    prcomp(complete(my_imp,x),scale=scale,center=TRUE))
  all_pc1 <- data.frame(llply(1:5, function(i) my_pr[[i]]$x[,1]))
  avg <- rowMeans(all_pc1)
  scale(sgn*avg)
}

# This one will do some other stuff

sanity_check <- function(data,idx,var) {
  # Return the average values of 'var' in 'data' for the lowest and highest
  # quartiles of idx
  v <- data[,var]
  is.na(v[v>800000]) <- TRUE
  lo <- mean(v[idx <= quantile(idx)[2]],na.rm=TRUE)
  hi <- mean(v[idx >= quantile(idx)[4]],na.rm=TRUE)
  c(lo,hi)
}
  


###############################################################################
## Fear index
###############################################################################

fear_common <- c('vic44','fear10','vic1ext','vic1exta','vic1hogar','aoj11',
                 'pese1','pese2','aoj17','diso7','diso8','diso10','diso18',
                 'diso14','diso16','diso17','vicbar1','vicbar1f','vicbar3',
                 'vicbar4','vicbar7')

fear_hnd <- make_idx(lapop.2014.HND,
                     c(fear_common,'vic40','vic41','vic43','vic45','fear6f'))
fear_gtm <- make_idx(lapop.2014.GTM,
                     c(fear_common,'vic40','vic41','vic43','fear6e','fear6f'),
                     sgn=-1)
fear_slv <- make_idx(lapop.2014.SLV,c(fear_common,'elsdiso18','elsdiso19'))
fear_all <- make_idx(lapop.2014.all,fear_common)

# Sanity check: diso8==1 means gangs are a serious problem
sanity_check(lapop.2014.GTM,fear_gtm,'diso8') # 4.242424, 1.468085
sanity_check(lapop.2014.SLV,fear_slv,'diso8') # 4.703209, 1.190476
sanity_check(lapop.2014.HND,fear_hnd,'diso8') # 4.830334, 1.515385
sanity_check(lapop.2014.all,fear_all,'diso8') # 4.593805, 1.383741
# Make sure national indices agree with the regional one
qplot(fear_gtm,fear_all[lapop.2014.all$pais==2]) + theme_classic()
qplot(fear_slv,fear_all[lapop.2014.all$pais==3]) + theme_classic()
qplot(fear_hnd,fear_all[lapop.2014.all$pais==4]) + theme_classic()

###############################################################################
## Community Activity Index (Honduras only)
###############################################################################

ca_hnd <- make_idx(lapop.2014.HND,
                   c('cp5','cp7','cp8','cp13','cp20','honcp22','honcp21a'),
                   sgn=-1)
# Sanity check: cp7==1 often attends meetings of parents' asssociation
sanity_check(lapop.2014.HND,ca_hnd,'cp7') # Agrees with Jorge's results

summary(lm(fear_hnd ~ ca_hnd))
# Borderline signficance (p = 0.0367), with a very small positive coefficient.
# Looks like very-engaged people are slightly more fearful?

ca_all <- make_idx(lapop.2014.all,
                   c('cp5','cp7','cp8','cp13','cp20'),sgn=-1)
sanity_check(lapop.2014.all,ca_all,'cp7')

summary(lm(fear_all ~ ca_all))
# Significance actually gets much higher when we look at the entire region
# and not just HND. The effect is small, but highly-engaged people are 
# somewhat more fearful. Given that engagement is relatively rare, it's worth
# asking what else makes them unusual.

# cp21 in SLV should be added to community activity index

###############################################################################
## Political violence Index (Guatemala only)
###############################################################################

# Political Violence.Rmd showed that a simple sum worked better 
# than PCA in this case

pv_data <- lapop.2014.GTM[,c('pv1','pv2a','pv2b','pv2c','pv2d','pv2e','pv2f',
                             'pv2g','pv2h','pv2i','pv2j','pv2k')]
is.na(pv_data[pv_data>800000]) <- TRUE
pv_gtm <- rowSums(2 - pv_data,na.rm=TRUE)
summary(lm(fear_gtm ~ pv_gtm))
# Massively significant correlation, not suprisingly

###############################################################################
## Extorsion Index 
###############################################################################

# Extorsion_Index.Rmd showed that a simple sum worked better 
# than PCA in this case

ex_data <- lapop.2014.all[,c('exc2','exc6','exc20','exc11','exc13','exc14',
                              'exc15','exc16','exc7')]
is.na(ex_data[ex_data>800000]) <- TRUE
ex_all <- rowSums(ex_data,na.rm=TRUE)

summary(lm(fear_all ~ ex_all))
# no significant correlation

###############################################################################
## Trust in government index
###############################################################################
tr_common <- c('b1','b2','b3','b4','b6','b10a','b12','b13','b18','b21','b21a',
               'b32','b47a','n9','n11','n15','b3milx')

tr_gtm <- make_idx(lapop.2014.GTM,c(tr_common,'pr4','m1'))
tr_slv <- make_idx(lapop.2014.SLV,c(tr_common,'b11','esb48','epp1','epp3','pr4',
                                    'epn3a','epn3b','epn3c','aoj18'))
tr_hnd <- make_idx(lapop.2014.HND,c(tr_common,'b11','b37','b14','b15','b19',
                                    'b46','honb51','venb11','venhonb51',
                                    'venhonvb10','epp1','epp3'),
                   sgn=-1)
tr_all <- make_idx(lapop.2014.all,tr_common)
                    
# Sanity check: b47a==1 means no trust in elections
sanity_check(lapop.2014.GTM,tr_gtm,'b47a') # 1.899183, 4.991979
sanity_check(lapop.2014.SLV,tr_slv,'b47a') # 2.437666, 6.005319
sanity_check(lapop.2014.HND,tr_hnd,'b47a') # 1.617949, 5.366925
sanity_check(lapop.2014.all,tr_all,'b47a') # 1.952339, 5.464851
# Do country and regional indices agree?
qplot(tr_gtm,tr_all[lapop.2014.all$pais==2]) + theme_classic()
qplot(tr_slv,tr_all[lapop.2014.all$pais==3]) + theme_classic()
qplot(tr_hnd,tr_all[lapop.2014.all$pais==4]) + theme_classic()

summary(lm(fear_hnd ~ tr_hnd)) # very significant
summary(lm(fear_gtm ~ tr_gtm)) # also
summary(lm(fear_slv ~ tr_slv)) # also
summary(lm(fear_all ~ tr_all)) # even more so

###############################################################################
## Wealth index (double-check GTM and SLV)
###############################################################################

w_common <- c('r3','r4','r4a','r5','r6','r7','r8','r12','r14',
              'r15','r18','r1','r16','r26','q10new','q10g')

w_gtm <- make_idx(lapop.2014.GTM,w_common)
w_slv <- make_idx(lapop.2014.SLV,w_common,sgn=-1)
w_hnd <- make_idx(lapop.2014.HND,c(w_common,'inf3a'),sgn=-1)
w_all <- make_idx(lapop.2014.all,w_common)
# Sanity check: q10new==0 means no income
sanity_check(lapop.2014.GTM,w_gtm,'q10new') # 1.649025, 12.612813
sanity_check(lapop.2014.SLV,w_slv,'q10new') # 2.917614, 14.167665
sanity_check(lapop.2014.HND,w_hnd,'q10new') # 1.304582, 12.397222
sanity_check(lapop.2014.all,w_all,'q10new') # 1.75046, 13.17888

summary(lm(fear_all ~ w_all)) # very significant; wealthy are more fearful

###############################################################################
## Authoritarianism indices 
###############################################################################

# Sympathy with gov critics
crit_common <- c('d1','d2','d3','d4','e3','e5','e15','e16') 
crit_all <- make_idx(lapop.2014.all,crit_common)
crit_hnd <- make_idx(lapop.2014.HND,crit_common)
# Sanity check: d1==1 means disapproval of critics' right to vote
sanity_check(lapop.2014.HND,crit_hnd,'d1') # 2.665796, 8.196335
sanity_check(lapop.2014.all,crit_all,'d1') # 2.382940, 7.657143

summary(lm(fear_all ~ crit_all)) # significant; fearful people more sympathetic

# Authoritarianism
# dem2 has a strange scale; switch 1 and 2
aut_hnd_data <- lapop.2014.HND[c('dem2','dem11','aut1','jc13','jc10','jc15a',
                                 'jc16a','honjc17')]
aut_hnd_data$dem2[aut_hnd_data$dem2==1] <- 100
aut_hnd_data$dem2[aut_hnd_data$dem2==2] <- 1
aut_hnd_data$dem2[aut_hnd_data$dem2==100] <- 2
aut_hnd <- make_idx(aut_hnd_data,names(aut_hnd_data),sgn=-1)
# Sanity check: Now, dem2==1 means democracy is preferable to all others
sanity_check(aut_hnd_data,aut_hnd,'dem2')
summary(lm(fear_hnd ~ aut_hnd)) # very significant and positive

aut_all_data <- lapop.2014.all[c('dem2','dem11','jc13','jc10','jc15a')]
#TODO: Haven't really checked into this index much
aut_all_data$dem2[aut_all_data$dem2==1] <- 100
aut_all_data$dem2[aut_all_data$dem2==2] <- 1
aut_all_data$dem2[aut_all_data$dem2==100] <- 2
aut_all <- make_idx(aut_all_data,names(aut_all_data))
sanity_check(aut_all_data,aut_all,'dem2')
summary(lm(fear_all ~ aut_all)) # very significant and positive

###############################################################################
## Multiple regression
###############################################################################

# These were highly significant in the Honduras multiple regression
sig_hnd <- lapop.2014.HND[,c('ur','sexi','per4','sd3new2','ico2','pole2n')]
is.na(sig_hnd[sig_hnd > 800000]) <- TRUE

summary(lm(fear_hnd ~ sig_hnd$ur + sig_hnd$sexi + 
             sig_hnd$per4 + sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))
# Actually, per4 is less significant here than it was (in fear_correlations.Rmd)
# when I included more variables.

summary(lm(fear_hnd ~ sig_hnd$ur + sig_hnd$sexi + 
             sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))
# All highly significant (especially ur)

summary(lm(fear_hnd ~ tr_hnd + w_hnd + crit_hnd + aut_hnd + sig_hnd$ur + 
             sig_hnd$sexi + sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))
# Trust in government and authoritarianism are still significant when we 
# control for everything else we were looking at; wealth less so, sympathy 
# with critics is gone completely.
summary(lm(fear_hnd ~ tr_hnd + aut_hnd + sig_hnd$ur + 
             sig_hnd$sexi + sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))


# What happened to wealth?

summary(lm(w_hnd ~ sig_hnd$ur)) # urban people significantly more wealthy
summary(lm(w_hnd ~ sig_hnd$ur + sig_hnd$sexi + 
             sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n)) 
# Wealthy people also unimpressed by police, much more likely to get female 
# interviewers (Did they send men to the bad neighborhoods?)
summary(lm(fear_hnd ~ w_hnd))
summary(lm(fear_hnd ~ w_hnd + sig_hnd$ur)) # urbanization destroys the wealth-fear correlation
summary(lm(fear_hnd ~ w_hnd + sig_hnd$sexi)) # wealth hurts the interviewer-sex correlation
summary(lm(fear_hnd ~ w_hnd + sig_hnd$sexi + sig_hnd$ur)) # wealth doesn't matter
summary(lm(fear_hnd ~ tr_hnd + w_hnd + crit_hnd + aut_hnd + sig_hnd$ur + 
             sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n)) 
# so actually wealth matters when we take interviewer sex out of it!

# What happened to sympathy with critics?

summary(lm(crit_hnd ~ sig_hnd$ur)) # urban people less sympathetic
summary(lm(fear_hnd ~ crit_hnd + sig_hnd$ur)) # no more correlation

###############################################################################
# Diving deeper: Community activity index
###############################################################################
 
summary(lm(ca_all ~ ex_all))
# Significant and positive; highly-engaged people paid more bribes
summary(lm(ca_all ~ tr_all)) # not significant
summary(lm(ca_all ~ w_all)) # wealthy people are less engaged
summary(lm(ca_all ~ crit_all)) # not really significant
summary(lm(ca_all ~ aut_all)) # not significant

summary(lm(ca_all ~ ex_all + w_all)) # nothing changes much
summary(lm(fear_all ~ ca_all + w_all))

summary(lm(fear_hnd ~ tr_hnd + aut_hnd + sig_hnd$ur + 
             sig_hnd$sexi + sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))

# Honduras-specific
summary(lm(fear_hnd ~ tr_hnd + aut_hnd + ca_hnd + sig_hnd$ur + 
             sig_hnd$sexi + sig_hnd$sd3new2 + sig_hnd$ico2 + sig_hnd$pole2n))
# Community activity retains its small but significant effect

