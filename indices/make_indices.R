# Load required packages

library(mice)
library(plyr)
library(ggplot2)

# Load data

lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
# vic1exta asks about the frequency of crime victimization and is NR if 
# respondant was never victimized; change this to zero
lapop.2014.HND$vic1exta[lapop.2014.HND$vic1exta == 999999] <-  0
lapop.2014.GTM$vic1exta[lapop.2014.GTM$vic1exta == 999999] <- 0
lapop.2014.SLV$vic1exta[lapop.2014.SLV$vic1exta == 999999] <- 0
common <- Reduce(intersect,list(names(lapop.2014.GTM),names(lapop.2014.SLV),
                                names(lapop.2014.HND)))
lapop.2014.all <- rbind(lapop.2014.GTM[,common],lapop.2014.SLV[,common],
                        lapop.2014.HND[,common])

###############################################################################
## Functions we need to construct indices
###############################################################################

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

fear_common <- c('fear10','vic1ext','vic1exta','vic1hogar','aoj11',
                 'pese1','pese2','aoj17','diso7','diso8','diso10','diso18',
                 'diso14','diso16','diso17','vicbar1','vicbar1f','vicbar3',
                 'vicbar4','vicbar7')

# Using the old versions of GTM and SLV indices until I can evaluate new ones.
fear_hnd <- make_idx(lapop.2014.HND,
                     c(fear_common,'vic40','vic41','vic43','vic45','fear6f'))
fear_gtm <- make_idx(lapop.2014.GTM,
                     c(fear_common,'vic40','vic41','vic43','fear6e','fear6f'),
                     sgn=-1)
fear_slv <- make_idx(lapop.2014.SLV,c(fear_common,'vicbar7f','elsdiso18',
                                      'elsdiso19'))
fear_all <- make_idx(lapop.2014.all,fear_common)
# NOTE: in HND, vic40-vic45 take values of 1=yes and 2=no
# in GTM, they take values of 0=no and 1=yes
# This doesn't affect fear_all, since they aren't used at all in SLV; just
# be careful with these particular variables.

###############################################################################
## Community Activity Index
###############################################################################

ca_hnd <- make_idx(lapop.2014.HND,
                   c('cp5','cp7','cp8','cp13','cp20','honcp22','honcp21a'),
                   sgn=-1)
ca_all <- make_idx(lapop.2014.all,
                   c('cp5','cp7','cp8','cp13','cp20'),sgn=-1)
sanity_check(lapop.2014.all,ca_all,'cp7')

###############################################################################
## Political violence Index (Guatemala only)
###############################################################################

pv_data <- lapop.2014.GTM[,c('pv1','pv2a','pv2b','pv2c','pv2d','pv2e','pv2f',
                             'pv2g','pv2h','pv2i','pv2j','pv2k')]
is.na(pv_data[pv_data>800000]) <- TRUE
pv_gtm <- rowSums(2 - pv_data,na.rm=TRUE)

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

###############################################################################
## Wealth index (double-check GTM and SLV)
###############################################################################

w_common <- c('r3','r4','r4a','r5','r6','r7','r8','r12','r14',
              'r15','r18','r1','r16','r26','q10new','q10g')

w_gtm <- make_idx(lapop.2014.GTM,w_common)
w_slv <- make_idx(lapop.2014.SLV,w_common,sgn=-1)
w_hnd <- make_idx(lapop.2014.HND,c(w_common,'inf3a'),sgn=-1)
w_all <- make_idx(lapop.2014.all,w_common)

###############################################################################
## Authoritarianism indices 
###############################################################################

# Sympathy with gov critics
crit_common <- c('d1','d2','d3','d4','e3','e5','e15','e16') 
crit_all <- make_idx(lapop.2014.all,crit_common)
crit_hnd <- make_idx(lapop.2014.HND,crit_common)
# Authoritarianism
# dem2 has a strange scale; switch 1 and 2
aut_hnd_data <- lapop.2014.HND[c('dem2','dem11','aut1','jc13','jc10','jc15a',
                                 'jc16a','honjc17')]
aut_hnd_data$dem2[aut_hnd_data$dem2==1] <- 100
aut_hnd_data$dem2[aut_hnd_data$dem2==2] <- 1
aut_hnd_data$dem2[aut_hnd_data$dem2==100] <- 2
aut_hnd <- make_idx(aut_hnd_data,names(aut_hnd_data),sgn=-1)
aut_all_data <- lapop.2014.all[c('dem2','dem11','jc13','jc10','jc15a')]
#TODO: Haven't really checked into this index much
aut_all_data$dem2[aut_all_data$dem2==1] <- 100
aut_all_data$dem2[aut_all_data$dem2==2] <- 1
aut_all_data$dem2[aut_all_data$dem2==100] <- 2
aut_all <- make_idx(aut_all_data,names(aut_all_data))

# TODO: Clean up the variables that won't be needed by whatever other 
# files call this one.

