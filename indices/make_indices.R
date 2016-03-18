# Load required packages

library(mice)
library(plyr)
library(ggplot2)

wd <- getwd()
setwd("C:/Users/Craig/Desktop/SMA/VSFS")

# Load data

lapop.2014.HND <- read.csv("HND-2014.csv",stringsAsFactors=FALSE)
lapop.2014.GTM <- read.csv("GTM-2014.csv",stringsAsFactors=FALSE)
lapop.2014.SLV <- read.csv("SLV-2014.csv",stringsAsFactors=FALSE)
# vic1exta asks about the frequency of crime victimization and is NR if 
# respondant was never victimized; change this to zero
lapop.2014.HND$vic1exta[lapop.2014.HND$vic1exta > 800000] <-  0
lapop.2014.GTM$vic1exta[lapop.2014.GTM$vic1exta > 800000] <- 0
lapop.2014.SLV$vic1exta[lapop.2014.SLV$vic1exta > 800000] <- 0
lapop.2014.GTM$a4_crime <- as.numeric(lapop.2014.GTM$a4 %in% c(5,14,27,30,57))
lapop.2014.SLV$a4_crime <- as.numeric(lapop.2014.SLV$a4 %in% c(5,14,27,30,57))
lapop.2014.HND$a4_crime <- as.numeric(lapop.2014.HND$a4 %in% c(5,14,27,30,57))
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
  my_data <- data[,vars]
  is.na(my_data[my_data>800000]) <- TRUE
  my_data <- my_data[,order(names(my_data))] # alphabetize columns
  print(names(my_data))
  print(sum(my_data,na.rm=TRUE))
  my_imp <- mice(my_data,printFlag=F,seed=seed)
  print(sum(complete(my_imp,1)))
  my_pr <- lapply(1:5,function(x) 
    prcomp(complete(my_imp,x),scale=scale,center=TRUE))
  all_pc1 <- data.frame(llply(1:5, function(i) my_pr[[i]]$x[,1]))
  for (i in 2:ncol(all_pc1)) {  # This isn't quite right yet.
    if (cor(all_pc1[,1],all_pc1[,2]) < 0) {
      all_pc1[,2] <- -1 * all_pc1[,2]
    }
  }
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
                 'vicbar4','vicbar7','a4_crime')

# Using the old versions of GTM and SLV indices until I can evaluate new ones.
fear_hnd <- make_idx(lapop.2014.HND,
                     c(fear_common,'vic40','vic41','vic43','vic44','vic45','fear6f'))
fear_gtm <- make_idx(lapop.2014.GTM,
                     c(fear_common,'vic40','vic41','vic43','fear6e','fear6f'),
                     sgn=-1)
fear_slv <- make_idx(lapop.2014.SLV,c(fear_common,'vicbar7f','elsdiso18',
                                      'elsdiso19'),sgn=-1)
fear_all <- make_idx(lapop.2014.all,fear_common,sgn=-1)

# to check against Rmd
quantile(fear_gtm) # -2.14888741 -0.78978002 -0.07731749  0.70524297  2.36829420
  # Matches now! I think alphabetizing columns is the trick.
quantile(fear_slv) # -1.6617010 -0.8657568 -0.2116029  0.7632114  2.4428460
quantile(fear_hnd) # -1.66495546 -0.78420207 -0.09084414  0.74054566  2.33662394  
quantile(fear_all) # -1.6687644 -0.7750963 -0.1055477  0.7350126  2.6638733 
# NOTE: in HND, vic40-vic45 take values of 1=yes and 2=no
# in GTM, they take values of 0=no and 1=yes
# This doesn't affect fear_all, since they aren't used at all in SLV; just
# be careful with these particular variables.

###############################################################################
## Community Activity Index
###############################################################################

ca_hnd <- make_idx(lapop.2014.HND,
                   c('cp5','cp7','cp8','cp13','cp20','honcp22','honcp21a'),
                   sgn=1)
ca_all <- make_idx(lapop.2014.all,
                   c('cp5','cp7','cp8','cp13','cp20'),sgn=1)

quantile(ca_hnd) # -1.187014097 -0.956660605 -0.009779228  0.637884956  3.669760977 
quantile(ca_all) # -1.07551709 -1.02637075 -0.07517033  0.66139991  4.55413689 

sanity_check(lapop.2014.all,ca_all,'cp7')

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

quantile(tr_gtm) # -2.44517711 -0.72199661 -0.03433949  0.62477781  3.18774069 
quantile(tr_slv) # -2.67290171 -0.71403085  0.04851013  0.73589602  2.38045355 
quantile(tr_hnd) # -2.45365500 -0.70820950  0.06972997  0.73113641  2.62630907 
quantile(tr_all) # -2.5103722094 -0.6908681253  0.0005998971  0.7143599120  2.6595092646

###############################################################################
## Wealth index 
###############################################################################

w_common <- c('r3','r4','r4a','r5','r6','r7','r8','r12','r14',
              'r15','r18','r1','r16','r26','q10new','q10g')

w_gtm <- make_idx(lapop.2014.GTM,w_common)
w_slv <- make_idx(lapop.2014.SLV,w_common,sgn=-1)
w_hnd <- make_idx(lapop.2014.HND,c(w_common,'inf3a'),sgn=-1)
w_all <- make_idx(lapop.2014.all,w_common)

quantile(w_all) # -1.6108186 -0.8521016 -0.1225483  0.7726567  2.3916136 

###############################################################################
## Authoritarianism indices 
###############################################################################

# Sympathy with gov critics
crit_common <- c('d1','d2','d3','d4','e3','e5','e15','e16') 
crit_all <- make_idx(lapop.2014.all,crit_common)
crit_hnd <- make_idx(lapop.2014.HND,crit_common)

quantile(crit_hnd) # -1.9932752 -0.6620249 -0.1337136  0.5735625  2.7728628 
quantile(crit_all) # -1.8595760 -0.6803297 -0.0801730  0.5911112  3.1651646 

# Authoritarianism
# dem2 has a strange scale; switch 1 and 2
aut_hnd_data <- lapop.2014.HND[c('dem2','dem11','aut1','jc13','jc10','jc15a',
                                 'jc16a','honjc17')]
aut_hnd_data$dem2[aut_hnd_data$dem2==1] <- 100
aut_hnd_data$dem2[aut_hnd_data$dem2==2] <- 1
aut_hnd_data$dem2[aut_hnd_data$dem2==100] <- 2
aut_hnd <- make_idx(aut_hnd_data,names(aut_hnd_data),sgn=1)
aut_all_data <- lapop.2014.all[c('dem2','dem11','jc13','jc10','jc15a')]
aut_all_data$dem2[aut_all_data$dem2==1] <- 100
aut_all_data$dem2[aut_all_data$dem2==2] <- 1
aut_all_data$dem2[aut_all_data$dem2==100] <- 2
aut_all <- make_idx(aut_all_data,names(aut_all_data))

quantile(aut_hnd) # -1.5323127 -0.6685949 -0.5160627  0.6448696  2.4573383 
quantile(aut_all) # -0.8249519 -0.8249519 -0.3775576  0.1469249  2.7204588 

rm(aut_all_data,aut_hnd_data)

setwd(wd)
rm(wd)

