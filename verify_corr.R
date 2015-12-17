###############################################################################
# This is not very pretty, but I wanted to get it out soon and then fix it up
# afterwards. It walks through a few different methods for selecting a core 
# group of highly-correlated variables for the entire N Triangle region.
#
# Next steps:
# 1. Take out the data loading (lines ##-##) and calculation of indices (lines
#    ##-##,##-##) and put them into a separate R package file. That way we have
#    index calculation in a single, authoritative location, and scripts like 
#    this one don't get so long. There should be some way to quality-check the
#    indices that are being calculated by the package and make sure the
#    results are identical to what we see in the index-development Rmd files.
# 2. Find a way to automate the "variable-dropping" procedure on lines ###-###
#    so that it doesn't require any manual entry.
# 3. Along the way, I had a few ideas for additional variables that should be
#    added to indices (in comments). These should be checked carefully and 
#    added into the relevant index-development Rmd files, as well as the 
#    index-calculation package.
# 4. Eventually, this should become an Rmd file for better visual presentation.
###############################################################################

# Load required packages

library(mice)
library(plyr)
library(ggplot2)

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

###############################################################################
# Which variables aren't included in one of our indices?
###############################################################################

fear_common <- c('vic44','fear10','vic1ext','vic1exta','vic1hogar','aoj11',
                 'pese1','pese2','aoj17','diso7','diso8','diso10','diso18',
                 'diso14','diso16','diso17','vicbar1','vicbar1f','vicbar3',
                 'vicbar4','vicbar7')
fear_more <- c(fear_common,'vic40','vic41','vic43','vic45','fear6f','fear6e')
ca_common <- c('cp5','cp7','cp8','cp13','cp20')
ca_more <- c(ca_common,'honcp22','honcp21a','cp21')
pv_gtm <- c('pv1','pv2a','pv2b','pv2c','pv2d','pv2e','pv2f',
            'pv2g','pv2h','pv2i','pv2j','pv2k')
ex_common <- c('exc2','exc6','exc20','exc11','exc13','exc14',
               'exc15','exc16','exc7')
tr_common <- c('b1','b2','b3','b4','b6','b10a','b12','b13','b18','b21','b21a',
               'b32','b47a','n9','n11','n15','b3milx')
tr_more <- c(tr_common,'pr4','m1','b11','esb48','epp1','epp3','pr4',
             'epn3a','epn3b','epn3c','b11','b37','b14','b15','b19',
             'b46','honb51','venb11','venhonb51',
             'venhonvb10','epp1','epp3','aoj18')
w_common <- c('r3','r4','r4a','r5','r6','r7','r8','r12','r14',
              'r15','r18','r1','r16','r26','q10new','q10g')
w_more <- c(w_common,'inf3a')
crit_common <- c('d1','d2','d3','d4','e3','e5','e15','e16')
aut_hnd <- c('dem2','dem11','aut1','jc13','jc10','jc15a',
             'jc16a','honjc17')
aut_common <- c('dem2','dem11','jc13','jc10','jc15a')
geo_more <- c('pais','estratopri','estratosec','upm','prov','municipio',
              'cluster','tamano','hondistrito')
dont_use <- c('idnum','fecha','wt','uniq_id','nationality','vb3n','vb11',
              'leng1')

used <- unique(c(fear_more,ca_more,pv_gtm,ex_common,tr_more,w_common,
                 crit_common,aut_hnd,geo_more,dont_use))

unused_common <- names(lapop.2014.all)[!(names(lapop.2014.all) %in% used)]
unused_gtm <- names(lapop.2014.GTM)[!(names(lapop.2014.GTM) %in% used) & 
                                    !(names(lapop.2014.GTM) %in% unused_common)]
unused_slv <- names(lapop.2014.SLV)[!(names(lapop.2014.SLV) %in% used) & 
                                      !(names(lapop.2014.SLV) %in% unused_common)]
unused_hnd <- names(lapop.2014.HND)[!(names(lapop.2014.HND) %in% used) & 
                                      !(names(lapop.2014.HND) %in% unused_common)]

## Check whether variables are binary

is_binary <- function(data,var) {
  u <- unique(data[,var])
  length(u[u<888888]) == 2
}

bin_common <- unused_common[sapply(unused_common,function(x) 
  is_binary(lapop.2014.all,x))]
bin_gtm <- unused_gtm[sapply(unused_gtm,function(x) 
  is_binary(lapop.2014.GTM,x))]
bin_slv <- unused_slv[sapply(unused_slv,function(x) 
  is_binary(lapop.2014.SLV,x))]
bin_hnd <- unused_hnd[sapply(unused_hnd,function(x) 
  is_binary(lapop.2014.HND,x))]

# Note that, while the indicators vb3n, vb11, and leng1 are used in all three
# countries, the answers are country-specific and shouldn't be used as 
# regional indicators.
unord_common <- c('idiomaq','a4','vic2','vic2aa','aoj22','env1','vb1',
                  'vb4new','vb101','vb20','for1n','for4',
                  'for5','q3c','ocup4a','ocup1a','q11n','etid')
ord_common <- unused_common[!(unused_common %in% bin_common) &
                            !(unused_common %in% unord_common)]
unord_gtm <- c('aoj21','chipart107n','parclien','guaetid2n','leng4','vb3n','vb11','leng1')
ord_gtm <- unused_gtm[!(unused_gtm %in% bin_gtm) &
                        !(unused_gtm %in% unord_gtm)]
unord_slv <- c('esexc16a','elsvb48','pr1','vb3n','vb11','leng1')
ord_slv <- unused_slv[!(unused_slv %in% bin_slv) &
                        !(unused_slv %in% unord_slv)]
unord_hnd <- c('dst1','vb3n','vb11','leng1')
ord_hnd <- unused_hnd[!(unused_hnd %in% bin_hnd) &
                              !(unused_hnd %in% unord_hnd)]

###############################################################################
## Functions we need to construct fear indices
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

fear_common <- c('vic44','fear10','vic1ext','vic1exta','vic1hogar','aoj11',
                 'pese1','pese2','aoj17','diso7','diso8','diso10','diso18',
                 'diso14','diso16','diso17','vicbar1','vicbar1f','vicbar3',
                 'vicbar4','vicbar7')

# Using the old versions of GTM and SLV indices until I can evaluate new ones.
fear_hnd <- make_idx(lapop.2014.HND,
                     c(fear_common,'vic40','vic41','vic43','vic45','fear6f'))
fear_gtm <- make_idx(lapop.2014.GTM,
                     c(fear_common,'vic40','vic41','vic43','fear6f'),
                     sgn=-1)
fear_slv <- make_idx(lapop.2014.SLV,fear_common)
fear_all <- make_idx(lapop.2014.all,fear_common)

# Sanity check: diso8==1 means gangs are a serious problem
sanity_check(lapop.2014.GTM,fear_gtm,'diso8') # 4.242424, 1.468085
sanity_check(lapop.2014.SLV,fear_slv,'diso8') # 4.703209, 1.190476
sanity_check(lapop.2014.HND,fear_hnd,'diso8') # 4.830769, 1.519182
sanity_check(lapop.2014.all,fear_all,'diso8') # 4.593805, 1.383741
# Make sure national indices agree with the regional one
qplot(fear_gtm,fear_all[lapop.2014.all$pais==2]) + theme_classic()
qplot(fear_slv,fear_all[lapop.2014.all$pais==3]) + theme_classic()
qplot(fear_hnd,fear_all[lapop.2014.all$pais==4]) + theme_classic()

###############################################################################
## Functions to find correlations
###############################################################################

bin_cor <- function(data,idx,var,cutoff=0.01) {
  tmp <- data.frame(var=data[,var])
  is.na(tmp[tmp>800000]) <- TRUE
  tmp$var <- tmp$var - min(tmp$var,na.rm=TRUE) # convert to 0-1 scale
  tmp$idx <- idx
  tt <- t.test(tmp$idx[tmp$var==0],tmp$idx[tmp$var==1])
  if (tt$p.value < cutoff) {
    res=data.frame(var=var,est=tt$estimate[2]-tt$estimate[1],pval=tt$p.value,
                   stringsAsFactors=FALSE)
  }
  else {
    res = data.frame(var=character(),est=numeric(),pval=numeric(),
                     stringsAsFactors=FALSE)
  }
  res
}

ord_cor <- function(data,idx,var,cutoff=0.01) {
  tmp <- data.frame(var=data[,var])
  is.na(tmp[tmp>800000]) <- TRUE
  reg <- lm(idx ~ tmp$var)
  p <- summary(reg)$coefficients[2,4]
  if (p < cutoff) {
    res=data.frame(var=var,est=summary(reg)$coefficients[2,1],pval=p,
                   stringsAsFactors=FALSE)
  }
  else {
    res = data.frame(var=character(),est=numeric(),pval=numeric(),
                     stringsAsFactors=FALSE)
  }
  res
}

unord_cor <- function(f,x,categ) {
  tmp <- f[,categ]
  is.na(tmp[tmp>800000]) <- TRUE
  result <- data.frame(var=character(),est=double(),pval=double(),
                       stringsAsFactors=FALSE)
  for(q in unique(tmp)) {
    if(sum(tmp==q,na.rm=TRUE) > 1) {
      yes <- x[tmp==q]
      no <- x[tmp!=q]
      if (sum(!is.na(no)) < 2) return()
      tt <- t.test(yes,no,na.rm=TRUE)
      if (tt$p.value < 0.01) {
        label <- paste(categ,q,sep='_')
        newrow <- data.frame(var=label,
                             est=tt$estimate[[1]] - tt$estimate[[2]],
                             pval=tt$p.value,
                             stringsAsFactors=FALSE)
        result <- rbind(result,newrow)
      }
    }
  }
  result
}

###############################################################################
## Now get region-wide correlations
###############################################################################

cor_all_bin <- ldply(bin_common, function(x) bin_cor(lapop.2014.all,fear_all,x))
cor_all_ord <- ldply(ord_common, function(x) ord_cor(lapop.2014.all,fear_all,x))
cor_all_unord <- ldply(unord_common, function(x) unord_cor(lapop.2014.all,fear_all,x))
cor_all <- rbind(cor_all_bin,cor_all_ord,cor_all_unord)
cor_all <- cor_all[order(cor_all$pval),]

###############################################################################
## Community Activity Index
###############################################################################

ca_hnd <- make_idx(lapop.2014.HND,
                   c('cp5','cp7','cp8','cp13','cp20','honcp22','honcp21a'),
                   sgn=-1)
ca_all <- make_idx(lapop.2014.all,
                   c('cp5','cp7','cp8','cp13','cp20'),sgn=-1)
sanity_check(lapop.2014.all,ca_all,'cp7')

# cp21 in SLV should be added to community activity index

###############################################################################
## Political violence Index (Guatemala only)
###############################################################################

pv_data <- lapop.2014.GTM[,c('pv1','pv2a','pv2b','pv2c','pv2d','pv2e','pv2f',
                             'pv2g','pv2h','pv2i','pv2j','pv2k')]
is.na(pv_data[pv_data>800000]) <- TRUE
pv_gtm <- rowSums(2 - pv_data,na.rm=TRUE)

###############################################################################
## Extorsion Index 
###############################################################################

ex_data <- lapop.2014.all[,c('exc2','exc6','exc20','exc11','exc13','exc14',
                             'exc15','exc16','exc7')]
is.na(ex_data[ex_data>800000]) <- TRUE
ex_all <- rowSums(ex_data,na.rm=TRUE)

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

###############################################################################
# Impute (to avoid bias from missing data) and see which correlations survive.
###############################################################################
a <- lapop.2014.all[,unused_common]
is.na(a[a>800000]) <- TRUE

a$a4_5 <- as.numeric(a$a4==5)  
a$a4_4 <- as.numeric(a$a4==4)  
a$a4_18 <- as.numeric(a$a4==18)  
a$vic2_4 <- as.numeric(a$vic2==4)
a[is.na(a$vic2_4),'vic2_4'] <- 0 # assume NA means no victimization
a$vic2aa_4 <- as.numeric(a$vic2aa==4)
a[is.na(a$vic2aa_4),'vic2aa_4'] <- 0 # assume NA means no victimization
a$aoj22_1 <- as.numeric(a$aoj22==1)
a$aoj22_2 <- as.numeric(a$aoj22==2)
a$env1_2 <- as.numeric(a$env1==2)
a$vb4new_6 <- as.numeric(a$vb4new==6)
a$vb4new_77 <- as.numeric(a$vb4new==77)
a$vb20_4 <- as.numeric(a$vb20==4)
a$for1n_7 <- as.numeric(a$for1n==7)
a$for1n_12 <- as.numeric(a$for1n==12)
a$for4_4 <- as.numeric(a$for4==4)
a$for4_12 <- as.numeric(a$for4==12)
a$for5_6 <- as.numeric(a$for5==6)
a$q3c_1 <- as.numeric(a$q3c==1)
a$q3c_5 <- as.numeric(a$q3c==5)
a$ocup4a_3 <- as.numeric(a$ocup4a==3)
a$ocup4a_5 <- as.numeric(a$ocup4a==5)
a$ocup1a_2 <- as.numeric(a$ocup1a==2)
a$ocup1a_4 <- as.numeric(a$ocup1a==4)
a$q11n_1 <- as.numeric(a$q11n==1)
a$etid_2 <- as.numeric(a$etid==2)

a2 <- a[,cor_all$var]
a2$fear_idx <- fear_all
a2$ca_idx <- ca_all
a2$tr_idx <- tr_all
a2$w_idx <- w_all
a2$crit_idx <- crit_all
a2$aut_idx <- aut_all

# now impute on the variables that correlate with fear
b <- mice(a2,printFlag=F)
# massive multiple correlation
get_pvals <- function(i) {
  bi <- complete(b,i)
  summary(lm(fear_idx ~ .,data=bi))$coefficients[,4]  
}

pvals <- apply(ldply(1:5,get_pvals),2,max)
pvals[pvals<0.01]

# Now we're talking: just 14 correlations that hold up in all 5 imputations
# ur, it1, pole2n, for6, pol1, clien1n, mil10c, ocup4a_3, a4_5, w14a, q3c_5,
# community activity, trust, authoritarianism.

# What if we go from the other direction, gradually removing the weakest correlations?
b1 <- complete(b,5)
# repeat this
pvals[which.max(pvals)]
b1 <- b1[,-which(names(b1)==names(which.max(pvals)))]
pvals <- summary(lm(fear_idx ~ .,data=b1))$coefficients[,4]  
ncol(b1)
names(a2)[!(names(a2) %in% names(b1))]

# when I do this until the largest pval left is < 0.01, then I see the following
list1 <- c('ur','it1','pole2n','sd3new2','for6','pol1','clien1n','mil10c',
           'ocup4a_3','a4_5','w14a','q3c_5','vb20_4','vb4new_77','vic2aa_4',
           'aoj22_1','for4_12','vb4new_6','ca_idx','tr_idx','aut_idx')
list2 <- c('ur','it1','ed2','pole2n','for6','pol1','ocup1a_2','clien1n',
           'mil10c','ocup4a_3','a4_5','w14a','q3c_5','vb20_4','vic2aa_4',
           'for1n_7','aoj22_1','for1n_12','fear_idx','ca_idx','tr_idx',
           'aut_idx')
list3 <- c('ur','it1','ed2','pole2n','for6','pol1','clien1n','mil10c',
           'ocup4a_3','a4_5','w14a','q3c_5','vb20_4','vic2aa_4','aoj22_1',
           'fear_idx','ca_idx','tr_idx','aut_idx')
list4 <- c('ur','it1','ed2','pole2n','sd3new2','for6','ocup1a_4','pol1',
           'clien1n','mil10c','ocup4a_3','a4_5','w14a','q3c_5','for4_4',
           'vic2aa_4','aoj22_1','for4_12','fear_idx','ca_idx','tr_idx','aut_idx')
list5 <- c('ur','it1','pole2n','for6','pol1','ocup1a_2','clien1n','mil10c
           ocup4a_3','a4_5','w14a','q3c_5','vb20_4','vb4new_77','aoj22_1',
           'for4_12','gix4','fear_idx','ca_idx','tr_idx','aut_idx')

list1[list1 %in% list2 &
        list1 %in% list3 &
        list1 %in% list4 &
        list1 %in% list5]

# ur, it1, pole2n, for6, pol1, clien1n, a4_5, w14a, q3c_5, aoj22_1, ca_idx, tr_idx, aut_idx
# compare to
pvals <- apply(ldply(1:5,get_pvals),2,max)
pvals[pvals<0.01]
# I'm impressed at how consistent the results are -- I need a way to automate this.
# I no longer have mil10c (trust in Iran), ocup4a_3 (job seekers)
# only the "constricting" approach gives me aoj22_1 (how to reduce crime)

summary(lm(fear_all ~ a2$ur + a2$it1 + a2$pole2n + a2$for6 + a2$pol1 + 
             a2$clien1n + a2$a4_5 + a2$w14a + a2$q3c_5 + ca_all + tr_all + aut_all))

summary(lm(fear_idx ~ ur + it1 + pole2n + for6 + pol1 + 
             clien1n + a4_5 + w14a + q3c_5 + ca_idx + tr_idx + aut_idx + aoj22_1,data=b1))

# one more thing to try: which variables have the lowest p-values in pairwise
# correlations?
get_justone <- function(var) {
  pvals <- sapply(1:5,function(i) 
    summary(lm(fear_all ~ complete(b,i)[,var]))$coefficients[2,4])
  max(pvals)
}
pair_pvals <- sort(sapply(names(a2),get_justone))
pair_pvals[2:16]

b1 <- complete(b,1)
summary(lm(fear_idx ~ ur + it1 + www1 + tr_idx + pole2n + ed2 + ed + w_idx + aut_idx + pn4 + sd3new2 + for6 + sd6new2 + ocup1a_4 + pol1,data=b1))
