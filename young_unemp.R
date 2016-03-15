# One focus area for the HND mission is the young and unemployed -- what can
# LAPOP data tell us about this demographic?

library(MASS)

source('make_geo.R',encoding='utf-8')

mydata <- lapop.2014.HND[,c('q2','ocup4a')]
mydata[mydata > 800000] <- NA
mydata <- cbind(mydata,my_geo[my_geo$pais_text=='Honduras',])

mean(mydata$q2 < 30,na.rm=TRUE) # 33% less than 30
mean(mydata$ocup4a==3,na.rm=TRUE) # 6.7% unemployed

mydata$yu <- as.numeric(mydata$q2 < 30 & mydata$ocup4a==3)
mean(mydata$yu,na.rm=TRUE) # 3.9% young and unemployed

# Follow steps in west.R

source('make_indices.R')
source('classify_vars.R')

h <- lapop.2014.HND[,c(unused_common,unused_hnd)]
is.na(h[h>800000]) <- TRUE
h$fear_idx <- fear_hnd
h$ca_idx <- ca_hnd 
h$tr_idx <- tr_hnd
h$w_idx <- w_hnd
h$crit_idx <- crit_hnd
h$aut_idx <- aut_hnd
h$yu <- mydata$yu
idxs <- c('fear_idx','ca_idx','tr_idx','w_idx','crit_idx','aut_idx')

cor_data_bin <- function(d,my_var,bin_vars,ord_vars,idx_vars,unord_vars) {
  cor_bin <- ldply(bin_vars, function(x) bin_bin(d,x,my_var)) 
  cor_ord <- ldply(ord_vars, function(x) ord_bin(d,x,my_var)) 
  cor_idx <- ldply(idx_vars, function(x) ord_bin(d,x,my_var)) 
  cor_unord <- ldply(unord_vars, function(x) unord_bin(d,x,my_var))
  cor_all <- rbind(cor_bin,cor_ord,cor_unord,cor_idx)
  cor_all <- cor_all[order(cor_all$pval),]
  unord_list <- ldply(cor_unord$var1, function(x)
    data.frame(var=strsplit(x,'_')[[1]][1],
               val=as.numeric(strsplit(x,'_')[[1]][2]),
               str=x,
               stringsAsFactors=FALSE))
  for (i in 1:nrow(unord_list)) {
    d[,unord_list$str[i]] <- as.numeric(d[,unord_list$var[i]] == unord_list$val[i])
  }
  res <- d[,cor_all$var1]
  res[,my_var] <- d[,my_var]
  res
}

h2 <- cor_data_bin(h,'yu',c(bin_common,bin_hnd),c(ord_common,ord_hnd),idxs,
                   c(unord_common,unord_hnd))
# We'll need to remove the variables used to make yu: ocup4a, q2y, q2
# These have, of course, very high correlations and are the first three entries.
h2 <- h2[,!names(h2) %in% c('ocup4a_3','ocup4a_1','ocup4a_5','q2y','q2')]

pm_h <- 1 - diag(ncol(h2))
pm_h[,which(names(h2)=='yu')] <- 0
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

gv <- get_vars(h_imp,'yu')

h_form <- paste('yu ~ ',paste(gv[-1],collapse =' + '))
h.reg <- with(data=h_imp,exp=lm(as.formula(h_form)))
tmp <- summary(pool(h.reg))
tmp[order(tmp[,5]),c(1,5)]

# So what makes the young and unemployed of Honduras special?
# More likely to be single
# Play more sports
# Don't see themselves as having anxious personalities
# More fearful
# More trust in Evangelical churches
# Intend to live/work abroad
# Fewer children
# Not religious

# It might be better to ask what makes them different from other young people...

hy <- h[h$q2 < 30,] # only 506 people
h2y <- cor_data_bin(hy,'yu',c(bin_common,bin_hnd),c(ord_common,ord_hnd),idxs,
                   c(unord_common,unord_hnd))
# We'll need to remove the variables used to make yu: ocup4a, q2y, q2
# These have, of course, very high correlations and are the first three entries.
h2y <- h2y[,!names(h2y) %in% c('ocup4a_3','ocup4a_1','ocup4a_4','ocup4a_5','q2y','q2')]

pm_hy <- 1 - diag(ncol(h2y))
pm_hy[,which(names(h2y)=='yu')] <- 0
hy_imp <- mice(h2y,printFlag=F,predictorMatrix=pm_hy)

gvy <- get_vars(hy_imp,'yu')

hy_form <- paste('yu ~ ',paste(gvy[-1],collapse =' + '))
hy.reg <- with(data=hy_imp,exp=lm(as.formula(hy_form)))
tmp <- summary(pool(hy.reg))
tmp[order(tmp[,5]),c(1,5)]

# Don't consider themselves anxious
# Not impressed with municipal services
# More trust in Evangelical churches
# Intend to work/study abroad
# More fearful
# Fewer children

# What about the locations where they live? 

fn <- function(muni) {
  t <- table(mydata$muni_text==muni,mydata$yu)
  ft <- fisher.test(t)
  data.frame(m=muni,pval=ft$p.value,est=ft$estimate)
}

pvals <- ldply(unique(mydata$muni_text), fn)
pvals[pvals$pval < 0.05,]

# I'm curious to see what the interpolated map looks like...

