# One of the focus areas for USAID/Honduras is the Western Highlands; I want
# to do something that will focus (at least a little) on this region.

# My main question: Is there anything that makes the Western Highlands different
# from the rest of the country?

library(mice)
library(MASS)

source('make_indices.R')
source('make_geo.R',encoding='utf-8')
# lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
# lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
# lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
# # vic1exta asks about the frequency of crime victimization and is NR if 
# # respondant was never victimized; change this to zero
# lapop.2014.HND$vic1exta[lapop.2014.HND$vic1exta == 999999] <-  0
# lapop.2014.GTM$vic1exta[lapop.2014.GTM$vic1exta == 999999] <- 0
# lapop.2014.SLV$vic1exta[lapop.2014.SLV$vic1exta == 999999] <- 0
source('classify_vars.R')

# Construct a variable called in_west that will tell us whether each response
# came from one of the W Highlands departments.

west <- c('Ocotepeque','Copan','Santa Barbara','Lempira','Intibuca','La Paz')
geo_hnd <- my_geo[my_geo$pais_text == 'Honduras',]
in_west <- as.numeric(geo_hnd$prov_text %in% west)
unique(geo_hnd$prov_text[as.logical(in_west)]) # sanity check
mean(in_west) # 20% of HND responses in W Highlands

# What we need now is to see which variables correlate with in_west. These functions 
# will be similar to the ones in verify_corr.Rmd, but will rely more on t-tests
# and Fisher exact tests, rather than linear regressions.

h <- lapop.2014.HND[,c(unused_common,unused_hnd)]
is.na(h[h>800000]) <- TRUE
h$fear_idx <- fear_hnd
h$ca_idx <- ca_hnd 
h$tr_idx <- tr_hnd
h$w_idx <- w_hnd
h$crit_idx <- crit_hnd
h$aut_idx <- aut_hnd
h$in_west <- in_west
idxs <- c('fear_idx','ca_idx','tr_idx','w_idx','crit_idx','aut_idx')

# This is copied from verify_corr.Rmd; adapted for 
# present use. 

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

h2 <- cor_data_bin(h,'in_west',c(bin_common,bin_hnd),c(ord_common,ord_hnd),idxs,
               c(unord_common,unord_hnd))
pm_h <- 1 - diag(ncol(h2))
pm_h[,which(names(h2)=='in_west')] <- 0
h_imp <- mice(h2,printFlag=F,predictorMatrix=pm_h)

# This is also borrowed from verify_corr.Rmd; take the more flexible version
# and factor it out into another source file

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

gv <- get_vars(h_imp,'in_west')

h_form <- 'in_west ~ a4_30 + colori + colorr + d5 + eff1 + etid_3 + etid_4 + fear_idx + 
           for6 + gi0 + honmun37 + honqt4 + infrax + mil10c + mil4 + pr3a + 
           sd6new2 + sexi + tr_idx + ur + vb2 + w_idx + wf1'

h_form <- paste('in_west ~ ',paste(gv[-1],collapse =' + '))
h.reg <- with(data=h_imp,exp=lm(as.formula(h_form)))
tmp <- summary(pool(h.reg))
tmp[order(tmp[,5]),c(1,5)]

# I'll wait to generate a visualization until I've fixed some of the issues with
# that piece of code, but we can already see what correlates with responses
# in the Western Highlands
#
# colori: darker-skinned researchers (not very interesting)
# sexi: more female researchers (ditto)
# ur: more rural
# mil4: don't want cooperation with US military
# honmun37: mayor's office reports on investments
mean(h[h$in_west==1,'honmun37']==1,na.rm=T) # 32.1%
mean(h[h$in_west==0,'honmun37']==1,na.rm=T) # 14.6%
# infrax: Speedy police response times
# colorr: lighter-skinned
# etid_3: indigenous
# pr3a: pirating CDs results in punishment
# w_idx: less wealthy
# honqt4: more transparency in state enterprises
# fear_idx: less fearful
# d5: disapprove of homosexuals running for office
# for6: less influence of China
# wf1: receive more gov assistance
# sd6new2: more satisfied with public health services
# tr_idx: less trust in gov

summary(lm(h2$in_west ~ h2$a4_30))
# a4 == 30 is a large effect, even though it loses significance
mean(h2[h2$in_west==1,'a4_30']==1,na.rm=T) # 1.3%
mean(h2[h2$in_west==0,'a4_30']==1,na.rm=T) # 0.08%
# and that's why.
