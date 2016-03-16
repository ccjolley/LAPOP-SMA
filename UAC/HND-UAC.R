# Look for unique attributes of the municipalities from which the most UACs 
# came in 2014 and 2015.

library(MASS)

source('make_indices.R')
source('make_geo.R')
source('classify_vars.R')

UAC_muni <- c('Distrito Central','Juticalpa','La Ceiba','Tocoa','Choluteca','Tela',
              'El Progreso','Omoa','Catacamas','Comayagua','Olanchito',
              'San Pedro Sula')

muni <- unique(my_geo$muni_text)

tmp <- my_geo[my_geo$muni_text %in% UAC_muni & my_geo$pais_text == 'Honduras',]
unique(tmp$muni_text) # nothing from Juticalpa, Omoa


h <- lapop.2014.HND[,c(unused_common,unused_hnd)]
hnd_geo <- my_geo[my_geo$pais_text=='Honduras',]
is.na(h[h>800000]) <- TRUE
h$fear_idx <- fear_hnd
h$ca_idx <- ca_hnd 
h$tr_idx <- tr_hnd
h$w_idx <- w_hnd
h$crit_idx <- crit_hnd
h$aut_idx <- aut_hnd
h$uac <- as.numeric(hnd_geo$muni_text %in% UAC_muni)
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

h2 <- cor_data_bin(h,'uac',c(bin_common,bin_hnd),c(ord_common,ord_hnd),idxs,
                   c(unord_common,unord_hnd))
pm_h <- 1 - diag(ncol(h2))
pm_h[,which(names(h2)=='uac')] <- 0
h_imp <- mice(h2,printFlag=F,predictorMatrix=pm_h)

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

gv <- get_vars(h_imp,'uac')

h_form <- paste('uac ~ ',paste(gv[-1],collapse =' + '))
h.reg <- with(data=h_imp,exp=lm(as.formula(h_form)))
tmp <- summary(pool(h.reg))
tmp[order(tmp[,5]),c(1,5)]