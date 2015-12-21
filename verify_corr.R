source('make_indices.R')


###############################################################################
# Which variables aren't included in one of our indices?
###############################################################################

n_fear_more <- c(fear_common,'vic40','vic41','vic43','vic45','fear6f')
n_ca_common <- c('cp5','cp7','cp8','cp13','cp20')
n_ca_more <- c(n_ca_common,'honcp22','honcp21a','cp21')
n_pv_gtm <- c('pv1','pv2a','pv2b','pv2c','pv2d','pv2e','pv2f',
            'pv2g','pv2h','pv2i','pv2j','pv2k')
n_ex_common <- c('exc2','exc6','exc20','exc11','exc13','exc14',
               'exc15','exc16','exc7')
n_tr_more <- c(tr_common,'pr4','m1','b11','esb48','epp1','epp3','pr4',
             'epn3a','epn3b','epn3c','b11','b37','b14','b15','b19',
             'b46','honb51','venb11','venhonb51',
             'venhonvb10','epp1','epp3','aoj18')
n_w_more <- c(w_common,'inf3a')
n_aut_hnd <- c('dem2','dem11','aut1','jc13','jc10','jc15a',
             'jc16a','honjc17')
n_aut_common <- c('dem2','dem11','jc13','jc10','jc15a')
n_geo_more <- c('pais','estratopri','estratosec','upm','prov','municipio',
              'cluster','tamano','hondistrito')
n_dont_use <- c('idnum','fecha','wt','uniq_id','nationality','vb3n','vb11',
              'leng1')

used <- unique(c(n_fear_more,n_ca_more,n_pv_gtm,n_ex_common,n_tr_more,
                 n_w_more,crit_common,n_aut_hnd,n_geo_more,n_dont_use))

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
## Functions to find correlations
###############################################################################
# TODO: modify these so that idx is another column in the same dataframe, not
# an independent vector.
# TODO: put these function definitions in another file so they're not taking
# up space here. Add some comments about exactly what they do.

bin_cor <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between a variable var1 and a binary variable 
  # var2. This uses a Fisher test conditioned on the values of var2, and will
  # be applicable if var1 is continuous (i.e. one of our composite indices).
  res = data.frame(var=character(),est=numeric(),pval=numeric(),
                   stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / nrow(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    tmp$v2 <- tmp$v2 - min(tmp$v2,na.rm=TRUE) # convert to 0-1 scale
    if (sum(tmp$v2,na.rm=TRUE) > 1) {
      tt <- t.test(tmp$v1[tmp$v2==0],tmp$v1[tmp$v2==1])
      if (tt$p.value < cutoff) {
        res=data.frame(var=var2,est=tt$estimate[2]-tt$estimate[1],pval=tt$p.value,
                       stringsAsFactors=FALSE)
      }
    }
  }
  res
}

ord_cor <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between two ordered variables, var1 and var2. 
  # This uses linear regression, and will be applicable when we're dealing
  # with ordered categorical variables or composite indices.
  # As a matter of convention, use the composite index as var1 so that the
  # output makes sense.
  res = data.frame(var=character(),est=numeric(),pval=numeric(),
                   stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / nrow(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    reg <- lm(v1 ~ v2,data=tmp)
    p <- summary(reg)$coefficients[2,4]
    if (p < cutoff) {
      res=data.frame(var=var2,est=summary(reg)$coefficients[2,1],pval=p,
                     stringsAsFactors=FALSE)
    }
  }
  res
}

unord_cor <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between an ordered variable var1 and an unordered
  # categorical variable var2. Do this by creating a binary variable for each
  # possible value of var2 and calling bin_cor().
  vals <- na.omit(unique(data[data[,var2] < 800000,var2]))
  tmp <- data.frame(v1=data[,var1])
  is.na(tmp[tmp>800000]) <- TRUE
  for (x in vals) {
    tmp[,paste(var2,x,sep='_')] <- as.numeric(data[,var2] == x)
  }
  ldply(names(tmp),function(x) bin_cor(tmp,'v1',x,cutoff))
}

###############################################################################
## Now get region-wide correlations
###############################################################################
a <- lapop.2014.all[,unused_common]
is.na(a[a>800000]) <- TRUE
# add in all of the composite indices
a$fear_idx <- fear_all
a$ca_idx <- ca_all
a$tr_idx <- tr_all
a$w_idx <- w_all
a$crit_idx <- crit_all
a$aut_idx <- aut_all
idxs <- c('fear_idx','ca_idx','tr_idx','w_idx','crit_idx','aut_idx')

cor_all_bin <- ldply(bin_common, function(x) bin_cor(a,'fear_idx',x))
cor_all_ord <- ldply(ord_common, function(x) ord_cor(a,'fear_idx',x))
cor_all_idx <- ldply(idxs, function(x) ord_cor(a,'fear_idx',x))
cor_all_unord <- ldply(unord_common, function(x) unord_cor(a,'fear_idx',x))
cor_all <- rbind(cor_all_bin,cor_all_ord,cor_all_unord,cor_all_idx)
cor_all <- cor_all[order(cor_all$pval),]


###############################################################################
# Create a new data frame containing all of the variables we want to use
# in multiple regressions.
###############################################################################

unord_vars <- ldply(cor_all_unord$var, function(x)
  data.frame(var=strsplit(x,'_')[[1]][1],
             val=as.numeric(strsplit(x,'_')[[1]][2]),
             str=x,
             stringsAsFactors=FALSE))
for (i in 1:nrow(unord_vars)) {
  a[,unord_vars$str[i]] <- as.numeric(a[,unord_vars$var[i]] == unord_vars$val[i])
}

# keep only the variables that showed a strong correlation with the fear
# index
a2 <- a[,cor_all$var]
a2$fear_idx <- a$fear_idx

# now impute on the variables that correlate with fear (but don't use fear!)
pm <- 1 - diag(ncol(a2))
pm[,which(names(a2)=='fear_idx')] <- 0
b <- mice(a2,printFlag=F,predictorMatrix=pm)

get_pvals <- function(i) {
  bi <- complete(b,i)
  pvals <- summary(lm(fear_idx ~ .,data=bi))$coefficients[,4]  
  pvals[names(pvals) != '(Intercept)']
}

pvals <- apply(ldply(1:5,get_pvals),2,max)
pvals[pvals<1/nrow(a2)] # apppropriate cutoff = 1/n

# Now we're talking: just 7 correlations that hold up in all 5 imputations
# ur, it1, pole2n, q3c_5, community activity, trust, authoritarianism.

# What if we go from the other direction, gradually removing the weakest correlations?
prune <- function(imp,i,cutoff=0) {
  bi <- complete(b,i)
  pvals <- summary(lm(fear_idx ~ .,data=bi))$coefficients[,4]
  pvals <- pvals[names(pvals) != '(Intercept)']
  if (cutoff == 0) {
    cutoff <- 1 / nrow(bi)
  }
  while (max(pvals > cutoff)) {
    bi <- bi[,-which(names(bi)==names(which.max(pvals)))]
    pvals <- summary(lm(fear_idx ~ .,data=bi))$coefficients[,4]
    pvals <- pvals[names(pvals) != '(Intercept)']
  }
  bi
}

Reduce(intersect,llply(1:5,function(i) names(prune(b,i))))
# The same list as before, with for6 (influence of China) and pol1 (interest in politics) added in.

# one more thing to try: which variables have the lowest p-values in pairwise
# correlations?
get_justone <- function(var) {
  pvals <- sapply(1:5,function(i) 
    summary(lm(fear_all ~ complete(b,i)[,var]))$coefficients[2,4])
  max(pvals)
}
pair_pvals <- sort(sapply(names(a2),get_justone))
pair_pvals[2:11]
# If I ignore multiple correlation effects, then I also see www1, ed2, ed, w_idx, and pn4
# This is important for social media analyses -- while we might expect the average SM user
# to be more educated, wealthy, and computer-literate, people with those qualities aren't,
# on average, more fearful than people who are similar in terms of the variables that came
# out on top in multiple regression.

# Pool results to get coefficients
b.reg <- with(data=b,exp=lm(fear_idx ~ ur + it1 + tr_idx + pole2n + aut_idx + for6 + pol1 + q3c_5 + ca_idx))
b.reg.pool <- pool(b.reg)
summary(b.reg.pool)


