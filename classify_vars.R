# NOTE: Make sure you always source this *after* make_indices.R

###############################################################################
# To systematically explore all possible correlations with another variable, 
# we first need to see which haven't been accounted for in one of our  
# composite indices.
###############################################################################

n_fear_more <- c(fear_common,'vic40','vic41','vic43','vic45','fear6f','fear6e',
                 'elsdiso18','elsdiso19','vicbar7f')
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
# common questions with country-specific answers
addback <- c('vb3n','vb11','leng1')
unused_gtm <- c(unused_gtm,addback)
unused_slv <- c(unused_slv,addback)
unused_hnd <- c(unused_hnd,addback)

###############################################################################
# We now have a list of unused variables that are common to all three 
# countries, as well as a list of the unique unused variables for each country. 
# Next, we need to categorize these variables as binary, ordered, or unordered 
# categorical.
###############################################################################

# Assume a function is binary if only two responses are present
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
# Function definitions
###############################################################################

ord_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between a variable var1 and a binary variable 
  # var2. This uses a t-test conditioned on the values of var2, and will
  # be applicable if var1 is continuous (i.e. one of our composite indices).
  res = data.frame(var1=character(),var2=character(),est=numeric(),
                   pval=numeric(),stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    tmp$v2 <- tmp$v2 - min(tmp$v2,na.rm=TRUE) # convert to 0-1 scale
    #if (sum(tmp$v2==0,na.rm=TRUE) > 1 & sum(tmp$v2==1,na.rm=TRUE) > 1) {
    if (sum(!is.na(tmp[tmp$v2==0,'v1'])) > 1 & 
        sum(!is.na(tmp[tmp$v2==1,'v1'])) > 1) {
      tt <- t.test(tmp$v1[tmp$v2==0],tmp$v1[tmp$v2==1])
      if (tt$p.value < cutoff) {
        res=data.frame(var1=var1,var2=var2,est=tt$estimate[2]-tt$estimate[1],
                       pval=tt$p.value,stringsAsFactors=FALSE)
      }
    }
  }
  res
}

ord_ord <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between two ordered variables, var1 and var2. 
  # This uses linear regression, and will be applicable when we're dealing
  # with ordered categorical variables or composite indices.
  # As a matter of convention, use the composite index as var1 so that the
  # output makes sense.
  res = data.frame(var1=character(),var2=character(),est=numeric(),
                   pval=numeric(),stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    reg <- lm(v1 ~ v2,data=tmp)
    p <- summary(reg)$coefficients[2,4]
    if (p < cutoff) {
      res=data.frame(var1=var1,var2=var2,est=summary(reg)$coefficients[2,1],
                     pval=p,stringsAsFactors=FALSE)
    }
  }
  res
}

ord_unord <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between an ordered variable var1 and an unordered
  # categorical variable var2. Do this by creating a binary variable for each
  # possible value of var2 and calling ord_bin().
  vals <- na.omit(unique(data[data[,var2] < 800000,var2]))
  if (cutoff == 0) {
    cutoff <- 1 / ncol(data)
  }
  tmp <- data.frame(v1=data[,var1])
  is.na(tmp[tmp>800000]) <- TRUE
  for (x in vals) {
    tmp[,paste(var2,x,sep='_')] <- as.numeric(data[,var2] == x)
  }
  ldply(names(tmp),function(x) ord_bin(tmp,'v1',x,cutoff))
}

bin_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between two binary variables, var1 and var2.
  # Uses a Fisher's exact test. This test reports an odds ratio rather than
  # the coefficient reported by other tests; return the log-odds ratio so 
  # that they're somewhat more comparable.
  res = data.frame(var1=character(),var2=character(),est=numeric(),pval=numeric(),
                   stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    tmp$v1 <- tmp$v1 - min(tmp$v1,na.rm=TRUE) # convert to 0-1 scale
    tmp$v2 <- tmp$v2 - min(tmp$v2,na.rm=TRUE) # convert to 0-1 scale
    if (sum(tmp$v1==0,na.rm=TRUE) > 1 & sum(tmp$v1==1,na.rm=TRUE) > 1 &
        sum(tmp$v2==0,na.rm=TRUE) > 1 & sum(tmp$v2==1,na.rm=TRUE)) {
      t <- table(tmp$v1,tmp$v2)  
      # rows of t are v1, columns are v2
      ft <- fisher.test(t) 
      if (ft$p.value < cutoff) {
        res=data.frame(var1=var1,var2=var2,est=log10(ft$estimate),
                       pval=ft$p.value,stringsAsFactors=FALSE)
      }
    }
  }
  res
}

unord_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between a binary variable var2 and an unordered
  # categorical variable var1. Do this by creating a binary variable for each
  # possible value of var1 and calling bin_bin().
  vals <- na.omit(unique(data[data[,var1] < 800000,var1]))
  if (cutoff == 0) {
    cutoff <- 1 / ncol(data)
  }
  tmp <- data.frame(v2=data[,var2])
  names(tmp) <- var2
  is.na(tmp[tmp>800000]) <- TRUE
  for (x in vals) {
    tmp[,paste(var1,x,sep='_')] <- as.numeric(data[,var1] == x)
  }
  ldply(names(tmp),function(x) bin_bin(tmp,x,var2,cutoff))
}
