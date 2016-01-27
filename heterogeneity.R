# Explore spatial heterogeneity

library(ggplot2)
library(plyr)
library(ggmap)

source('make_geo.R',encoding='utf-8')
source('make_indices.R')

###############################################################################
# Lat/long info for Honduras won't be quite right -- my_geo contains aldeas
# locations, and I want municipalities.
###############################################################################

tmp <- my_geo
tmp$locstr <- paste(tmp$muni_text,tmp$prov_text,tmp$pais_text,
                       sep=", ")
uniq_loc <- unique(tmp[tmp$pais_text=='Honduras','locstr'])
latlong <- ldply(uniq_loc,function(x) geocode(x,source="google",output="latlon"))
latlong$locstr <- uniq_loc
names(latlong) <- c('long2','lat2','locstr')
j <- join(tmp,latlong,by="locstr")
my_geo[my_geo$pais_text=='Honduras','lat'] <- j[j$pais_text=='Honduras','lat2']
my_geo[my_geo$pais_text=='Honduras','long'] <- j[j$pais_text=='Honduras','long2']

###############################################################################
# For any variable, calculate the fraction of its variance that can be 
# attributed to geography at the municipal level.
###############################################################################
geo_variance <- function(data,x,make_plot=FALSE,xlab='Value') {
  tmp <- my_geo
  tmp$muni_uniq <- paste(tmp$pais_text,tmp$muni_text,sep='_')  
  tmp$x <- x
  tmp$x[tmp$x > 800000] <- NA
  muni_avg <- ddply(tmp,~muni_uniq,summarize,x=mean(x,na.rm=TRUE))  
  tmp$muni_avg <- muni_avg$x[match(tmp$muni_uniq,muni_avg$muni_uniq)] 
  my_lm <- lm(data=tmp,x ~ muni_avg)
  if (make_plot==TRUE) {
    tmp <- na.omit(tmp)
    p <- ggplot(tmp,aes(x=x,y=muni_avg)) +
      geom_point(color='tomato',alpha=0.2,size=5) +
       geom_smooth(method='lm',size=2,color='royalblue') +
       annotate('text',label=paste("R-sq:",round(summary(my_lm)$r.squared,2)), 
                size=10,x=min(tmp$x) + 0.1*(max(tmp$x)-min(tmp$x)),
                y=0.9*max(tmp$muni_avg),hjust=0,vjust=0,color='royalblue') +
       theme_classic() +
       ylab('Muni-level average') +
       xlab(xlab) +
       theme(text=element_text(size=20))
    print(p)
  }
  summary(my_lm)$r.squared
}

geo_variance(lapop.2014.all,fear_all,make_plot=T,xlab="Fear index")
geo_variance(lapop.2014.all,lapop.2014.all$d6,make_plot=T,xlab="Approval for gay marriage")

###############################################################################
# Calculate geo_variance for all region-wide variables and plot density
###############################################################################

# Ignore these irrelevant variables 
drop <- c("formatq","sexi","colori","uniq_id","pais","idnum","estratopri",
          "estratosec","upm","prov","municipio","cluster","tamano","idiomaq",
          "fecha","wt","ti",'sex')
# Also remove unordered categorical variables
unord_common <- c('idiomaq','a4','vic2','vic2aa','aoj22','env1','vb1',
                  'vb4new','vb101','vb20','for1n','for4',
                  'for5','q3c','ocup4a','ocup1a','q11n','etid','vb11','leng1',
                  'vb3n')

plot_us <- common[!common %in% c(drop,unord_common)]
d <- lapop.2014.all[,plot_us]
d$fear_idx <- fear_all
d$ca_idx <- ca_all
d$tr_idx <- tr_all
d$w_idx <- w_all
d$crit_idx <- crit_all
d$aut_idx <- aut_all
# TODO: Eventually I want to systematically include dummy variables; for now
# I'll just manually create a couple that I need
d$q3c_5 <- as.numeric(lapop.2014.all$q3c==5)
d$ocup4a_3 <- as.numeric(lapop.2014.all$ocup4a==3)
geo_var <- ldply(names(d),function(x) geo_variance(d,d[,x]))
geo_var$name <- names(d)

head(geo_var[order(geo_var$V1,decreasing=TRUE),])

ggplot(data=geo_var,aes(x=V1)) +
  geom_density(fill='aquamarine3',adjust=0.25) +
  theme_classic() +
  geom_vline(xintercept=median(geo_var$V1),color='khaki2',size=2) +
  xlab('Geographic fraction of variance') +
  theme(axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.line=element_blank(),
        axis.title.y=element_blank(),
        text=element_text(size=20))

###############################################################################
# For a given variable, get a list of locations for which its value is
# significantly different from the average
###############################################################################
sig_locs <- function(data,var,direction,cutoff=0.01) {
  # direction should be 'two.sided', 'less', or 'greater'
  tmp <- my_geo
  tmp$muni_uniq <- paste(tmp$pais_text,tmp$muni_text,sep='_')  
  tmp$x <- var
  tmp$x[tmp$x > 800000] <- NA
  res <- data.frame()
  for (m in unique(tmp$muni_uniq)) {
    tt <- t.test(tmp[tmp$muni_uniq==m,'x'],tmp[tmp$muni_uniq!=m,'x'],
                 alternative=direction)
    if (tt$p.value < cutoff) {
      res <- rbind(res,data.frame(muni_uniq=m,
                                  est=tt$estimate[1]-tt$estimate[2],
                                  pval=tt$p.value))
    }
  }
  j <- join(res,tmp[,c(1,3,5,6,7)],by='muni_uniq',match='first')
  rownames(j) <- NULL
  j <- j[order(j$pval),]
  j[,c(4,5,6,7,2,3)]
}

###############################################################################
# Get these outlier locations for the maps in my presentation
###############################################################################
setwd("C:/Users/Craig/Dropbox/SMA/VSFS/Presentation")
write.csv(sig_locs(d,d$fear_idx,'greater'),
          'fear-outliers.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$ur,'less'),
          'urban-outliers.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$it1,'greater'),
          'it-outliers.csv',fileEncoding='utf-8')


sig_locs(d,d$d6,'greater') # Just Distrito Central, Honduras


write.csv(sig_locs(d,d$aut_idx,'greater'),
          'aut-outliers.csv',fileEncoding='utf-8')

write.csv(sig_locs(d,d$ed2,'greater'),
          'ed2-outliers.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$pole2n,'greater'),
          'pole2n-outliers.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$ca_idx,'greater'),
          'ca_idx_high.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$ca_idx,'less'),
          'ca_idx_low.csv',fileEncoding='utf-8')

write.csv(sig_locs(d,d$pol1,'less'),
          'pol1_high.csv',fileEncoding='utf-8')
write.csv(sig_locs(d,d$pol1,'greater'),
          'pol1_low.csv',fileEncoding='utf-8')

geo_variance(d,d$tr_idx)

write.csv(sig_locs(d,d$tr_idx,'less'),
          'trust_outliers.csv',fileEncoding='utf-8')

write.csv(sig_locs(d,d$q3c_5,'greater'),
          'evangel_outliers.csv',fileEncoding='utf-8')

sig_locs(d,d$ocup4a_3,'greater') # returns nothing (needs a more graceful error msg)
