# Generate a couple of Excel spreadsheets with all the fear-related variables we could hope to map.

source('make_indices.R')
source('make_geo.R')
library(plyr)
library(XLConnect)
library(ggmap)

# These variables were found (by stepwise regression) to explain the fear index
# well; we'll want to map them.

map_us <- lapop.2014.all[,c('clien1n','d6','ed2','for6','it1','mil10c','pol1',
                         'pole2n','sd3new2','ur','w14a','www1')]
categ <- lapop.2014.all[,c('a4','aoj22','for4','for1n','for5','ocup4a','q3c','vb20')]
categ[categ>800000] <- NA
map_us$a4_5     <- as.numeric(categ$a4==5)
map_us$aoj22_1  <- as.numeric(categ$aoj22==1)
map_us$for4_4   <- as.numeric(categ$for4==4)
map_us$for1n_7  <- as.numeric(categ$for1n==7)
map_us$for5_6   <- as.numeric(categ$for5==6)
map_us$ocup4a_3 <- as.numeric(categ$ocup4a==3)
map_us$q3c_5    <- as.numeric(categ$q3c==5)
map_us$vb20_4   <- as.numeric(categ$vb20==4)
map_us$aut_idx <- aut_all
map_us$ca_idx <- ca_all
map_us$tr_idx <- tr_all
map_us$fear_idx <- fear_all
map_us[map_us>800000] <- NA

# For mapping purposes, we'll fill in the missing values with MICE, then avg
imp <- mice(map_us)
all.imp <- llply(1:5,function(i) complete(imp,i))
map_avg <- aaply(laply(all.imp, as.matrix), c(2, 3), mean)

# I'm going to use map_us rather than map_avg so that imputed values aren't 
# included in the department averages.
my_geo <- cbind(my_geo,map_us)
out_geo <- ddply(my_geo,.(prov_text),numcolwise(function(x) mean(x,na.rm=TRUE)))
labels <- my_geo[,c('pais_text','prov_text')]
labels <- labels[match(out_geo$prov_text,labels$prov_text),c('pais_text','prov_text')]
out_geo <- cbind(labels,out_geo[,-1:-3])

# add counts for # of survey responses in each location. This is a little 
# sketchy, since not all locations will have the same number of missing
# values for each variable -- I'll have to give a little more attention to
# the few variables with lots of missing values.
str_all <- paste(my_geo$pain_text,my_geo$prov_text,sep='_')
str_table <- table(str_all)
str_uniq <- paste(out_geo$country,out_geo$prov,sep='_')
out_geo$count <- str_table[str_uniq]

out_geo <- out_geo[order(out_geo$pais_text,out_geo$prov_text),]
write.csv(out_geo,file='mapus-prov.csv')

# Output two different versions, one with imputation and one without. See how different
# the maps look at the end (and how well ArcGIS can deal with missing values)
out_geo2 <- data.frame(country=my_geo$pais_text,
                       prov=my_geo$prov_text,
                       muni=my_geo$muni_text,
                       aldeas=my_geo$dist_text,
                       lat=my_geo$lat,
                       long=my_geo$long,
                       stringsAsFactors = FALSE)
out_geo2_na <- cbind(out_geo2,my_geo[,-1:-6])
out_geo2_imp <- cbind(out_geo2,map_avg)

write.csv(out_geo2_na,'mapus-ll-na.csv')
write.csv(out_geo2_imp,'mapus-ll-imp.csv')
