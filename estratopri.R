# The people at LAPOP tell me that their data is only really 
# representative at the estratopri level -- to map this effectively,
# we need to correlate this with administrative units.

setwd("C:/Users/Craig/Desktop/SMA/VSFS/LAPOP-SMA")
source('make_geo.R',encoding='utf-8')

res <- unique(my_geo[,c('pais_text','prov_text','estratopri')])
names(res) <- c('country','admin1','estratopri')
# now join to shapefile to get adm1 codes

setwd('C:/Users/Craig/Desktop/UAC/')
source('NT-isocodes.R')
setwd("C:/Users/Craig/Desktop/SMA/VSFS/LAPOP-SMA")

j <- join(res,shp_clean[,c('country','admin1','adm1_code')],by=c('country','admin1'))
j[j$admin1=='Islas de la Bahia','adm1_code'] <- shp_clean[shp_clean$admin1=='Bay Islands','adm1_code']

# j has 57 rows; shp_clean has 55 -- what's getting duplicated?
# table(j$adm1_code) -- GTM-1945 and GTM-1947

j[j$adm1_code=='GTM-1945',] # Quetzaltenango, GTM
j[j$adm1_code=='GTM-1947',] # San Marcos, GTM

# Nailing down a boundary at the sub-admin1 level will be tricky, since
# municipalities weren't comprehensively sampled. Where were most?

table(my_geo[my_geo$prov_text=='Quetzaltenango','estratopri']) # almost 50-50
table(my_geo[my_geo$prov_text=='San Marcos','estratopri']) # 2/3 in 203, 1/3 in 206

unique(my_geo[my_geo$prov_text=='Quetzaltenango',c('estratopri','muni_text')])
# only 4 sites
unique(my_geo[my_geo$prov_text=='San Marcos',c('estratopri','muni_text')])

# I made a guess at a dividing line that cut across these two departments and 
# put the municipalities above on either side of the line.

j[j$adm1_code=='GTM-1945','estratopri'] <- NA
j[j$adm1_code=='GTM-1947','estratopri'] <- NA
j <- unique(j) # Now just 55 rows
setwd("C:/Users/Craig/Documents/ArcGIS/LAPOP/2016-03-15/")
write.csv(j,'estratopri-adm1.csv')

# In addition, Zacapa, Guatemala doesn't seem to have been sampled and can't be assigned.
# Almost all of the areas around Zacapa are in estratopri 205; put it there as well.
