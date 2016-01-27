# Generate the geolocation data that will be needed by other files

library(ggmap)
library(plyr)



# No need to load these again, as long as this is called *after*
# make_indices.R
# lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
# lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
# lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
geo <- c('pais','estratopri','estratosec','prov','municipio')
my_geo <- rbind(lapop.2014.GTM[,geo],lapop.2014.SLV[,geo],
                        lapop.2014.HND[,geo])
my_geo[my_geo$pais==4,'hondistrito'] <- lapop.2014.HND$hondistrito
# rm(geo,lapop.2014.GTM,lapop.2014.SLV,lapop.2014.HND)

# my_geo now contains the geo data for all locations surveyed.
# Next, translate the numbers in my_geo into intelligible location names

# Country
pais_num <- c(2,3,4)
pais_text <- c('Guatemala','El Salvador','Honduras')
my_geo$pais_text <- pais_text[match(my_geo$pais,pais_num)]
rm(pais_num,pais_text)

# Department
prov_num <- c(1:18,20:22,301:314,401:413,418:422)                              
prov_text <- c('Guatemala', 'El Progreso', 'Sacatepéquez', 'Chimaltenango', 
               'Escuintla', 'Santa Rosa', 'Sololá', 'Totonicapán', 
               'Quetzaltenango', 'Suchitepéquez', 'Retalhuleu', 'San Marcos', 
               'Huehuetenango', 'Quiché','Baja Verapaz', 'Alta Verapaz', 
               'Petén', 'Izabal', 'Chiquimula', 'Jalapa', 'Jutiapa', 
               'Ahuachapan', 'Santa Ana', 'Sonsonate', 'Chalatenango', 
               'La Libertad','San Salvador', 'Cuscatlan', 'La Paz', 'Cabañas', 
               'San Vicente', 'Usulutan', 'San Miguel', 'Morazan', 'La Union', 
               'Francisco Morazán','Comayagua', 'La Paz', 'Cortés', 
               'Atlántida', 'Colón', 'Yoro', 'Islas de la Bahía', 'Copán', 
               'Intibucá', 'Lempira', 'Ocotepeque', 'Santa Bárbara', 
               'El Paraíso', 'Olancho', 'Gracias a Dios', 'Choluteca', 'Valle')
my_geo$prov_text <- prov_text[match(my_geo$prov,prov_num)]
rm(prov_num,prov_text)

# Municipality
muni_num_hnd <- c(101,108,109,201,204,208,209,301,401,402,403,405,406,407,408,
                  409,501,503,504,505,603,604,701,702,703,704,705,805,902,905,
                  906,907,1101,1105,1203,1307,1308,1309,1310,1405,1801,1806,
                  1807,1902,1908,2001,2101,2107,2108,2201,2205)
muni_num_gtm <- c(101,106,107,108,110,115,116,117,203,301,314,401,413,501,502,
                  504,509,602,609,701,710,806,901,911,919,920,1001,1006,1013,
                  1104,1204,1212,1213,1301,1304,1311,1319,1401,1404,1413,1504,
                  1601,1606,1609,1614,1705,1708,1803,1805,2004,2101,2102,2201,
                  2202,2211)
muni_num_slv <- c(30101,30105,30107,30110,30201,30205,30207,30210,30302,30306,
                  30315,30407,30408,30503,30511,30512,30515,30521,30601,30602,
                  30604,30607,30608,30610,30613,30614,30617,30618,30619,30702,
                  30703,30708,30816,30821,30822,30903,30906,31002,31003,31010,
                  31111,31117,31123,31202,31209,31217,31219,31309,31312,31403,
                  31408,31416)
muni_text_hnd <- c('Distrito Central','El Porvenir','San Ignacio','Comayagua',
                   'Siguatepeque','El Rosario','San Jerónimo','La Paz',
                   'San Pedro Sula','Choloma','Puerto Cortés','San Manuel',
                   'Santa Cruz de Yojoa','Villanueva','La Lima','Potrerillos',
                   'El Porvenir','Tela','La Ceiba','Arizona','Sonaguera',
                   'Tocoa','Yoro','El Negrito','El Progreso','Morazán',
                   'Olanchito','José Santos Guardiola','El Paraíso',
                   'Nueva Arcadia','San José','Intibucá','Candelaria',
                   'San Juan Guarita','Fraternidad','Arada','Ilama',
                   'Quimistán','San Pedro Zacapa ','Santa Rosa de Copán',
                   'Danlí','Yuscarán','Teupasenti','Catacamas','Guayape',
                   'Brus Laguna','Choluteca','Duyure','Pespire','Nacaome',
                   'Caridad')
muni_text_gtm <- c('Guatemala','Chinautla','San Pedro Ayampuc','Mixco',
                   'San Juan Sacatépequez','Villa Nueva','Villa Canales',
                   'Petapa','San Agustín Acasaguastlán','Antigua Guatemala',
                   'Alotenango','Chimaltenango','San Andrés Itzapa',
                   'Escuintla','Santa Lucía Cotzumalguapa','Siquinalá',
                   'San José','Barberena','Taxisco','Sololá','Panajachel',
                   'Santa María Chiquimula','Quetzaltenango',
                   'Concepción Chiquirichapa','El Palmar','Coatepeque',
                   'Mazatenango','Santo Domingo Suchitepéquez','Chicacao',
                   'San Martín Zapotitlán','Comitancillo','Nuevo Progreso',
                   'El Tumbador','Huehuetenango','Cuilco','La Libertad',
                   'Colotenango','Santa Cruz Del Quiché','Zacualpa','Nebaj',
                   'Cubulco','Cobán','Tucurú','San Pedro Carchá','Chahal',
                   'La Libertad','Dolores','El Estor','Los Amates','Jocotán',
                   'Jalapa','San Pedro Pinula','Jutiapa','El Progreso','Comapa')
muni_text_slv <- c('Ahuachapan','El Refugio','Jujutla','San Pedro Puxtla',
                   'Candelaria De La Frontera','El Porvenir','Metapan',
                   'Santa Ana','Armenia','Izalco','Sonsonate','Chalatenango',
                   'Dulce Nombre De Maria','Colon','Santa Tecla',
                   'Quezaltepeque','San Juan Opico','Tepecoyo','Aguilares',
                   'Apopa','Cuscatancingo','Ilopango','Mejicanos','Panchimalco',
                   'San Martin','San Salvador','Soyapango','Tonacatepeque',
                   'Ciudad Delgado','Cojutepeque','El Carmen','San Cristobal',
                   'San Pedro Nonualco','Zacatecoluca','San Luis La Herradura',
                   'Ilobasco','Sensuntepeque','Guadalupe',
                   'San Cayetano Istepeque','San Vicente','Mercedes Umaña',
                   'San Dionisio','Usulutan','Ciudad Barrios','Moncagua',
                   'San Miguel','Sesori','Guatajiagua','Jocoro',
                   'Concepcion De Oriente','La Union','Santa Rosa De Lima')
my_geo$muni_text <- NA
my_geo[my_geo$pais==2,]$muni_text <- muni_text_gtm[match(my_geo[my_geo$pais==2,]$municipio,muni_num_gtm)]
my_geo[my_geo$pais==3,]$muni_text <- muni_text_slv[match(my_geo[my_geo$pais==3,]$municipio,muni_num_slv)]
my_geo[my_geo$pais==4,]$muni_text <- muni_text_hnd[match(my_geo[my_geo$pais==4,]$municipio,muni_num_hnd)]
rm(muni_num_gtm,muni_num_slv,muni_num_hnd,muni_text_gtm,muni_text_slv,muni_text_hnd)

# For Honduras, we'll use aldeas-level information, since we have it
dist_num <- c(10201,10210,10211,10701,10750,10757,10801,10802,10803,10805,20801,20816,20830,
              20901,20906,20915,20917,30101,30134,30301,30304,30306,30311,31301,31308,31312,
              31801,31808,40101,40120,40901,40911,40922,41301,41304,41701,41702,41703,41706,
              50101,50108,50201,50207,50234,50501,50505,50601,50612,50638,50802,50807,50901,
              50905,50907,51001,51026,51101,51104,51107,51116,51201,51204,60101,60109,60401,
              60405,60406,61101,61104,61109,70101,70104,70110,70117,70301,70319,70333,71501,
              71526,71539,80101,80104,80501,80504,80505,81901,81905,90201,90202,90204,90212,
              100601,100606,100611,110301,110304,110307,120101,120109,130301,130303,130305,
              131801,131802,131803,131804,140501,140502,140504,150301,150306,151103,151107,
              151110,151112,160201,160203,160207,161102,161106,161110,161112,161701,161706,
              161725,161731,162301,162303,162305,162307,170101,170102,170105,170107,170501,
              170503,170504,180101,180106,180112,180118,180301,180315,180326,180327,180401,
              180403,180454,180601,180602,180608,180615,180701,180716,180746,180752)
dist_text <- c('El Porvenir','La Unión','La Ceiba','Tela','Mezapa o Santa Rosa del Norte','Paujiles',
               'Arizona','Mezapita','Atenas de San Cristobal','Colonia Río Plátano','Sonaguera',
               'Flores del Este','Los Carrioles','Tocoa','El Guapinol','Quebrada de Arena','Taujica',
               'Comayagua','Río Blanco o San Jose','El Rosario','Concepción de Guasistagua','La Laguna',
               'San Francisco de Loma Larga','San Jerónimo','Las Arenas','Ocotes Caídos','Siguatepeque',
               'El Pacayal','Santa Rosa de Copán','Quezailica','El Paraíso','La Laguna','Río Lindo','La Entrada',
               'Chalmeca','San José','Buena Vista','El Porvenir','Vivistorio','San Pedro Sula','Cofradía','Choloma',
               'El Higuero','Río Bijao','Potrerillos','El Manacal','Puerto Cortés','El Bálsamo','Travesía','Cañaveral', 
               'Río Lindo','San Manuel','Col. Pineda No.1','El Plan','Santa Cruz de Yojoa','La Jutosa','Villanueva',
               'Campo Dos Caminos','El Milagro','Palos Blancos','La Lima','Flor de Oriente','Choluteca','El Pillado',
               'Duyure','Liraqui','Tierra Colorada','Pespire','El Espinal','San Juan Bautista','Yuscarán',
               'El Chagüite de Oriente','El Rodeo','Ojo de Agua','Danlí','El Tablón','San Diego','Teupasenti',
               'Las Flores','Santa Rosa No.2','Distrito Central','Carpintero','El Porvenir','El Guantillo',
               'El Pedernal','San Ignacio','San Miguel de Barrosa','Brus Laguna','Barra Patuca','Belén',
               'Río Plátano','Intibucá','La Sorto','Pueblo Viejo','José Santos Guardiola','Jonesville Point',
               'Punta Gorda','La Paz','Yarumela','Candelaria','San Francisco','San Lorenzo','El Rodeo',
               'Los Horcones','Pueblo Viejo','San Juan Guarita','Fraternidad','Las Cruces','San Francisco',
               'Catacamas','La Bodega','El Coyol','El Rodeo','Monte Grande','Santa Cruz de Guayape','Arada',
               'Candelaria','El Palmo','Agua Zarca','La Estancia','San José de Oriente',
               'San Vicente de Las Nieves','Quimistán','Correderos','Pinalejo','Santa Cruz Minas',
               'San Pedro Zacapa','Azacualpa','El Mogote','Horconcitos','Nacaome','Agua Caliente','El Tular',
               'Moropocay','Caridad','La Esperanza','Las Delicias','Yoro','La Guata','Locomapa No.1','Subirana',
               'El Negrito','Finca Treinta y Seis','Toyos','Villa del Carmen o Finca Trein','El Progreso',
               'Agua Blanca Sur','Urraco Pueblo','Morazán','Portillo De González','El Filón del Porvenir',
               'Mojimán','Olanchito','Campo Nerones','Las Hicoteas','Nombre de Jesús')
my_geo$dist_text <- dist_text[match(my_geo$hondistrito,dist_num)]
rm(dist_num,dist_text)

# Add lat/long information to my_geo. For Honduras, we'll extract that from the USGS aldeas data
aldeas <- read.delim("../adelas.txt") # not my typo!
ald2 <- aldeas[,c('Codigo','Nombre','Latitude','Longitude')]
geo2 <- my_geo[,c('hondistrito','dist_text')]
names(ald2) <- c('hondistrito','Nombre','Latitude','Longitude')
joined <- join(geo2,ald2,type="left")
my_geo$lat <- joined$Latitude
my_geo$long <- joined$Longitude
rm(aldeas,ald2,geo2,joined)

# Get locations in Guatemala and El Salvador from the Google API
my_geo$locstr <- paste(my_geo$muni_text,my_geo$prov_text,my_geo$pais_text,
                       sep=", ")
uniq_loc <- unique(my_geo[my_geo$pais<4,'locstr'])
latlong <- ldply(uniq_loc,function(x) geocode(x,source="google",output="latlon"))
latlong$locstr <- uniq_loc
names(latlong) <- c('long2','lat2','locstr')
joined2 <- join(my_geo,latlong,by="locstr")
my_geo[is.na(my_geo$lat),'lat'] <- joined2[is.na(my_geo$lat),'lat2']
my_geo[is.na(my_geo$long),'long'] <- joined2[is.na(my_geo$long),'long2']
rm(uniq_loc,latlong,joined2)

# Clean up my_geo to contain only the useful columns
my_geo <- my_geo[,7:12]
