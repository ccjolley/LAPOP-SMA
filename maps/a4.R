# Find the most common response to question a4 in each primary sampling unit

setwd("C:/Users/Craig/Desktop/SMA/VSFS/LAPOP-SMA/maps")
source('../utils/make_geo.R',encoding='utf-8')

mydata <- data.frame(a4=c(a4=lapop.2014.GTM$a4,lapop.2014.SLV$a4,
                          lapop.2014.HND$a4))
mydata <- cbind(mydata,my_geo[,c('estratopri')])
mydata <- mydata[mydata$a4 < 888888,]
names(mydata) <- c('a4','estratopri')

mode_stat <- function(x) {
  df <- as.data.frame(table(x))
  as.numeric(df$x[which.max(df$Freq)])
}

res <- ddply(mydata,'estratopri',summarize,mode=mode_stat(a4))
# modal response is always poverty (4) or crime (5)
res[res$mode==4,'text'] <- 'Poverty'
res[res$mode==5,'text'] <- 'Crime'

# ArcMap only wants to join on text fields
res$estratopri <- as.character(res$estratopri) 
write.csv(res,'a4.csv',row.names=FALSE)


