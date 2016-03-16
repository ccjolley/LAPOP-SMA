# Find the most common response to question a4 in each department

lapop.2014.HND <- read.csv("../HND-2014.csv",stringsAsFactors=FALSE)
lapop.2014.GTM <- read.csv("../GTM-2014.csv",stringsAsFactors=FALSE)
lapop.2014.SLV <- read.csv("../SLV-2014.csv",stringsAsFactors=FALSE)
source('make_geo.R')

mydata <- data.frame(a4=c(a4=lapop.2014.GTM$a4,lapop.2014.SLV$a4,
                          lapop.2014.HND$a4))
mydata <- cbind(mydata,my_geo[,c('pais_text','prov_text')])
mydata <- mydata[mydata$a4 < 888888,]

mode_stat <- function(x) {
  df <- as.data.frame(table(x))
  as.numeric(df$x[which.max(df$Freq)])
}

mode_pval <- function(x) {
  # return the p-value for the proportion test for the first and 
  # second-most common values within x
  df <- as.data.frame(table(x))
  df <- df[order(df$Freq,decreasing=TRUE),]
  f1 <- df$x[1]
  f2 <- df$x[2]
  m <- matrix(c(sum(x==f1),sum(x==f2),sum(x!=f1),sum(x!=f2)),nrow=2,ncol=2)
  pt <- prop.test(m)
  pt$p.value
}

mode2_stat <- function(x) {
  # return the second-most-common response
  df <- as.data.frame(table(x))
  df <- df[order(df$Freq,decreasing=TRUE),]
  as.numeric(df$x[2])
}

mode2_pval <- function(x) {
  # return the p-value for the proportion test for the first two and 
  # the third-most common values within x
  df <- as.data.frame(table(x))
  df <- df[order(df$Freq,decreasing=TRUE),]
  f1 <- df$x[1]
  f2 <- df$x[2]
  f3 <- df$x[3]
  m <- matrix(c(sum(x==f1 | x==f2),sum(x==f3),sum(x!=f1 & x!= f2),sum(x!=f3)),nrow=2,ncol=2)
  pt <- prop.test(m)
  pt$p.value
}


mydata$uniq_text <- paste(mydata$pais_text,mydata$prov_text,sep='_')
res <- ddply(mydata,'uniq_text',summarize,mode=mode_stat(a4),pval=mode_pval(a4),
      mode2=mode2_stat(a4),pval2=mode2_pval(a4))

# for HND, I have 

m <- mydata[match(res$uniq_text,mydata$uniq_text),c('pais_text','prov_text')]
res2 <- cbind(res[,c('mode','pval','mode2','pval2')],m)
hnd <- res2[res2$pais_text=='Honduras',]
hnd[hnd$pval>0.01,]
hnd[hnd$pval<0.01,]
# more statistical ties than not