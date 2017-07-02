d <- as.POSIXct("2017-06-28") - as.POSIXct("2017-06-27")
date0 <- seq(as.POSIXct("1990-12-19"),as.POSIXct("2017-06-30"),d)
d1 <- as.POSIXct("1984-11-18")
d2 <- as.POSIXct("1990-11-20")
date <- c(d1,d2,date0)
period0 <- rep(0,length(date))
remark <- rep("",length(date))
stock_peroid <- data.frame(date,period0,remark)
stock_peroid$date <- as.character(stock_peroid$date)

stock_peroid[stock_peroid$date > "1990-12-19" & stock_peroid$date < "1992-06-01",]$period0 <- 1
stock_peroid[stock_peroid$date >= "1992-11-01" & stock_peroid$date < "1993-03-01",]$period0 <- 1
stock_peroid[stock_peroid$date >= "1994-08-01" & stock_peroid$date < "1994-09-01",]$period0 <- 1
stock_peroid[stock_peroid$date >= "1996-01-01" & stock_peroid$date < "1997-05-01",]$period0 <- 1
stock_peroid[stock_peroid$date >= "1999-05-01" & stock_peroid$date < "2001-06-01",]$period0 <- 1
stock_peroid[stock_peroid$date >= "2005-04-13" & stock_peroid$date < "2007-10-17",]$period0 <- 1
stock_peroid[stock_peroid$date > "2008-11-05" & stock_peroid$date < "2009-08-03",]$period0 <- 1
stock_peroid[stock_peroid$date > "2014-11-22" & stock_peroid$date < "2015-06-12",]$period0 <- 1


stock_peroid[stock_peroid$date > "2009-08-04" & stock_peroid$date < "2014-11-21",]$period0 <- 2
stock_peroid[stock_peroid$date > "2016-01-27" & stock_peroid$date < "2016-11-23",]$period0 <- 2

stock_peroid[stock_peroid$date < "1990-12-19",]$period0 <- -1
stock_peroid[stock_peroid$date > "2016-11-23",]$period0 <- -1


library(stringr)
namer2p <- function(name){
  myname <- str_sub(name,2)
  str_replace_all(myname,'[.]',"-")
}

indexcount <- c(1,6,7,8,12,4,12,11,16,14,31,3,2,3)
ind0 <- c(1,(cumsum(indexcount)+1)[-length(indexcount)])
ind1 <- cumsum(indexcount)


