library(stringr)
namer2p <- function(name){
  myname <- str_sub(name,2)
  str_replace_all(myname,'[.]',"-")
}

indexcount <- c(1,6,7,8,12,4,12,11,16,14,31,3,2,3)
ind0 <- c(1,(cumsum(indexcount)+1)[-length(indexcount)])
ind1 <- cumsum(indexcount)


library(rjson)
namejson <- fromJSON(file = "id2token.json")


##initialize
mc_ind <- read.csv("macro_indicator.csv",fileEncoding = "utf-8")
col2rid <- read.csv("col2rid.csv",header = F)

xa <- rep(1:130,809)
ya <- sort(rep(1:809,130))
hm <- data.frame(xa,ya)
txt <- paste(mc_ind$date[810-hm$ya],unlist(namejson)[hm$xa],sep = ":")


library(dplyr)
library(tidyr)
mc_ind$date <-as.character(mc_ind$date)
submc <- filter(mc_ind,date >= "19992,01,1" & date <= "2016-11-01")







