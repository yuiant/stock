library(stringr)
namer2p <- function(name){
  myname <- str_sub(name,2)
  str_replace_all(myname,'[.]',"-")
}

indexcount <- c(1,6,7,8,12,4,12,11,16,14,31,3,2,3)
ind0 <- c(1,(cumsum(indexcount)+1)[-length(indexcount)])
ind1 <- cumsum(indexcount)
inds <- cbind(ind0,ind1)



library(rjson)
namejson <- fromJSON(file = "id2token.json")


##initialize
mc_ind <- read.csv("macro_indicator.csv",fileEncoding = "utf-8")
ridcol <- read.csv("col2rid.csv",header = F)

xa <- rep(1:130,809)
ya <- sort(rep(1:809,130))
hm <- data.frame(xa,ya)
hm$status <- as.integer(is.na(apply(hm,1,FUN = function(x){is.na(mc_ind[810-x[2],2+x[1]])})))
txt <- paste(mc_ind$date[810-hm$ya],unlist(namejson)[hm$xa],sep = ":")
hm$txt <- txt

library(dplyr)
library(tidyr)
mc_ind$date <-as.character(mc_ind$date)
submc <- filter(mc_ind,date >= "1992-01-01" & date <= "2016-11-01")


p <- plot_ly(data = submc,x=~date) %>% 
  add_trace(y = ~X02.00.01) %>%
  add_trace(y=submc$X02.00.00)

n <- dim(submc)[2]-2
cm <- combn(n,2)
a <- apply(cm,2,FUN = function(x){cor(submc[,x[1]+2],submc[,x[2]+2],use = "pairwise.complete.obs")})
myname <- paste(names(submc)[3:132],unlist(namejson),sep = ":")
cname <- apply(cm,2,FUN = function(x){paste(myname[x[1]],myname[x[2]],sep = "#")})
cdf <- data.frame(cor = a, name = cname)

write.table(cdf,"cdf.csv",sep = ",",fileEncoding = "utf-8")


namejson1 <- fromJSON(file = "id2token (copy).json")
indexcount1 <- unlist(lapply(namejson1,FUN = length))
ind0 <- c(1,(cumsum(indexcount)+1)[-length(indexcount)])
ind1 <- cumsum(indexcount)
inds <- cbind(ind0,ind1)


namenest <- function(mydf,rootname){
  colnum <- ncol(mydf)
  root <- list(name = rootname,children = NULL)
}

namenest <- function(root){
  children <- root$children
  if(length(children) > 0){
    sp <- split(children,f = children[,1])
    root$children <- lapply(sp,FUN = function(mysp){
      mychildren <- list(name = unique(mysp[,1]),children = mysp[,-1])
      namenest(mychildren)
      return(mychildren)
    })
  }
 
  return(root)
}








