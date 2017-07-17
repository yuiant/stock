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
  if(!is.null(children)){
    sp <- split(children,f = children[,1])
    names(sp) <- NULL
    root$children <- lapply(sp,FUN = function(mysp){
      newchild <- NULL
      if(length(mysp) > 1){
        newchild <- data.frame(mysp[,-1],stringsAsFactors = F)
      }
      mychildren <- list(name = unique(mysp[,1]),children = newchild)
      namenest(mychildren)
    })
  }
  return(root)
}




notlagdata <- submc0[,c(1,2,notlagIndex+2)]

lagsubdata <- submc0[,lagsubdata+2]
lagsubdata <- lagsubdata[-nrow(lagsubdata),]
lagsubdata <- rbind(rep())

alldatawithlag

pcadata <- notlagdata[,-c(1:2)]
cormat <- cor(pcadata,use = "pairwise.complete.obs")
e <- eigen(cormat)
evalue <- e$values
evalue[which(evalue < 0)] <- min(evalue[which(evalue >= 0)])
e$values <- evalue

spercent <- 99
varcumsum <- cumsum(e$values)/sum(e$values)
minpcax <- min(which(varcumsum >= spercent/100,arr.ind = T))

dc <- ncol(pcadata)
dr <- nrow(pcadata)
pm <- c(as.matrix(pcadata))
pm[which(is.na(pm))] <- -0.0001
pcadata1 <- matrix(pm,nrow = dr,ncol = dc)

newpcadata <- pcadata1 %*% e$vectors[,1:minpcax]
newsubmc <- data.frame(cbind(notlagdata[,2],newpcadata))
names(newsubmc) <- c("p",paste("Com",1:minpcax,sep = ""))

library(C50)
library(caret)








newsubmc1 <- notlagdata[,-1]
names(newsubmc1)[1] <- "p"
newsubmc1$p[which(newsubmc1$p == 2)] <- 0
newsubmc1$p <- as.factor(newsubmc1$p)

a <- lapply(1:100,FUN = function(xxx){
  set.seed(round(runif(n = 1,min = 1,max = 10000)))
  folds <- createFolds(y = newsubmc1$p,k = 10)
  res <- lapply(folds,FUN = function(fd){
    trainset <- newsubmc1[-fd,]
    
    testset <- newsubmc1[fd,]
    
    model <- C5.0(x = trainset[,-1],y = trainset[,1])
    pred <- predict(model,newdata = testset[,-1])
    predtt <- table(pred,testset$p)
    accuracy <- (predtt[1,1]+predtt[2,2])/(sum(predtt))
  })
  res <- unlist(res)
})

folds <- createFolds(y = newsubmc1$p,k = 10)
res <- lapply(folds,FUN = function(fd){
  trainset <- newsubmc1[-fd,]
  
  testset <- newsubmc1[fd,]
  
  model <- C5.0(x = trainset[,-1],y = trainset[,1])
  pred <- predict(model,newdata = testset[,-1])
  predtt <- table(pred,testset$p)
  accuracy <- (predtt[1,1]+predtt[2,2])/(sum(predtt))
})








