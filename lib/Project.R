library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("arules")
library(arules)
df <- read.csv("~/Downloads/sample.csv",header=T)
df1 <- read.csv("~/Kaggle/Santender/finalized.csv",header = T)
df2 <- read.csv("~/Kaggle/Santender/itemfinalized.csv",header = T)

df2 <- df2[,-26]
df1 <- df1[,-1]

head(df1)
head(df2)

dim(df1)
dim(df2)

df=df %>% 
  arrange(ncodpers,fecha_dato)
user_list=df%>%distinct(ncodpers)%>%arrange(ncodpers)
head(user_list)
index=sample(df$ncodpers,30)

colnames(df)
demo=df %>%
  group_by(ncodpers)%>%
  filter(ind_actividad_cliente==1,indfall=="N",fecha_dato=="2016-05-28",ncodpers%in%index)%>%
  select(age, fecha_alta, ind_nuevo,tiprel_1mes,renta,segmento,ind_ahor_fin_ult1:ind_recibo_ult1)


demo=data.frame(demo)
info=demo%>%
  select(ncodpers,age,fecha_alta,ind_nuevo,tiprel_1mes,renta,segmento)

demands=demo%>%
  select(ncodpers,ind_cco_fin_ult1,ind_cno_fin_ult1,ind_ctop_fin_ult1,ind_reca_fin_ult1,ind_nomina_ult1)

info
write.csv(info,"~/Fall_2016/EECS6893/info.csv",row.names = F)
demands
write.csv(demands,"~/Fall_2016/EECS6893/demands.csv",row.names = F)



colnames(df)
colSums(df[,27:50])/dim(df[,27:50])[1]

f1 <- read.csv("~/Downloads/f1.csv",header = T)
f2 <- read.csv("~/Downloads/f2.csv",header = T)
head(f1)
head(f2)


total=merge(df1,df2,by="ncodpers")

demands=total[,12:35]
ind=(rowSums(demands)!=0)
total=total[ind,]
total=data.frame(total)
row.names(total) <- total[,1]
total=total[,-1]
head(total)
#total=total[1:10,]
dim(total)



################################# Customized K-means Function  ####################################
K_means <- function(x, centers, distFun, nItter) {
  clusterHistory <- vector(nItter, mode="list")
  centerHistory <- vector(nItter, mode="list")
  
  for(i in 1:nItter) {
    print(i)
    distsToCenters <- distFun(x, centers)
    clusters <- apply(distsToCenters, 1, which.max)
    centers <- apply(x, 2, tapply, clusters, mean)
    # Saving history
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  
  list(clusters=clusterHistory, centers=centerHistory)
}

k=10
centers <- total[sample(nrow(total), k),]
centers <- as.matrix(centers)

similarity <- function(points1, points2,w=0.8) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  info=points1[,1:10]
  demands=points1[,11:34]
  centre1=points2[,1:10]
  centre2=points2[,11:34]
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- w * cor(t(info),centre1[i,]) + (1-w) * cor(t(demands),centre2[i,])
  }
  distanceMatrix
}
iteration=30
res <- K_means(total, centers, similarity, iteration)
res$centers[[30]]
###################################### Generate Datasets with clustered Users ###########################
res[[30]]
#res$clusters[[10]]
#dim(total)
#length(res$clusters[[10]])
ktotal=cbind(total,clusters=res$clusters[[iteration]])
dim(ktotal)
head(ktotal)
colnames(ktotal)
max(ktotal$clusters)
##################################### Train Apriori within each Cluster ###############################

rules=list()
for (i in 1:k){
  temp=ktotal%>%group_by(clusters)%>%filter(clusters==i)
  #dim(temp)
  temp=data.frame(temp)
  temp=temp[,11:34]
  
  #colnames(temp)
  for(b in grep("ind_", colnames(temp), value=TRUE)) temp[[b]] <- as.logical(temp[[b]])
  #head(temp)
  trans <- as(temp, "transactions")
  itemsets <- apriori(trans, parameter = list(target = "rules",
                                              supp=0.001, minlen = 2, maxlen=10))
  #inspect(head(sort(itemsets), n=10))
  quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
  #inspect(head(sort(itemsets, by = "lift"), n=10))
  
  rules[[i]] <- sort(itemsets, decreasing=TRUE, by="confidence")
  print(i)
}
inspect(rules[[1]],n=5)

######################### Generate Prediction ###################################################
prediction <- matrix(NA,nrow=dim(ktotal)[1])
rownames(prediction) <- rownames(ktotal)
pb <- txtProgressBar(min=0,max=dim(ktotal)[1],style = 3)
count_ns=0
for (i in 1:dim(ktotal)[1]){
  temp=ktotal[i,11:34]
  cluster=ktotal[i,35]
  for(b in grep("ind_", colnames(temp), value=TRUE)) temp[[b]] <- as.logical(temp[[b]])
  trans <- as(temp, "transactions")
  rulesMatch <- is.subset(rules[[cluster]]@lhs,trans)
  #applicable <- rules[[cluster]][rulesMatch==TRUE]
  #inspect(applicable[1])
  suitableRules <-  rulesMatch & !(is.subset(rules[[cluster]]@rhs,trans))
  #recommendations <- c()
  if(sum(suitableRules)==0){
    if (temp$ind_cco_fin_ult1==FALSE){
      recommendations="ind_cco_fin_ult1"
    }else if(temp$ind_recibo_ult1==FALSE){
      recommendations="ind_recibo_ult1"
    }else if(temp$ind_ctop_fin_ult1==FALSE){
      recommendations="ind_ctop_fin_ult1"
    }else if(temp$ind_cno_fin_ult1==FALSE){
      recommendations="ind_cno_fin_ult1"
    }else if(temp$ind_ecue_fin_ult1==FALSE){
      recommendations="ind_ecue_fin_ult1"
    }else if(temp$ind_nom_pens_ult1==FALSE){
      recommendations="ind_nom_pens_ult1"
    }else if(temp$ind_nomina_ult1==FALSE){
      recommendations="ind_nomina_ult1"
    }else if(temp$ind_tjcr_fin_ult1==FALSE){
      recommendations="ind_tjcr_fin_ult1"
    }else if(temp$ind_dela_fin_ult1==FALSE){
      recommendations="ind_dela_fin_ult1"
    }else
      recommendations="NA"
    count_ns=count_ns+1
  } else {
    recommendations <- strsplit(LIST(rules[[cluster]][suitableRules]@rhs)[[1]],split=" ")
    recommendations <- lapply(recommendations,function(x){paste(x,collapse=" ")})
    recommendations <- as.character(recommendations)
  }
  prediction[i] <- c(recommendations)
#  if(i%%5000==0) print(i)
  setTxtProgressBar(pb,i)
}
close(pb)
head(prediction)

length(prediction)

write.csv(prediction,"~/Kaggle/Santender/prediction.csv")
write.csv(prediction,"~/Kaggle/Santender/prediction2.csv")
write.csv(prediction,"~/Kaggle/Santender/prediction3.csv")
write.csv(prediction,"~/Kaggle/Santender/prediction4.csv")
prediction=read.csv("~/Kaggle/Santender/prediction3.csv",header=T)
head(prediction)
rownames(prediction)=prediction[,1]
prediction=prediction[,2]
table(prediction)
table(prediction)
count_ns
colSums(ktotal[,11:34])
table(prediction)
rules[[2]]
rules[[3]]
inspect(prediction[1])

