#library importing
library(dplyr)
library(factoextra)

#data preparation and cleaning
dataset<-read.csv("https://raw.githubusercontent.com/lutfiahusnakhoirunnisa/bankcustomersegmentation/main/dataclustering.csv", sep=";")
str(dataset)
sum(is.na(dataset)==TRUE)
dataclust<-dataset %>% filter(!is.na(CREDIT_LIMIT))
dataset$CREDIT_LIMIT=as.numeric(dataset$CREDIT_LIMIT)
summary(dataclust)

#wss
fviz_nbclust(dataclust, kmeans, method = "wss")

#clustering with kmeans
modelkm <- kmeans(dataclust, 4)
client_type <- modelkm$cluster
client_type=as.factor(client_type)
aggregate(dataclust,by=list(client_type),FUN=mean)
cluster_km <- data.frame(dataclust, client_type)
head(cluster_km)

#plot
library(ggplot2)
ggplot(cluster_km,aes(x=BALANCE,y=PURCHASES,col=client_type,size=CREDIT_LIMIT))+geom_point()