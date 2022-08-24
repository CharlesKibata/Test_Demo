# cleaning the memrory
rm(list = ls())
#
#setting working directory
setwd("C:/Users/user/Desktop/canny brain/Mee/August/G124691 V1")
# libraries
library(tidyverse)
library(ggplot2)
library(MASS)
library(forecast)
library(reshape2)
library(reshape)
library(lubridate)

# loading the data
users<-read.csv("users.csv")
server<-read.csv("server.csv")
Subscriber<-read.csv("Subscriber.csv")

# melting the data
userM<-melt(users, id = c("Country.Name"))
serverM<-melt(server, id = c("Country.Name"))
SubscriberM<-melt(Subscriber,id = c("Country.Name"))

# renaminin the column names
colnames(userM)<-c("country","year","users")
colnames(serverM)<-c("country","year","servers")
colnames(SubscriberM)<-c("country","year","subscribers")

# merging the two data
data<-merge(x = userM, y = c(serverM,SubscriberM), by = c("country","year"),
      all.x = TRUE)
data<-data[,c(1:4,7)]
# removing X in the column year
data$year = as.numeric(gsub("\\X", "", data$year))
# replacing na with 0
data[is.na(data)] <- 0
# selecting the ten countries

data<- data[data$country %in% c("British Virgin Islands","Denmark","Seychelles",
                                "Netherlands","Singapore", "United States",
                                "North America","Switzerland","Ireland","Iceland"), ]   



# Summary of the data
summary(data)
# top ten countries in the  user, subscribers, and servers
df<-data %>% group_by(country) %>%
  summarise_at(vars(users),list(sum=mean))%>%
  arrange(desc(sum))

ggplot(data = head(df,10),aes(x=reorder(country,-sum),y=sum))+
  geom_bar(stat = 'identity')+xlab("Countries ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab(" Total percentage of internet users")

df<-data %>% group_by(country) %>%
  summarise_at(vars(servers),list(sum=mean))%>%
  arrange(desc(sum))

ggplot(data = head(df,10),aes(x=reorder(country,-sum),y=sum))+
  geom_bar(stat = 'identity')+xlab("Countries ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab(" Secure internet servers")
df

df<-data %>% group_by(country) %>%
  summarise_at(vars(subscribers),list(sum=mean))%>%
  arrange(desc(sum))

ggplot(data = head(df,10),aes(x=reorder(country,-sum),y=sum))+
  geom_bar(stat = 'identity')+xlab("Countries ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab(" Mobile Cellular subscribers")
df
# scatterplot
# Subscriber
par(mfrow=c(2,2))
df<-data %>% group_by(year) %>%
  summarise_at(vars(subscribers),list(sum=mean))%>%
  arrange(desc(sum))
plot(df$year,df$sum,xlab = "Year",ylab = "Average mobile cellular Subscribers")

# users
df<-data %>% group_by(year) %>%
  summarise_at(vars(users),list(sum=mean))%>%
  arrange(desc(sum))
plot(df$year,df$sum,xlab = "Year",ylab = "% of internet users")

# Servers
df<-data %>% group_by(year) %>%
  summarise_at(vars(servers),list(sum=mean))%>%
  arrange(desc(sum))
plot(df$year,df$sum,xlab = "Year",ylab = "Secure Internet servers")
dev.off()

  

# Kmean clustering

# Scalling the data
df<-scale(data[,-c(1:2)])
library(factoextra)
library(cluster)

# determining the optimal number of clusters
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# four
# Kmeans
kmean<- kmeans(df,centers = 3,nstart = 20)

kmean

# plot of the kmean
fviz_cluster(kmean,data = df)

#find means of each cluster
aggregate(data[,-c(1:2)], by=list(cluster=kmean$cluster), mean)


# predictive model
fit<-lm(users~subscribers+servers,data = data)
#summary
summary(fit)


# model diagonistics
par(mfrow=c(2,2))
plot(fit,which = 1:4)
