rm(list=ls())

#set the working directory
setwd("E:/Data Science_ Edwisor/Credit Card Segmentation")

#check the path
getwd()

#load the data
ccdata = read.csv(file='credit-card-data.csv')

#now we will view the summary of the data loaded
summary(ccdata)

head(ccdata,5)

#we will view if there is any missing values
library(Amelia)
missmap(ccdata, main = "Missing value map")

#we will now replace missing values with median
ccdata$CREDIT_LIMIT[is.na(ccdata$CREDIT_LIMIT)] <- median(ccdata$CREDIT_LIMIT, na.rm= T)
ccdata$MINIMUM_PAYMENTS[is.na(ccdata$MINIMUM_PAYMENTS)] <- median(ccdata$MINIMUM_PAYMENTS, na.rm = T)

sum(is.na(ccdata))

#correlation
str(ccdata)

#we can drop CUST_ID from our dataset as it is not a useful variable
drop <- c("CUST_ID")
ccdata = ccdata[,!(names(ccdata) %in% drop)]

dim(ccdata)

# Rename variables for correlogram plot
library(tidyverse)
ccdata <- ccdata %>% rename(bal = BALANCE,
                            bal_freq = BALANCE_FREQUENCY,
                            pur = PURCHASES,
                            oo_pur = ONEOFF_PURCHASES,
                            inst_pur = INSTALLMENTS_PURCHASES,
                            cadv = CASH_ADVANCE,
                            pur_freq = PURCHASES_FREQUENCY,
                            oo_pur_freq = ONEOFF_PURCHASES_FREQUENCY,
                            pur_inst_freq = PURCHASES_INSTALLMENTS_FREQUENCY,
                            cadv_freq = CASH_ADVANCE_FREQUENCY,
                            cadv_trx = CASH_ADVANCE_TRX,
                            pur_trx = PURCHASES_TRX,
                            c_lim = CREDIT_LIMIT,
                            pay = PAYMENTS,
                            min_pay = MINIMUM_PAYMENTS,
                            prc_full = PRC_FULL_PAYMENT,
                            ten = TENURE)

# Correlation plot
correlation_matrix <- cor(ccdata)

# install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix
         , method = 'color'
         , type = 'upper'
         , number.cex = .5
         , addCoef.col = "white"
         , tl.col = "black")


# we will use "elbow method" to find the optimal number of clusters
# Gives us 8 clusters being optimal
set.seed(123)
WCSS <- vector()
for (i in 1:20) WCSS[i] <- sum(kmeans(ccdata, i)$withins)
plot(1:20, WCSS, type = "b", main = "Elbow Method", xlab = "Clusters", ylab = "WCSS")


# Standardise
ccdata = scale(ccdata)

kmeans <- kmeans(ccdata, 5, iter.max = 300, nstart = 10)
ccdata <- data.frame(ccdata,kmeans$cluster)
ccdata %>% group_by(ccdata$kmeans.cluster) %>% summarise(n())

# Interpret results - grouped boxplot
ccdata %>% ggplot(aes(y=bal, x=kmeans.cluster, fill=factor(kmeans.cluster))) + geom_boxplot() +
  labs(fill = "Clusters", title = "Box plot by Cluster for Balance") + xlab("Cluster")

ccdata %>% ggplot(aes(y=pur, x=kmeans.cluster, fill=factor(kmeans.cluster))) + geom_boxplot() +
  labs(fill = "Clusters", title = "Box plot by Cluster for Purchases") + xlab("Cluster")

ccdata %>% ggplot(aes(y=pur_freq, x=kmeans.cluster, fill=factor(kmeans.cluster))) + geom_boxplot() +
  labs(fill = "Clusters", title = "Box plot by Cluster for Purchase Frequency") + xlab("Cluster")

ccdata %>% ggplot(aes(y=cadv, x=kmeans.cluster, fill=factor(kmeans.cluster))) + geom_boxplot() +
  labs(fill = "Clusters", title = "Box plot by Cluster for Cash Advance Amount") + xlab("Cluster")

ccdata %>% ggplot(aes(y=c_lim, x=kmeans.cluster, fill=factor(kmeans.cluster))) + geom_boxplot() +
  labs(fill = "Clusters", title = "Box plot by Cluster for Credit Limit") + xlab("Cluster")


