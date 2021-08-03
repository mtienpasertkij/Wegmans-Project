##CLUSTER ANALYSIS WITH HIERARCHICAL CLUSTERING - PT 1

library(dummies)
library(tidyverse)
library(psych)
library(dendextend)
library(tibble)
library(cluster)
library(animation)
library(tidyverse)
library(factoextra)

DD <- read.csv("C:/users/chrdo/Downloads/Wegmans All Data Full (1).csv")
head(DD)
str(DD)
DD$PPU <- as.numeric(DD$PPU)
sum(is.na(DD$PPU))
DD$PPU[is.na(DD$PPU)] <- 0

hist(DDsub$HOH_AGE)

##remove categorical columns
DD <- subset(DD, select = -c(TRANSACTION_KEY, DATE_KEY, CUSTOMER_ID, ITEM_NBR, INSTACART_IND, ITEM_DESCRIPTION,
                             PRODUCT_HIERARCHY, DEPARTMENT_NAME, CATEGORY_NAME, CLASS_NAME, IS_ORGANIC, 
                             IS_FAMILYPACK, IS_WEGMANSBRAND, HOUSEHOLD_ID))

###HIERARCHICHAL CLUSTERING USING SUBSET OF 10K OBSERVATIONS
DDsub <- DD[sample(nrow(DD),10000),]
summary(DDsub)

##calculate Eulidean distance between customers
dist_customers_DD <- dist(DD, method = "euclidean")

##complete Linkage Analysis - max distance between two sets
hc_customers_DD <- hclust(dist_customers_DD, method = "complete")

##plot dendogram
plot(hc_customers_DD)

##cluster assignemnt at vector h = 200
clust_customers_DD <- cutree(hc_customers_DD, h = 15000)

##segmented customers df
segment_customers_DD <- mutate(DD, cluster = clust_customers_DD)

##analyze characteristics of clusters created
count(segment_customers_DD, cluster)

##color dendogram based on height cutoff
dend_customers_DD <- as.dendrogram(hc_customers_DD)

##calculate mean for each category
segment_customers_DD %>%
  group_by(cluster) %>%
  summarise_all(funs(mean(.)))

##K-MEANS CLUSTERING USING ENTIRE DATASET
DDk <- scale(DD)
head(DDk, n=20)

distance <- get_dist(DDk)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

##k-means clustering with elbow plot to identify clusters to extract
##use mpl_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = DDk, centers = k)
  model$tot.withinss
})

##generate df containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

##plot elbow plot
options(scipen = 10)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

set.seed(123)
km.res <- kmeans(DDk, 6, nstart = 25)
str(km.res)

DDk %>%
  as_tibble() %>%
  mutate(cluster = km.res$cluster,
         state = row.names(DD)) %>%
  ggplot(aes(HH_INCOME, HOH_AGE, color = factor(cluster), label = HOH_AGE)) +
  geom_text()

aggregate(DD, by=list(cluster=km.res$cluster), mean)

DDk1 <- cbind(DD, cluster = km.res$cluster)
head(DDk1)

