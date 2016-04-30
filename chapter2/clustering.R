library(dplyr)
library(cluster)
library(flexclust)

setwd("/home/vitidn/mydata/repo_git/DataSmart/chapter2/")

num_cluster = 5
#select either kmeans or kmedians_cosine(the book suggests that kmedians_cosine is better)
model_use = "kmedians_cosine"

offer_info = read.csv("OfferInformation.csv")
#exclude "." from the column names
col_names = colnames(offer_info)
col_names = sapply(col_names,function(x) gsub("\\.","",x))
col_names = as.character(col_names)
colnames(offer_info) = col_names
#re-order months
offer_info$Campaign@levels = c("January","February","March","April","May","June","July","August","September","October","November","December")

transaction = read.csv("Transactions.csv")
col_names = colnames(transaction)
col_names = sapply(col_names,function(x) gsub("\\.","",x))
col_names = as.character(col_names)
colnames(transaction) = col_names

#view a number of transactions each user bought
by_name = group_by(transaction,CustomerLastName)
report = summarise(by_name,count=n())
#re-order from number of transactions
report = arrange(report,desc(count))
head(report)
tail(report)
summary(report)
#take a look at the dimension(number of distinct users)
dim(report)

#for each customer, construct a vector with size 32, has "1" on the element of the offer that user purchased
customer_names = arrange(report,CustomerLastName)$CustomerLastName
customer_data = data.frame(customer_name=customer_names)
for(i in 1:32){
  customer_data[[paste("offer",i,sep = "_")]] = 0
}

#filling a row for each user, base on their purchased orders
for(i in 1:dim(customer_data)[1]){
  name = customer_data[i,1]
  offer_ids = filter(transaction,CustomerLastName == name)$Offer
  for(offer_id in offer_ids){
    customer_data[i,(offer_id+1)] = 1
  }
}

if(model_use == "kmeans"){
  k_model = kcca(select(customer_data,offer_1:offer_32),k = num_cluster,family = kccaFamily("kmeans"), control = list(initcent="kmeanspp"))
}else
{
  #for k-medians model with cosine-distance
  #got the original function from kccaFamily("kmeans")
  cosine_dist_function = function(x,centers){
    if (ncol(x) != ncol(centers)) 
      stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
    z <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
    vector_lengths = sqrt(rowSums(x^2))
    for (k in 1:nrow(centers)) {
      dot_product = colSums(t(x) * centers[k,])
      centroid_length = sqrt(sum(centers[k, ]^2))
      cosine_sim = dot_product / (centroid_length * vector_lengths)
      cosine_distances = 1.0 - cosine_sim
      z[, k] <- replace(cosine_distances, is.nan(cosine_distances),1)
    }
    z
  }
  k_model = kcca(select(customer_data,offer_1:offer_32),k = num_cluster,family =kccaFamily(dist = cosine_dist_function,cent = function(x){apply(x, 2, median)}) )
}
#assign users to their corresponding cluster
customer_data[["clustering"]] = k_model@cluster

#retrieve data frame for cluster:i
focust_cluster <- function(i = 1)
{
  focus_customers = filter(customer_data, clustering == i) %>% select(customer_name)
  temp_table = inner_join(focus_customers,transaction,c("customer_name" = "CustomerLastName"))
  temp_table = inner_join(offer_info,temp_table,c("Offer" = "Offer"))
  temp_table = (select(temp_table,Offer:PastPeak)) %>% arrange(Offer,PastPeak)
  return(temp_table)
}

#exploratory-analysis for each cluster
for(i in 1:max(k_model@cluster)){
  par(mfrow=c(2,3))
  cluster_data = focust_cluster(i)
  hist(cluster_data$Discount,main = paste("cluster:",i,sep= " "))
  hist(cluster_data$MinimumQtykg)
  barplot(prop.table(table(cluster_data$Campaign)))
  barplot(prop.table(table(cluster_data$Varietal)))
  barplot(prop.table(table(cluster_data$Origin)))
  barplot(prop.table(table(cluster_data$PastPeak)))
}

#plot the silhouette from kmeans (will be in range [-1,1])
par(mfrow=c(1,1))
plot(silhouette(x=customer_data$clustering , dist(select(customer_data,offer_1:offer_32)) ))
