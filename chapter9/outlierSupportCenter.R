df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter9/SupportCenter.csv")
#correct percent column
df[,"X..Sick.Days.Taken.on.Friday"] = (as.numeric(sub("%","",df[,"X..Sick.Days.Taken.on.Friday"])) / 100)
df[,"Employee.ID"] = as.factor(df[,"Employee.ID"])
#normalize data
df[,2:ncol(df)] = scale(df[,2:ncol(df)])
df = df[1:400,]
#create distance matrix
distance_matrix = matrix(0,nrow=nrow(df),ncol=nrow(df))
for(i in 1:nrow(distance_matrix)){
  for(j in i:nrow(distance_matrix)){
    if(i == j){
      next
    }
    row_i = df[i,2:ncol(df)]
    row_j = df[j,2:ncol(df)]
    distance = sqrt(sum((row_i-row_j)^2))
    distance_matrix[i,j] = distance
    distance_matrix[j,i] = distance
  }
}
rownames(distance_matrix) = df[,1]
colnames(distance_matrix) = df[,1]

#turn each row in distance_matrix to a number indicates the order of distance
order_matrix = matrix(0,nrow=nrow(df),ncol=nrow(df))
for( i in 1:nrow(distance_matrix) ){
  rows = distance_matrix[i,]
  orders = order(rows)
  new_rows = vector(mode="numeric",length = length(rows))
  for(j in 1:length(orders)){
    new_rows[orders[j]] = j
  }
  order_matrix[i,] = new_rows
}
colnames(order_matrix) = df[,1]

#Method 1: count in-bound degrees
plotInboundEdge <- function(order_matrix,k){
  inbound_edges = apply(order_matrix,2,function(x){return(length(which(x < (k+1))))}) - 1
  plot(x = colnames(order_matrix),y = inbound_edges)
  return(inbound_edges)
}

inbound_edges = plotInboundEdge(order_matrix,k=20)
#find which Eployee ID still have no inbound edge
inbound_edges[inbound_edges == 0]
#137155 143406 
#0      0 

#Method 2: calculate k-distance for k = 5
k_distances = apply(distance_matrix,1,function(x){
  return(sort(x)[6])
})
names(k_distances) = df[,1]
head(sort(k_distances,decreasing = TRUE))
#  143406   137155   133358   132990   137095   141775 
#4.503634 4.066916 3.326019 3.268679 3.187560 3.175690 

#Method 3: Local Outlier Factor(LOF)
library(Rlof)

lof_data  = lof(df[,-1],k=5)
names(lof_data) = df[,1]
head(sort(lof_data,decreasing = TRUE))
#  143406   137155   144624   138710   142782   132833 
#1.995643 1.645404 1.446578 1.420133 1.413738 1.391650 