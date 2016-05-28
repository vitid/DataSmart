library(igraph)

df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter5/wines.csv")
df[is.na(df)] = 0

#generate adjacent-cosine-similarity matrix
sim_matrix = matrix(0,nrow = length(col_names), ncol = length(col_names))
col_names = colnames(df)
for(i in 1:length(col_names)){
  for(j in i:length(col_names)){
    if(i == j){
      #the book leaves diagonal as "0"(can be confusing...)
      next
    }
    col_i = df[,col_names[i]]
    col_j = df[,col_names[j]]
    cosine_similarity = col_i %*% col_j / sqrt( (col_i %*% col_i) * (col_j %*% col_j) )
    sim_matrix[i,j] = cosine_similarity
    sim_matrix[j,i] = cosine_similarity
  }
}

colnames(sim_matrix) = col_names

print(sprintf("The graph has %s edges",length(sim_matrix[sim_matrix > 0]) ))
#as 80-percentile, the cut-off cosine similarity is 0.5
quantile(sims,c(0.8))

df = as.data.frame(sim_matrix)
df[df >= 0.5] = 1
df[df < 0.5] = 0
rownames(df) = col_names

write.table(df , 
          file = "/home/vitidn/mydata/repo_git/DataSmart/chapter5/wines.top20.csv", 
          sep = ";",
          quote = FALSE,
          row.names = TRUE)

adj_matrix = as.matrix(df)
nw_graph = graph.adjacency(adjmatrix = adj_matrix,mode="undirected")
#visualize graph
V(nw_graph)$size = 3
plot(nw_graph)
#Louvain algorithm for community detection(can detect overlapping community)
nw_comm = cluster_louvain(nw_graph)
plot(nw_comm,nw_graph)
print(sprintf("Modularity Score: %f",modularity(nw_comm)))
print(sprintf("#Communities: %d",length(nw_comm)))
