library(igraph)

friend_graph = graph_from_literal(Ross--Rachel,Joey--Rachel,Rachel--Chandler,Chandler--Monica,Chandler--Phoebe)
plot(friend_graph)
friend_communities = cluster_louvain(friend_graph)
plot(friend_communities,friend_graph)
