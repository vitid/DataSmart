library(ggplot2)
#sampling Brix/Acid of 1000 "Biondo Commune" orange juices from Egypt
samplings = rnorm(1000,mean=13,sd=0.9)
plot(density(samplings))

#compare Brix/Acid of 11 orange juices
mean_vector = c(10.5,6.5,12,11,12,10,9,15,8,13,14)
sd_vector = c(2,1.1,0.2,1,1.3,1.4,0.3,0.8,1,0.9,0.6)

#sampling 1000 instances
data_matrix = sapply(1:11, function(x){
  return(rnorm(1000,mean_vector[x],sd_vector[x]))
})
colnames(data_matrix) = c("Hamlin/BZ","Mosambi","Valencia","Hamlin/CA","Gardner","Sunstar","Jincheng","Berna","Verna","Biondo Commune","Belladonna")
df = as.data.frame(data_matrix)
df_stack = stack(df)

ggplot(df_stack,aes(x=values)) + geom_density(aes(group = ind, colour = ind, fill = ind),alpha = 0.3)
