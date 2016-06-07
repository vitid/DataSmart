library(randomForest)
library(ROCR)

df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter7/RetailMart.csv")
df_test = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter6/RetailMart.test.csv")

rf_model = randomForest(PREGNANT ~ .,data = df,replace = FALSE,maxnodes = 2)
predict_values = predict(rf_model,df_test)
plot(performance(prediction(predict_values,df_test[,"PREGNANT"]),measure = "tpr",x.measure = "fpr"))
title("ROC")

#compare with vanilla RandomForest
rf_model = randomForest(PREGNANT ~ .,data = df)
predict_values = predict(rf_model,df_test)
plot(performance(prediction(predict_values,df_test[,"PREGNANT"]),measure = "tpr",x.measure = "fpr"),add=TRUE,lty = 2)
legend("bottomright",c("Bagged(max 2 nodes)","RandomForest"),lty = c(1,2))

#boosting -> wait for chapter 10...
