library(ROCR)

#Fit with Linear Regression
#training set
df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter6/RetailMart.csv")
linear_model = lm(PREGNANT ~ .,data = df)
summary(linear_model)

#test set
df_test = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter6/RetailMart.test.csv")
predict_values = predict(linear_model,df_test)

#plot ROC curve
prediction_object = prediction(predict_values,df_test[,"PREGNANT"])

roc.perf = performance(prediction_object,measure = "tpr",x.measure = "fpr")
plot(roc.perf)
title("ROC")

roc.perf = performance(prediction_object,measure = "tpr",x.measure = "cutoff")
plot(roc.perf)
title("True Positive Rate vs Cutoff")

roc.perf = performance(prediction_object,measure = "fpr",x.measure = "cutoff")
plot(roc.perf)
title("False Positive Rate vs Cutoff")

#Fit with Logistic Regression
logistic_model = glm(PREGNANT ~ ., data = df,family = binomial())
summary(logistic_model)
prob_values = predict(logistic_model,df_test,type = "response")
#plot ROC curve
plot(performance(prediction(prob_values,df_test[,"PREGNANT"]),measure = "tpr",x.measure = "fpr"))
title("ROC")

#Compare Linear Regression & Logistic Regression
plot(performance(prediction(predict_values,df_test[,"PREGNANT"]),measure = "tpr",x.measure = "fpr"))
plot(performance(prediction(prob_values,df_test[,"PREGNANT"]),measure = "tpr",x.measure = "fpr"), lty = 2, add = TRUE)
title("ROC Comparison")
legend("bottomright",c("Linear","Logistic"),lty = c(1,2))
