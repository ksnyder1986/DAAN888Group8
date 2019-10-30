library(caret)
library(ROCR)
library(nnet)
library("neuralnet")

review_analysis_data2 <- subset(review_analysis_data,select= c(Like_Restaurant,U_service,U_environment,U_Mexican,U_Middle_Eastern,U_European
                                                               ,U_Cafe,U_speed,U_location,U_Indian,U_Latin_American,U_Fastfood,U_Diners
                                                               ,U_cleanliness,U_Eastern_Asian,U_Southeast_Asian,U_Mediterranean,U_Seafood
                                                               ,U_Bars,U_taste,U_Japanese,U_American,U_Italian,U_Pizza,U_Breakfast
                                                               ,Mexican,Middle_Eastern,European,Cafe,Indian,Latin_American,Fastfood,Diners
                                                               ,Eastern_Asian,Southeast_Asian,Mediterranean,Seafood
                                                               ,Bars,Japanese,American,Italian,Pizza,Breakfast
                                                               ,B_Service,B_Speed,B_Cleanliness,B_Taste,B_Environment,B_Location))
review_analysis_data2 <- na.omit(review_analysis_data2)

smp_size <- floor(0.75 *nrow(review_analysis_data2))
set.seed(123)
train_ind <- sample(seq_len(nrow(review_analysis_data2)),size=smp_size)

train <- review_analysis_data2[train_ind,]
test <- review_analysis_data2[-train_ind,]

#Binomial Regression
log_model <- glm(Like_Restaurant ~ ., family=binomial("logit"),data=train)
summary(log_model)

log_pred_train <- as.numeric(predict(log_model, newdata = train) > 0.5)
mean(log_pred_train == train$Like_Restaurant)

log_pred_test <- as.numeric(predict(log_model, newdata = test) > 0.5)
mean(log_pred_test == test$Like_Restaurant)

(confusion_matrix <- table(predicted = log_pred_test, actual = test$Like_Restaurant))
(precision <- confusion_matrix[2,2] / sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2] / sum(confusion_matrix[,2]))
(f = 2*precision*recall/(precision+recall))
(specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,]))

#Multinomial Regression
multinom_model <- multinom(Like_Restaurant ~ ., data=train, maxit = 1000)
summary(multinom_model)

multinom_pred_train <- predict(multinom_model, newdata = train)
mean(multinom_pred_train == train$Like_Restaurant)

multinom_pred_test <- predict(multinom_model, newdata = test)
mean(multinom_pred_test == test$Like_Restaurant)

(confusion_matrix <- table(predicted = multinom_pred_test, actual = test$Like_Restaurant))
(precision <- confusion_matrix[2,2] / sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2] / sum(confusion_matrix[,2]))
(f = 2*precision*recall/(precision+recall))
(specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,]))


#Decision Tree
