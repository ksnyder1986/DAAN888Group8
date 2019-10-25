library(caret)
library(ROCR)
library(nnet)
library("neuralnet")

review_analysis_data2 <- subset(review_analysis_data,select= -c(user_id,business_id))
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

#Neural Network
review_analysis_data2_pp <- preProcess(review_analysis_data2, method = c("range"))
train_pp <- predict(review_analysis_data2_pp, train)
test_pp <- predict(review_analysis_data2_pp, test)

n <- names(review_analysis_data2)
f <- as.formula(paste("Like_Restaurant ~", paste(n[!n %in% c("Like_Restaurant")], collapse = " + ")))
nn_model <- neuralnet(formula = f , data=train_pp, hidden=1)
summary(nn_model)

#Decision Tree
