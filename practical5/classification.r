library(rpart)
library(caret)
library(e1071)
library(class)
data(iris)

#Holdout method
smp_size <- floor(0.75 * nrow(iris))
train <- iris[1:smp_size, ]
test <- iris[-(1:smp_size), ]

model <- naiveBayes(Species ~ ., data = train)
prediction <- predict(model, test)
View(confusionMatrix(prediction, test[,5])$table, 'NaiveBayes using Holdout')

model <- rpart(Species ~ ., data = train)
prediction <- predict(model, test, type = "class")
View(confusionMatrix(prediction, test[,5])$table, 'Decision Tree using Holdout')

prediction = knn(train[,-5], test[,-5], factor(train[,5]), k = 10)
View(confusionMatrix(prediction, test[,5])$table, 'KNN using Holdout')

#Random Subsampling
smp_size <- floor(0.75 * nrow(iris))
set.seed(123)
train_ind <- sample(nrow(iris), size = smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

model <- naiveBayes(Species ~ ., data = train)
prediction <- predict(model, test)
View(confusionMatrix(prediction, test[,5])$table, 'NaiveBayes using Random Sampling')

model <- rpart(Species ~ ., data = train)
prediction <- predict(model, test, type = "class")
View(confusionMatrix(prediction, test[,5])$table, 'Decision Tree using Random Sampling')

prediction = knn(train[,-5], test[,-5], factor(train[,5]), k = 10)
View(confusionMatrix(prediction, test[,5])$table, 'KNN using Random Sampling')

train_control <- trainControl(method="cv", number=10)
model <- train(Species~., data=iris, trControl=train_control, method="nb")
prediction <- predict(model, test)
View(confusionMatrix(prediction, test[,5])$table, 'NaiveBayes using KFOLD')

train_control <- trainControl(method="cv", number=10)
model <- train(Species~., data=iris, trControl=train_control, method="rpart")
prediction <- predict(model, test)
View(confusionMatrix(prediction, test[,5])$table, 'Decision Tree using KFOLD')

train_control <- trainControl(method="cv", number=10)
model <- train(Species~., data=iris, trControl=train_control, method="knn")
prediction <- predict(model, test)
View(confusionMatrix(prediction, test[,5])$table, 'KNN using KFOLD')
