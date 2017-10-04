install.packages("caret")
library(caret)
install.packages("glmnet")
library(glmnet)


########################
### Необходимые данные
########################

## corp — Corpus()
## dtm — DocumentTermMatrix()

###########################################
### Подготовка обучающей и тестовой выборки
###########################################

index <- 1:nrow(dtm) # номера документов от 1 до N
trainindex <- sample(index, trunc(length(index)*0.8)) # 80% случайных номеров (обучающая выборка)
training <- as.matrix(dtm[trainindex,]) # матрица обучающей выборки
testing <- as.matrix(dtm[-trainindex,]) # матрица тестовой выборки

###############################
## Обучение модели
###############################

## тренируем модель
training[, "age"] <- as.vector(training[, "age"])
cv_fit <- cv.glmnet(training, training[, "age"], alpha = 0)

## предсказываем возраст с оптимальной моделью из тренировки и оптимальной лямбдой
age.predicted <- predict(cv_fit$glmnet.fit, s = cv_fit$lambda.min, newx = testing)

## считаем эр квадрат
sst <- sum((age - mean(age))^2)
sse <- sum((age.prediction - age)^2)

1 - sse/sst

## R squared
rsq <- 1 - sse/sst
rsq
