#install.packages("caret")
library(caret)

########################
### Необходимые данные
########################

## corp — Corpus()
## dtm — DocumentTermMatrix()
## labels — метки классов

## Подготовка данных в quanteda (альтернатива к tm)
library(quanteda)
dtm <- twitter.coded$all_tweets %>% str_replace('@\w+', "@user") %>% tokens(remove_numbers=TRUE, remove_punctuation=TRUE) %>% dfm %>% dfm_trim(min_docfreq=10)
dtm <- dtm %>% dfm_wordstem(language = "ru") %>% dfm_weight(type="relmaxfreq") 
age.dtm <- cbind(age=twitter.coded$age, dtm) %>% as.data.frame

###########################################
### Подготовка обучающей и тестовой выборки
###########################################

## зафиксируем случайные числа
#set.seed(191)

## Случайная выборка
index <- 1:nrow(dtm) # номера документов от 1 до N
trainindex <- sample(index, trunc(length(index)*0.8)) # 80% случайных номеров (обучающая выборка)


split <- createDataPartition(y=age.dtm$age, p = 0.9, list = FALSE)
train <- age.dtm[split,]
test <- age.dtm[-split,]

###############################
## Обучение модели
###############################

###############################
## Задача — регрессия
###############################
ridgeFit <- train(age ~., data = train, method='ridge', lambda = 4, preProcess=c('scale', 'center'))

###############################
## Задача — классификация
###############################

labels <- twitter.coded$life_stage

trainlabels<-labels[train]
testlabels<-labels[-train]


## Настройка параметров обучения
cvCtrl <- trainControl(method = "cv", number=10) #  10-fold cross-validation

## Список доступных методов
names(getModelInfo())
## См. также http://topepo.github.io/caret/bytag.html

## Запуск процедуры обучения
## Логистическая регрессия
glmFit <- train(train, trainlabels, method="glmnet", alpha=0, trControl=cvCtrl)

###############################
## Оценка качества
###############################

## Предсказание классов на тестовой выборке с помощью обученной модели
ridgeClasses <- predict(ridgeFit, newdata = testing)
glmClasses <- predict(glmFit, newdata = testing)

## Confusion Matrix
glmCM <- confusionMatrix(data = glmClasses, reference=testlabels, positive="TRUE")
glmCM # просмотрим результаты теста

ridgeCM <- confusionMatrix(data = ridgeClasses, reference=testlabels, positive="TRUE")
ridgeCM

###############################
## Анализ переменных
###############################

varImp(ridgeFit) # двадцать самых важных переменных
varImp(glmFit) # двадцать самых важных переменных
