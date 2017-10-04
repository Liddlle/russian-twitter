#install.packages("caret")
library(caret)

########################
### Необходимые данные
########################
## загружаем размеченный корпус
twitter.coded <- read.csv("../data/full_corpus_done_for_seminar.csv", header=TRUE, as.is=TRUE)


## Подготовка данных в quanteda (альтернатива к tm)
library(quanteda)
library(stringr)
library(dplyr)
## строим матрицу термов-документов
dtm <- twitter.coded$all_tweets %>% str_replace_all('@[a-zA-Z0-9_]+', "@user") %>% tokens(what="word", remove_numbers=TRUE, remove_punct=FALSE, remove_separators=TRUE) %>% dfm 

## стемминг, отбрасывание слов с частотностью меньше 10, взвешивание (опционально)
dtm <- dtm %>% dfm_wordstem(language = "ru") %>% dfm_trim(min_count=10) #%>% dfm_weight(type="relmaxfreq") 
age.dtm <- cbind(age=twitter.coded$age, dtm) %>% as.data.frame

################################
### Feature engeneering
################################
looong <- str_count(twitter.coded$all_tweets, "\\w*([a-яА-Я])\\1\\1+\\w*")
age.dtm <- cbind(looong=looong, dtm)

###########################################
### Подготовка обучающей и тестовой выборки
###########################################

## зафиксируем случайные числа
#set.seed(191)
## отберем 10% выборки для тестирования
split <- createDataPartition(y=age.dtm$age, p = 0.9, list = FALSE)
train <- age.dtm[split,]
test <- age.dtm[-split,]
train.df <- twitter.coded[split,]
test.df <- twitter.coded[-split,]

###############################
## Обучение модели
###############################

###############################
## Задача — регрессия
###############################

### реализация на glmnet
library(glmnet)
train.dfm <- dfm_subset(dtm, split)
test.dfm <- dfm_subset(dtm, seq(1,nrow(dtm))[-split])

## Ridge regression с заранее заданным списком значений lambda
#lambdas <- 10^seq(3, -2, by = -.1)
#model.glmridge <- cv.glmnet(train.dfm, twitter.coded$age[split], alpha = 0, lambda = lambdas)

## Ridge regression с автоматическим подбором значений lambda
model.glmridge <- cv.glmnet(train.dfm, twitter.coded$age[split], alpha = 0)
plot(model.glmridge)
## Lasso regression с автоматическим подбором значений lambda
model.glmlasso <- cv.glmnet(train.dfm, twitter.coded$age[split], alpha = 1)
plot(model.glmlasso)


### реализация средствами caret
## Настройка параметров обучения
cvCtrl <- trainControl(method = "cv", number=10, verboseIter=TRUE) #  10-fold cross-validation

### значения alpha и lambda подбираются автоматически
model.glm <- train(age ~ . , data=train, method="glmnet", trControl=cvCtrl)
plot(model.glm)

###############################
## Оценка качества
###############################

## Предсказание значений с помощью модели
### реализация на glmnet
predicted.ridge <- predict.glmnet(model.glmridge$glmnet.fit, s = model.glmridge$lambda.min, newx=test.dfm, type="response")
### реализация на caret
predicted.age <- predict(model.glm, newdata=test)

## Визуальная оценка качества
plot(test.df$age, predicted.ridge)
## Более красивый график (и модель получше)
test.df %>% ggplot(aes(x=age, y=predicted.age, color=sex, label=screenName)) + geom_point() + geom_label()

age.predicted <- function(age, error, prediction) {
    (prediction > age-error) & (prediction < age+error)
}

age.prediction.accuracy <- function(predicted) {
    sum(predicted)/length(predicted)
}

## Верно ли модель предсказывает возраст
age.prediction.accuracy(age.predicted(test.df$age, test.df$error, predicted.age))
## А если с точностью ±5 лет?
age.prediction.accuracy(age.predicted(test.df$age, rep(5, length(test.df$age)), predicted.age))
## А если с точностью ±10 лет?
age.prediction.accuracy(age.predicted(test.df$age, rep(10, length(test.df$age)), predicted.age))

###############################
## Анализ переменных
###############################

## двадцать самых важных переменных, средствами caret
varImp(model.glm) 

## список самых больших коэффициентов модели, средствами glmnet
tmp_coeffs <- coef(model.glm$finalModel, s = model.glm$bestTune$lambda)
glm.coefs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
## просмотрим, что получилось
glm.coefs %>% arrange(-abs(coefficient)) %>% View


## положительные коэффициенты, возраст пользователей
glm.coefs %>% filter(coefficient>0 & abs(coefficient)>0.01) %>% arrange(-abs(coefficient))
summary(twitter.coded$age[as.vector(dtm[,"держав"])>0])
summary(twitter.coded$age[as.vector(dtm[,"льгот"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"льгот"])>0]))
summary(twitter.coded$age[as.vector(dtm[,"колхоз"])>0])
summary(twitter.coded$age[as.vector(dtm[,"#наук"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"#наук"])>0]))
summary(twitter.coded$age[as.vector(dtm[,"гадост"])>0])
summary(twitter.coded$age[as.vector(dtm[,"ипотек"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"ипотек"])>0]))

## colorful plot of all major elder words in one chart
## first we collect most important elder words, skip #hashtags and Intercept
elder.words <- glm.coefs %>% filter(coefficient>0 & abs(coefficient)>1.5) %>% arrange(-abs(coefficient)) %>% filter(!grepl("#|Intercept", name)) %>% .$name %>% as.character
## then plot it
data.frame(age=twitter.coded$age, dtm[,elder.words]) %>% gather(word, freq, -age) %>% filter(freq>0)  %>% ggplot(aes(x=age, color=word)) + geom_density()


## отрицательные коэффициенты, возраст пользователей
glm.coefs %>% filter(coefficient<0 & abs(coefficient)>0.01) %>% arrange(-abs(coefficient))
summary(twitter.coded$age[as.vector(dtm[,"урок"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"урок"])>0]))
summary(twitter.coded$age[as.vector(dtm[,"любл"])>0])
summary(twitter.coded$age[as.vector(dtm[,"универ"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"универ"])>0]))
summary(twitter.coded$age[as.vector(dtm[,"игнор"])>0])
plot(density(twitter.coded$age[as.vector(dtm[,"игнор"])>0]))
summary(twitter.coded$age[as.vector(dtm[,"сердечк"])>0])
summary(twitter.coded$age[as.vector(dtm[,"хоч"])>0])

## colorful plot of all major youngster words in one chart
## first we collect most important youngster words
youngster.words <- glm.coefs %>% filter(coefficient<0 & abs(coefficient)>0.1) %>% arrange(-abs(coefficient)) %>% .$name %>% as.character
## then plot it
data.frame(age=twitter.coded$age, dtm[,yongster.words]) %>% gather(word, freq, -age) %>% filter(freq>0) %>% ggplot(aes(x=age, color=word)) + geom_density()

