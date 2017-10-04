#install.packages("caret")
library(caret)

########################
### Необходимые данные
########################
## загружаем размеченный корпус
twitter.coded <- read.csv("../data/full_corpus_done_for_seminar.csv", header=TRUE, as.is=TRUE)

################################
### Немного дескриптивки
################################

### Распределение возраста по полу
twitter.coded %>% ggplot(aes(x=age, color=sex)) + geom_density()

################################
### Считаем слова
################################

## Подготовка данных в quanteda (альтернатива к tm)
library(quanteda)
library(stringr)
library(dplyr)
## строим матрицу термов-документов
dtm <- twitter.coded$all_tweets %>% str_replace_all('@[a-zA-Z0-9_]+', "@user") %>% tokens(what="word", remove_numbers=TRUE, remove_punct=FALSE, remove_separators=TRUE) %>% dfm 

## стемминг, отбрасывание слов с частотностью меньше 10, взвешивание (опционально)
dtm <- dtm %>% dfm_wordstem(language = "ru") %>% dfm_trim(min_docfreq=0.05) #%>% dfm_weight(type="relmaxfreq") 

################################
### Feature engeneering
################################
looong <- str_count(twitter.coded$all_tweets, "\\w*([a-яА-Я])\\1\\1+\\w*")
dtm.extended <- cbind(looong=looong, dtm) %>% as.data.frame


###########################################
### Подготовка обучающей и тестовой выборки
###########################################

## зафиксируем случайные числа
set.seed(2939)
## отберем 10% выборки для тестирования
split <- createDataPartition(y=age.dtm$age, p = 0.9, list = FALSE)
train.data <- dtm.extended[split,]
test.data <- dtm.extended[-split,]
train.df <- twitter.coded[split,]
test.df <- twitter.coded[-split,]


###############################
## Обучение модели
###############################

## параметры обучения: 10-fold кросс-валидация
ctrl <- trainControl(method="cv", 10, verboseIter=TRUE)


##########################################################
## Задача — классификация (предсказание пола пользователя)
##########################################################

## Logistic regression (Maxent classifier) with regularization (elasticnet)
model.lr <- train(train.data, train.df$sex, method="glmnet", family="binomial", trControl=ctrl)
## glance at the model quality
model.lr

## install.packages("naivebayes")
## Naive Bayes classifier
model.nb <- train(train.data, train.df$sex, method="naive_bayes", trControl=ctrl)
## glance at the model quality
model.nb


## Список доступных алгоритмов (классификации и не только)
names(getModelInfo())
## См. также http://topepo.github.io/caret/bytag.html

###############################
## Оценка качества
###############################

## Предсказание значений с помощью модели

## логистическая регрессия
predicted.sex.lr <- predict(model.lr, newdata=test.data)
cm.lr <- confusionMatrix(data = predicted.sex.lr, reference = test.df$sex, positive="ж")
cm.lr

## наивный байес
predicted.sex.nb <- predict(model.nb, newdata=test.data)
cm.nb <- confusionMatrix(data = predicted.sex.nb, reference = test.df$sex, positive="ж")
cm.nb

###############################
## Анализ переменных
###############################
## MaxEnt classifier most important features
varImp(model.lr)
## Naive Bayes classifier most important features
varImp(model.nb)

## список самых больших коэффициентов модели, средствами glmnet
tmp_coeffs <- coef(model.lr$finalModel, s = model.lr$bestTune$lambda)
glm.coefs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
## просмотрим, что получилось
glm.coefs %>% arrange(-abs(coefficient)) %>% View
## насколько полезна наша новая фича?
glm.coefs %>% arrange(-abs(coefficient)) %>% filter(name=="looong")

## male features
glm.coefs %>% filter(coefficient>0 & abs(coefficient)>0.1) %>% arrange(-abs(coefficient))

## colorful plot of all major male words in one chart
## first we collect most important male words, skip #hashtags and Intercept
male.words <- glm.coefs %>% filter(coefficient>0 & abs(coefficient)>0.3) %>% arrange(-abs(coefficient)) %>% filter(!grepl("#|Intercept", name)) %>% .$name %>% as.character
## then plot it
data.frame(age=twitter.coded$age, dtm[,male.words]) %>% gather(word, freq, -age) %>% filter(freq>0)  %>% ggplot(aes(x=age, color=word)) + geom_density()

## female features
glm.coefs %>% filter(coefficient<0 & abs(coefficient)>0.1) %>% arrange(-abs(coefficient))

## colorful plot of all major female words in one chart
## first we collect most important female words, skip #hashtags and Intercept
female.words <- glm.coefs %>% filter(coefficient<0 & abs(coefficient)>0.25) %>% arrange(-abs(coefficient)) %>% filter(!grepl("#|Intercept", name)) %>% .$name %>% as.character
## then plot it
data.frame(age=twitter.coded$age, dtm[,female.words]) %>% gather(word, freq, -age) %>% filter(freq>0)  %>% ggplot(aes(x=age, color=word)) + geom_density()

####################################
### Error analysis
####################################

misclassified.females <- twitter.coded$screenName[which(predicted.sex.lr == "м" & test.df$sex == "ж")]
misclassified.males <- twitter.coded$screenName[which(predicted.sex.lr == "ж" & test.df$sex == "м")]

## construct corpus
corp <- corpus(twitter.coded$all_tweets, docvars=twitter.coded[,c("screenName", "sex", "age", "error", "life_stage")])

## select misclassified female tweets
corp.misc.female <- corp %>% corpus_subset(screenName %in% misclassified.females)
## count words
misc.female.dtm <- corp.misc.female %>% str_replace_all('@[a-zA-Z0-9_]+', "@user") %>% tokens(what="word", remove_numbers=TRUE, remove_punct=FALSE, remove_separators=TRUE) %>% dfm %>% dfm_wordstem(language = "ru") %>% dfm_trim(min_docfreq=10) 
## plot wordcloud
textplot_wordcloud(misc.female.dtm)
## look for possible keywords
textstat_keyness(dtm, twitter.coded$screenName %in% misclassified.females) %>% head(20)

## select misclassified male tweets
corp.misc.male <- corp %>% corpus_subset(screenName %in% misclassified.females)
## count words
misc.male.dtm <- corp.misc.male %>% str_replace_all('@[a-zA-Z0-9_]+', "@user") %>% tokens(what="word", remove_numbers=TRUE, remove_punct=FALSE, remove_separators=TRUE) %>% dfm %>% dfm_wordstem(language = "ru") %>% dfm_trim(min_docfreq=10) 
## plot wordcloud
textplot_wordcloud(misc.male.dtm)
## look for possible keywords
textstat_keyness(dtm, twitter.coded$screenName %in% misclassified.males) %>% head(20)
