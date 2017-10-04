library(streamR)
library(twitteR)
library(httpuv)
library(ROAuth)
library(dplyr)
library(tidyr)

## store API credentials into variables
api_key <- "lh3b1pDSlXTYXPgkoI4dbhEtJ"
api_secret <- "sGGV8WheYv7LEZNw27urmtGazwPm8p3qLDnFnia0tIR2MjWutr"
access_token <- "4339748027-duIBsxFgiunvDblDHjiYd14Vpq4fcQJlHFXdFGD"
access_secret <- "WM8uxnlN4X6b1wgaDYGzfsKRYI30uLT2oGYGX82y86cWm"

## initialize API credentials
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
my_oauth <- OAuthFactory$new(consumerKey = api_key, 
                             consumerSecret = api_secret, 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)
## authorize for streamR
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

9823314
## authorize for twitteR
setup_twitter_oauth(my_oauth$consumerKey, my_oauth$consumerSecret, my_oauth$oauthKey, my_oauth$oauthSecret)

## build query from a stopwords list
library("tm")
ruwords <- paste(stopwords("ru")[nchar(stopwords("ru"))<6], collapse=",")

## capture tweets using twitter streaming API
## a session lasts for 10 minutes and appends output to "rutweets.json" file
filterStream(file.name="rutweets.json", track = ruwords, language="ru", timeout = 60, oauth=my_oauth, verbose=TRUE)
tweets.df <- parseTweets("rutweets.json")

## look at the data
tweets.df %>% View

## fast frequency list building with stringr
library(stringr)
ht <- str_extract_all(tweets.df$text, "(\\d|\\w|-)+")
ht <- unlist(ht)
head(sort(table(ht), decreasing = TRUE))

## glance at a distribution of followers_count
library(ggplot2)
tweets.df %>% distinct(name, user_url, followers_count, friends_count) %>% ggplot(aes(x=log(followers_count))) + geom_density()

## sample 3200 users with <5000 followers
user.sample <- tweets.df %>% filter(followers_count<5000) %>% sample_n(100) %>% distinct(user_id_str, name, user_url)
## sample 3200 users with <1000 followers
user.sample1000 <- tweets.df %>% filter(followers_count<1000) %>% sample_n(100) %>% distinct(user_id_str, name, user_url)

## look what we have
tweets.df %>% filter(followers_count<1000) %>% filter(grepl("\\p{Lu}\\p{Ll}+ \\p{Lu}\\p{Ll}+", name, perl=TRUE)) %>% distinct(name, user_id_str) %>% sample_n(100) %>% View

## final sample used below
user.sample <- tweets.df %>% filter(followers_count<1000) %>% filter(grepl("\\p{Lu}\\p{Ll}+ \\p{Lu}\\p{Ll}+", name, perl=TRUE)) %>% distinct(name, user_id_str) %>% sample_n(100) 


## define functions to download tweets from the users included in the sample
## get at maximum 200 last tweets by a user and save to a data.frame
user_tweets <- function(user_id, n=200) {
    user.timeline <- data.frame()
        user.timeline <- userTimeline(user_id, n)
    return(twListToDF(user.timeline))
}

## get tweets for a given list of users, and save it into a data.frame
scrape_tweets <- function(user_list) {
    out.list <- list()
    for (user in user_list) {
        Sys.sleep(1) ## wait for a second between api calls
        print(user) ## show some progress message
        try( ## skip errors
            out.list[[user]] <- user_tweets(user)
            )
    }
    return(do.call("rbind", out.list))
} 

## get tweets for the sampled users (using functions defined above)
users.df <- scrape_tweets(user.sample$user_id_str)

## save precious downloaded data
write.csv(users.df, file="user.sample.csv", row.names=TRUE)
## load data back info R
users.df <- read.csv("user.sample.csv", colClasses="character")
users.df <- users.df %>%  separate(X, c("uid", "tweet_n"), sep="[.]") 

## how many users do we get, actually?
users.df %>% distinct(uid) %>% nrow

## how many users with at least 10 tweets?
users.df %>% group_by(uid) %>% mutate(ntweets = n()) %>% filter(ntweets>=10) %>% summarize(n()) %>% nrow

## discard users with less than 10 tweets
users.df <- users.df %>% group_by(uid) %>% mutate(ntweets = n()) %>% filter(ntweets>=10)

## concatenate all tweets by a user into a single text
user.tweets.df <- users.df %>% group_by(uid) %>% summarize(all_tweets=paste0(text, collapse=" "))

## prepare df with user-level metadata
user.info <- tweets.df %>% select(user_id_str, name, user_url, followers_count, statuses_count, friends_count, protected, user_lang,  user_created_at) %>% group_by(user_id_str) %>% summarize_all(function(x){x[1]}) 

## join tweets with user metadata
tweet.corpus <- left_join(user.tweets.df, user.info, by=c("uid" = "user_id_str"))

## write corpus
write.csv(tweet.corpus, "tweet.corpus.csv")
