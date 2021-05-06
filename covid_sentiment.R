library(dplyr)
library(tidyverse)
library(stringr)
library(ROCR)
library(tidytext)
library(lubridate)

## load the covid related tweet prediction model
load("./model/covid_rltion_model.rda")

## load city characteristics

## Load full data 
data = read.csv('./data/tweet_data_full.csv')

## Load Dictionary
load("./dictionary/word_appear_in_covid_count.txt")
load("./dictionary/word_more_covid.txt")
load("./dictionary/word_highfreq_covid.txt")
load("./dictionary/word_appear_in_noncovid_count.txt")
load("./dictionary/word_more_noncovid.txt")
load("./dictionary/word_highfreq_noncovid.txt")

## process data
## Find words in tweets
count_covid = sapply(word_covid, grepl, data$text)
count_noncovid = sapply(word_noncovid, grepl, data$text)

count_covid2 = sapply(word_covid2, grepl, data$text)
count_noncovid2 = sapply(word_noncovid2, grepl, data$text)

count_covid3 = sapply(word_covid3, grepl, data$text)
count_noncovid3 = sapply(word_noncovid3, grepl, data$text)

## Count words in tweets
data$word_appear_in_covid_count = apply(count_covid, 1, function(x) length(which(x == TRUE)))
data$word_appear_in_noncovid_count = apply(count_noncovid, 1, function(x) length(which(x == TRUE)))

data$word_more_covid = apply(count_covid2, 1, function(x) length(which(x == TRUE)))
data$word_more_noncovid = apply(count_noncovid2, 1, function(x) length(which(x == TRUE)))

data$word_highfreq_covid = apply(count_covid3, 1, function(x) length(which(x == TRUE)))
data$word_highfreq_noncovid = apply(count_noncovid3, 1, function(x) length(which(x == TRUE)))


## Word count
data$wordcount = sapply(strsplit(data$text, " "), length)

## Normalize data
normalize_data = function(vector){
  norm_dat = (vector -mean(vector))/sd(vector)
  norm_dat                                                             
}

data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')] = 
  sapply(data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')], 
         normalize_data)

## Predict covid related probability
data$covid_relate_prop = predict(logreg, data, type='response')

## Parse out month and year
data$month = month(data$date)
data$year = year(data$date)

## Analyze tweet sentiment
bing_word_sentiment = get_sentiments("bing")

summary_by_city_month = data%>%
  unnest_tokens(words, text)%>%
  inner_join(.,bing_word_sentiment, by = c('words' = 'word'))
summary_by_city_month$sentiment = ifelse(summary_by_city_month$sentiment == "positive", 1,0)
summary_by_city_month = summary_by_city_month%>% group_by(geo, month,year)%>%
  summarise(total_tweets = n(),
            mean_covid_relate = mean(covid_relate_prop),
            mean_sentiment = mean(sentiment))  

write.csv(summary_by_city_month, './data/summary_by_city_month.csv')








  