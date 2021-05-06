library(dplyr)
library(tidyverse)
library(stringr)
library(ROCR)

## Load training data
model_data =read.csv('./data/Coded_Full_RC.csv')

#train test split
set.seed(1234)
train_data = model_data[sample(nrow(model_data),0.8*nrow(model_data)),]
test_data = setdiff(model_data,train_data)


## Count word frequency in covid related and non-covid related sentences

# Unnest words from tweets
word_covid_rltion <- train_data %>%
  select(text, covid_related) %>%
  tidytext::unnest_tokens(word, text)


# Percentage of the word appearing in covid related tweets out of all the tweets and 
# in noncovid related tweets out of all the tweets
prop_word= word_covid_rltion%>%group_by(word)%>%
  summarise(count_covid = sum(covid_related==1),prop_covid = count_covid /n(),
            count_noncovid =sum(covid_related==0),prop_noncovid = count_noncovid /n() )%>%
  # Filter out count >=10
  filter(count_covid >= 10 | count_noncovid >=10)%>%
  arrange(desc(count_covid))

# Covid related words are words that have a higher percentage of appearing in covid related tweets than in a non-covid related tweet. Vise versa. 
word_covid = prop_word$word[which(prop_word$prop_covid>prop_word$prop_noncovid)]
word_noncovid = prop_word$word[which(prop_word$prop_noncovid>prop_word$prop_covid)]

# Probability of words in covid related tweets out of all the words in covid tweet and 
# probability of words in non-covid related tweets out of all the words in non-covid tweet 
prop_word2= word_covid_rltion%>%group_by(word)%>%
  summarise(count_covid = sum(covid_related==1),prop_covid = count_covid /length(which(word_covid_rltion$covid_related == 1)),
            count_noncovid =sum(covid_related==0),prop_noncovid = count_noncovid /length(which(word_covid_rltion$covid_related == 0)) )%>%
  filter(count_covid >= 10 | count_noncovid >=10)%>%
  arrange(desc(count_covid))

#  Covid related words are words that have a probability of appearing in covid related tweet higher than a probability appearing
# in noncovid related tweet. Vise versa.
word_covid2 = prop_word2$word[which(prop_word2$prop_noncovid*0.50 + prop_word2$prop_noncovid < prop_word2$prop_covid)]
word_noncovid2 = prop_word2$word[which(prop_word2$prop_covid*0.50 + prop_word2$prop_covid < prop_word2$prop_noncovid)]

# Top 50 words with the highest frequency in covid and in noncovid tweet
word_covid3 = prop_word%>%arrange(desc(prop_covid))
word_covid3 = word_covid3$word[1:50]
word_noncovid3 = prop_word%>%arrange(desc(prop_noncovid))
word_noncovid3 = word_noncovid3$word[1:50]

## Find words in tweets
count_covid = sapply(word_covid, grepl, train_data$text)
count_noncovid = sapply(word_noncovid, grepl, train_data$text)

count_covid2 = sapply(word_covid2, grepl, train_data$text)
count_noncovid2 = sapply(word_noncovid2, grepl, train_data$text)

count_covid3 = sapply(word_covid3, grepl, train_data$text)
count_noncovid3 = sapply(word_noncovid3, grepl, train_data$text)

## Count words in tweets
train_data$word_appear_in_covid_count = apply(count_covid, 1, function(x) length(which(x == TRUE)))
train_data$word_appear_in_noncovid_count = apply(count_noncovid, 1, function(x) length(which(x == TRUE)))

train_data$word_more_covid = apply(count_covid2, 1, function(x) length(which(x == TRUE)))
train_data$word_more_noncovid = apply(count_noncovid2, 1, function(x) length(which(x == TRUE)))

train_data$word_highfreq_covid = apply(count_covid3, 1, function(x) length(which(x == TRUE)))
train_data$word_highfreq_noncovid = apply(count_noncovid3, 1, function(x) length(which(x == TRUE)))


## Word count
train_data$wordcount = sapply(strsplit(train_data$text, " "), length)

## Normalize data
normalize_data = function(vector){
  norm_dat = (vector -mean(vector))/sd(vector)
  norm_dat                                                             
}

train_data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')] = 
  sapply(train_data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')], 
         normalize_data)


## Fit log regression model
train_data$covid_related = as.factor(as.numeric(train_data$covid_related))
logreg = glm(covid_related~word_appear_in_covid_count+word_appear_in_noncovid_count+
               word_more_covid+word_more_noncovid+word_highfreq_covid+word_highfreq_noncovid+
               wordcount, data = train_data,family = binomial())
summary(logreg)

## predict covid-related in train data
train_pred_prob= predict(logreg, train_data, type='response')
covid_pred = ifelse(train_pred_prob>=0.5, 1,0)
# Accuracy
accuracy = mean(train_data$covid_related == covid_pred)
print(accuracy)
# Confusion matrix
table(train_data$covid_related,covid_pred)
# AUC
train.pred <- prediction(train_pred_prob, train_data$covid_related)
train.perf <- performance(train.pred, "auc")
print(paste('the auc score is ', train.perf@y.values[[1]]))

## The confusion matrix here shows that the log regression is biased toward classifying Covid-related tweet (make sense because
## we have more covid-related data)

## Test Data

## Find words in tweets
count_covid = sapply(word_covid, grepl, test_data$text)
count_noncovid = sapply(word_noncovid, grepl, test_data$text)

count_covid2 = sapply(word_covid2, grepl, test_data$text)
count_noncovid2 = sapply(word_noncovid2, grepl, test_data$text)

count_covid3 = sapply(word_covid3, grepl, test_data$text)
count_noncovid3 = sapply(word_noncovid3, grepl, test_data$text)

## Count words in tweets
test_data$word_appear_in_covid_count = apply(count_covid, 1, function(x) length(which(x == TRUE)))
test_data$word_appear_in_noncovid_count = apply(count_noncovid, 1, function(x) length(which(x == TRUE)))

test_data$word_more_covid = apply(count_covid2, 1, function(x) length(which(x == TRUE)))
test_data$word_more_noncovid = apply(count_noncovid2, 1, function(x) length(which(x == TRUE)))

test_data$word_highfreq_covid = apply(count_covid3, 1, function(x) length(which(x == TRUE)))
test_data$word_highfreq_noncovid = apply(count_noncovid3, 1, function(x) length(which(x == TRUE)))

## Word count
test_data$wordcount = sapply(strsplit(test_data$text, " "), length)

## Normalize data
test_data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')] = 
  sapply(test_data[c('word_appear_in_covid_count','word_appear_in_noncovid_count','word_more_covid','word_more_noncovid','word_highfreq_covid','word_highfreq_noncovid','wordcount')], 
         normalize_data)


## predict covid-related
test_pred_prob= predict(logreg, test_data, type='response')
covid_pred = ifelse(test_pred_prob>=0.5, 1,0)
# Accuracy
accuracy = mean(test_data$covid_related == covid_pred)
print(accuracy)
# Confusion matrix
table(test_data$covid_related,covid_pred)
# AUC
test.pred <- prediction(test_pred_prob, test_data$covid_related)
test.perf <- performance(test.pred, "auc")
print(paste('the auc score is ', test.perf@y.values[[1]]))

## Save log regression model 1 for predicting covid related vs. not
save(logreg, file = "./model/covid_rltion_model.rda")

## Save word dictionary
save(word_covid, file = "./dictionary/word_appear_in_covid_count.txt")
save(word_covid2, file = "./dictionary/word_more_covid.txt")
save(word_covid3, file = "./dictionary/word_highfreq_covid.txt")

save(word_noncovid, file = "./dictionary/word_appear_in_noncovid_count.txt")
save(word_noncovid2, file = "./dictionary/word_more_noncovid.txt")
save(word_noncovid3, file = "./dictionary/word_highfreq_noncovid.txt")

################################## DRAFT #######################################
##### if have random tweets that is not from keyword search
## Inspect data and see if prediction probability match annotation. 
## Visualization: average per tweet probability being covid related for each city each month/year, plot
## Plot 1: in each city, visualize the expected chance that a tweet is covid-related
## Plot 2: same thing by month, or Plot 3: same thing by both city and month


