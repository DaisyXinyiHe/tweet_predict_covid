library(dplyr)
library(tidyverse)
library(ROCR)
library(lubridate)
library(randomForest)

## load city data
city_data = read_csv('./data/city.csv')
summary_covid =  read_csv('./data/summary_by_city_month.csv')
covid_case = read_csv('./data/us-counties.csv')

## combine summary covid (mean sentiment and covid related in tweets) and city info

add_zero = function(x){
  if (floor(log10(x) + 1 < 5)){x = paste0('0',x) }else{x = as.character(x)}
}

city_data$fips = sapply(city_data$fips, add_zero)
summary_covid$geo = sapply(summary_covid$geo, add_zero) 
summary_covid = inner_join(summary_covid, city_data, by = c('geo' = 'fips'))

## Previous month sentiment
summary_covid$month_year = paste0(summary_covid$month,'/',summary_covid$year)
summary_covid = summary_covid%>%
  arrange(geo, year, month)%>%
  group_by(geo)%>%
  mutate(pre_sentiment = lag(mean_sentiment))
  
summary_covid = summary_covid[complete.cases(summary_covid),]

## match city characteristics data with summary_by_city_month (tweet sentiment info)
## New York City is a special case. It does not have FIPS in the New York Time covid case data because it is a collective of Manhattan, 
## Queens, Brooklyn, etc. 

covid_case$month = month(covid_case$date)
covid_case$year = year(covid_case$date)
covid_case$fips[which(covid_case$county == 'New York City')] = '36061'

covid_case_by_fips_month_year = covid_case%>%
  arrange(fips,month,year)%>%
  group_by(fips, month, year)%>%
  mutate(day = row_number(), 
         cases_of_month = last(cases) - first(cases), 
         deaths_of_month = last(deaths) - first(deaths))%>%
  select(fips,month, year, county, cases_of_month, deaths_of_month)%>%
  unique(.)

summary_covid$geo = as.character(summary_covid$geo)
summary_covid = inner_join(summary_covid, covid_case_by_fips_month_year, by = c('geo' = 'fips', 'month' = 'month', 'year' = 'year'))%>%arrange(geo, year, month)


## Normalize feature
normalize_data = function(vector){
  norm_dat = (vector -mean(vector))/sd(vector)
  norm_dat                                                             
}

summary_covid[c('total_tweets', 'population_2019')] = 
  sapply(summary_covid[c('total_tweets', 'population_2019')], 
         normalize_data)

## Set feature as factor
summary_covid[c('month', 'year', 'region','month_year')] = sapply(summary_covid[c('month', 'year', 'region','month_year')] , as.factor)

## Train 2nd model: feature predicts covid cases and covid death
## Train Test Split: all LA data will be test
train = summary_covid%>%filter(area != 'Los Angeles')
test = summary_covid%>%filter(area == 'Los Angeles')

## Select features
train_geo = train['geo']
train = train%>%ungroup()%>%select( month_year, total_tweets, pre_sentiment, region, population_2019, cases_of_month, deaths_of_month)

# train = train%>%select( total_tweets, mean_sentiment, region, population_2019, cases_by_month, death_by_month)

test_geo = test['geo']
test = test%>%ungroup()%>%select( month_year, total_tweets, pre_sentiment, region, population_2019, cases_of_month, deaths_of_month)
# test = test%>%select( total_tweets,  mean_sentiment, region, population_2019, cases_by_month, death_by_month)

############ Linear Regression Model ##############

# Perform training: predict covid case
set.seed(1234)
train_cases = train%>%select(-deaths_of_month)
LR_1 = lm(cases_of_month ~ region+population_2019+month_year+total_tweets, data=train_cases)
LR_cases =lm(cases_of_month ~ ., data=train_cases)
summary(LR_1)
summary(LR_cases)
# LR_sentiment = lm(mean_sentiment~., data = train_cases)
# summary(LR_sentiment)

# Predict test case with LR with sentiment
test_cases = test%>%select(-deaths_of_month)
test_pred_1 = predict(LR_1,test_cases)
test_pred_cases = predict(LR_cases,test_cases)


# Root Mean square error
RMSE_cases_1 = sqrt(mean((test_pred_1  - test_cases$cases_of_month)**2))
RMSE_cases_LR = sqrt(mean((test_pred_cases  - test_cases$cases_of_month)**2))
print(paste0('Root mean sqaure error for not using previous month sentiment: ', RMSE_cases_1))
print(paste0('Root mean sqaure error for using previous month sentiment: ', RMSE_cases_LR))

# "Root mean sqaure error for not using previous month sentiment: 116050.055329422"
# "Root mean sqaure error for using previous month sentiment: 116048.751693893"


# Perform training: predict death case
set.seed(1234)
train_death = train%>%select(-cases_of_month)
LR_death_1 = lm(deaths_of_month ~ region+population_2019+month_year+total_tweets, data=train_death)
LR_death =lm(deaths_of_month ~ ., data=train_death)
summary(LR_death_1)
summary(LR_death)

# Predict test death
test_death = test%>%select(-cases_of_month)
test_pred_1 = predict(LR_death_1,test_death)
test_pred = predict(LR_death,test_death)


# Root Mean square error
RMSE_deaths_1 = sqrt(mean((test_pred_1   - test_death$deaths_of_month)**2))
print(paste0('Root mean sqaure error for no previous month sentiment: ', RMSE_deaths_1))
RMSE_deaths_LR = sqrt(mean((test_pred   - test_death$deaths_of_month)**2))
print(paste0('Root mean sqaure error for LR: ', RMSE_deaths_LR))

# "Root mean sqaure error for no previous month sentiment: 1791.95317360897"
# "Root mean sqaure error for LR: 1792.35845111703"

####### There is not much different between the RMSE of no-sentiment model and sentiment model. ###########

############ Random Forest Model ##############

# Perform training: predict covid case
set.seed(1234)
train_cases = train%>%select(-deaths_of_month)

rf_classifier_1 <- randomForest(cases_of_month ~ region+population_2019+month_year+total_tweets, 
                          data=train_cases, ntree=1000,
                          keep.forest=TRUE, importance=TRUE,
                          respect.unordered.factors = T)
rf_classifier_case = randomForest(cases_of_month ~ ., 
                                  data=train_cases, ntree=1000,
                                  keep.forest=TRUE, importance=TRUE,
                                  respect.unordered.factors = T)
print(importance(rf_classifier_1))
varImpPlot(rf_classifier_1)
print(importance(rf_classifier_case))
varImpPlot(rf_classifier_case)


train_pred_1 = predict(rf_classifier_1,train_cases)
train_pred = predict(rf_classifier_case,train_cases)


# Predict test case
test_cases = test%>%select(-deaths_of_month)
test_pred_1 = predict(rf_classifier_1,test_cases)
test_pred = predict(rf_classifier_case,test_cases)

# Root Mean square error
RMSE_cases_1 = sqrt(mean((test_pred_1  - test_cases$cases_of_month)**2))
print(paste0('Root mean square error for not using previous month sentiment: ', RMSE_cases_1))
RMSE_cases_RF = sqrt(mean((test_pred - test_cases$cases_of_month)**2))
print(paste0('Root mean square error for RF cases: ', RMSE_cases_RF))

## "Root mean square error for not using previous month sentiment: 116387.273873201"
##"Root mean square error for RF cases: 116441.844400449"

# Perform training: predict death case
set.seed(1234)
train_death = train%>%select(-cases_of_month)
rf_classifier_death_1 =randomForest(deaths_of_month ~ region+population_2019+month_year+total_tweets, 
                              data=train_death, ntree=1000,
                              keep.forest=TRUE, importance=TRUE,
                              respect.unordered.factors = T)
rf_classifier_death =randomForest(deaths_of_month ~ ., 
                                  data=train_death, ntree=1000,
                                  keep.forest=TRUE, importance=TRUE,
                                  respect.unordered.factors = T)
print(importance(rf_classifier_death_1))
varImpPlot(rf_classifier_death_1)
print(importance(rf_classifier_death))
varImpPlot(rf_classifier_death)

train_pred = predict(rf_classifier_death,train_death)

# Predict test death
test_death = test%>%select(-cases_of_month)
test_pred_1 = predict(rf_classifier_1,test_death)
test_pred = predict(rf_classifier_death,test_death)

# Root Mean square error
RMSE_deaths_1 = sqrt(mean((test_pred_1  - test_death$deaths_of_month)**2))
print(paste0('Root mean sqaure error for not using previous month sentiment: ', RMSE_deaths_1))
RMSE_deaths_RF = sqrt(mean((test_pred   - test_death$deaths_of_month)**2))
print(paste0('Root mean sqaure error for RF: ', RMSE_deaths_RF))

# "Root mean sqaure error for not using previous month sentiment: 36603.6897038828"
## Root mean sqaure error for RF: 1941.11581505466"

######Using Random Forest, we found the root mean square error of the model of using sentiment is better
# than the modle of not using sentiment. #########








