library(readr)
library(dplyr)
library(tidytext)
library(foreach)



# Load data
filelist = list.files('./data')
tweet.data.full <- foreach(dat = 1:length(filelist), .combine='rbind') %do% {
  filename <- paste0('./data/',filelist[dat])
  if (file.exists(filename) == TRUE){
    data <- read_csv(filename)
    data = data%>%select(id, text, author_id, date, geo)


  }

  data
}

# write.csv(tweet.data.full, 'tweet_data_full.csv')





###################DRAFT################
# # Parse out words, count, and arrange by count
# words_data = data %>%select(text)%>%unnest_tokens(words, text)%>%
#   group_by(words)%>%
#   summarise(count = n())%>%
#   arrange(desc(count))
# 
# # Use the bing library to get sentiment
# bing_sentiment = get_sentiments("bing")
# words_data= inner_join(words_data, bing_sentiment, by = c('words' = 'word'))
# words_data$sentiment[which(words_data$sentiment == 'positive')] = 1
# words_data$sentiment[which(words_data$sentiment == 'negative')] = 0
# words_data$sentiment = as.numeric(words_data$sentiment)
# 
# # Avg Sentiment
# avg_sentiment = sum(words_data$sentiment*words_data$count) / sum(words_data$count)
# print(paste("Average sentiment:", avg_sentiment))
