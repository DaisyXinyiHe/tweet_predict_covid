summary_covid <- read.csv("/Users/rosiecheng/Desktop/Spring 2021/ML/Final Project/Google drive downloads/summary_covid.csv")

city_plot_data <- summary_covid %>%
  select(region, area, month_year, total_tweets, mean_sentiment,cases_of_month, deaths_of_month) %>% 
  mutate(total_tweets_std = scale(total_tweets, scale = T),
         cases_of_month_std = scale(cases_of_month, scale = T),
         deaths_of_month_std = scale(deaths_of_month, scale = T)) %>%
  mutate(time = case_when(month_year == "4/2020" ~ 1,
                          month_year == "5/2020" ~ 2,
                          month_year == "6/2020" ~ 3,
                          month_year == "7/2020" ~ 4,
                          month_year == "8/2020" ~ 5,
                          month_year == "9/2020" ~ 6,
                          month_year == "10/2020" ~ 7,
                          month_year == "11/2020" ~ 8,
                          month_year == "12/2020" ~ 9,
                          month_year == "1/2021" ~ 10,
                          month_year == "2/2021" ~ 11,
                          month_year == "3/2021" ~ 12,
                          month_year == "4/2021" ~ 13)) 

city_plot_data$time <- as.numeric(city_plot_data$time)


###### Plot by city
city_plot_data$area <- factor(city_plot_data$area,
                             levels = c('New York City', 'Los Angeles', 'Chicago', 'Miami', 'Dallas',
                                 'Philadelphia', 'Houston', 'Atlanta', 'Washington', 'Boston',
                                 'Phoenix', 'Seattle', 'San Francisco', 'Detroit', 'San Diego',
                                 'Minneapolis', 'Tampa', 'Denver', 'Riverside', 'Baltimore',
                                 'Las Vegas', 'Portland', 'San Antonio', 'St. Louis', 'Sacramento',
                                 'Orlando', 'San Jose', 'Cleveland', 'Pittsburgh', 'Austin',
                                 'Cincinnati', 'Kansas City', 'Indianapolis', 'Columbus', 'Charlotte',
                                 'Virginia Beach', 'Milwaukee', 'Providence', 'Jacksonville', 'Salt Lake City',
                                 'Nashville', 'Richmond', 'Memphis', 'Raleigh', 'New Orleans',
                                 'Louisville', 'Oklahoma City', 'Bridgeport', 'Buffalo'))




plot_1 <- ggplot(data = city_plot_data, aes(time)) +
  facet_wrap(~ area) +
  geom_line(aes(y=mean_sentiment), colour="sienna3") +  
  geom_line(aes(y=total_tweets_std), colour="skyblue3") +
  geom_line(aes(y=cases_of_month_std), colour="darkolivegreen4") +
  geom_line(aes(y=deaths_of_month_std), colour="goldenrod3") +
  scale_x_continuous("Month", c(1:13)) +
  scale_y_continuous("Standardized number", limits = c(-3,3)) +
  ggtitle("Number of crimes each hour by crime types")

ggsave(plot = plot_1, file='by_city.png', height=5, width=15)



#####  Plot by region
region_plot_data <- city_plot_data %>%
  select(region, time, total_tweets, mean_sentiment,cases_of_month, deaths_of_month) %>%
  group_by(time, region) %>%
  mutate(sentiment_region = mean(mean_sentiment),
         total_tweets_region = sum(total_tweets),
         total_cases_region = sum(cases_of_month),
         total_deaths_region = sum(deaths_of_month)) %>%
  ungroup() %>%
  mutate(total_tweets_region_std = (total_tweets_region - mean(total_tweets_region))/sd(total_tweets_region),
         total_cases_region_std = (total_cases_region - mean(total_cases_region))/sd(total_cases_region),
         total_deaths_region_std = (total_deaths_region - mean(total_deaths_region))/sd(total_deaths_region))

# region_plot_data <- city_plot_data %>%
#   select(region, time, total_tweets, mean_sentiment,cases_of_month, deaths_of_month) %>% 
#   group_by(time, region) %>%
#   mutate(sentiment_region = 10*mean(mean_sentiment),
#          total_tweets_region = sum(total_tweets),
#          total_cases_region = sum(cases_of_month),
#          total_deaths_region = sum(deaths_of_month)) %>% 
#   ungroup() %>% 
#   mutate(total_tweets_region_log = log(total_tweets_region),
#          total_cases_region_log = log(total_cases_region),
#          total_deaths_region_log = log(total_deaths_region)) 
# 
# region_plot_data$region <- factor(region_plot_data$region,
#                              levels = c('west','midwest','southwest','southeast','northeast'))


plot_2 <- ggplot(data = region_plot_data, aes(time)) +
  facet_wrap(~ region, nrow =1, ncol = 5) +
  geom_line(aes(y=sentiment_region), colour="sienna3") +  
  geom_line(aes(y=total_tweets_region_std), colour="skyblue3") +
  geom_line(aes(y=total_cases_region_std), colour="darkolivegreen4") +
  geom_line(aes(y=total_deaths_region_std), colour="goldenrod3") +
  scale_x_continuous("Month", c(1:13)) +
  scale_y_continuous("Numbeer (with transformations", limit = c(4,14)) +
  ggtitle("Trends in Five Regions")

ggsave(plot = plot_2, file='by_region.png', height=5, width=15)
  

  

  
  
  
  
  
  



