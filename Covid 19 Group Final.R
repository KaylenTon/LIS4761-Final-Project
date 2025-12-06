# Loads libraries
library(tidyverse)
library(tidytext)
library(skimr)
library(lubridate)
library(textstem)

# Load and combines Twitter datasets
tweets_apr_jun <- read_csv("C:/Users/Polly/Downloads/archive/Covid-19 Twitter Dataset (Apr-Jun 2020).csv")
tweets_aug_sep <- read_csv("C:/Users/Polly/Downloads/archive/Covid-19 Twitter Dataset (Aug-Sep 2020).csv")

tweets <- bind_rows(tweets_apr_jun, tweets_aug_sep)

# Convert created_at to date
tweets <- tweets %>%
  mutate(date = as.Date(created_at, format = "%m/%d/%Y"),
         month = floor_date(date, "month"))   # add month column

# Filters for tweets in Florida
tweets_fl <- tweets %>%
  filter(str_detect(place, regex("Florida", ignore_case = TRUE)))

# Loads Bing sentiment dictionary
bing <- get_sentiments("bing")
data("stop_words")

# Tokenize and join with Bing
joined_fl <- tweets_fl %>%
  unnest_tokens(word, clean_tweet) %>%
  mutate(word = str_to_lower(word)) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(bing, by = "word")

# Exploratory Analysis

# Quick overview
skim(tweets)

# Overall sentiment distribution
joined_fl %>%
  count(sentiment.y) %>%
  ggplot(aes(x = sentiment.y, y = n, fill = sentiment.y)) +
  geom_col() +
  labs(title = "Overall Distribution of Tweet Sentiment (Florida)")

# Monthly sentiment distribution
joined_fl %>%
  count(month, sentiment.y) %>%
  ggplot(aes(x = month, y = n, fill = sentiment.y)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Sentiment Distribution (Florida)")

# Monthly tweet volume
tweets_fl %>%
  filter(!is.na(month)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "purple") +
  labs(title = "Monthly Tweet Volume (Florida)")

# Average monthly sentiment score
monthly_sentiment_fl <- joined_fl %>%
  mutate(sentiment_score = ifelse(sentiment.y == "positive", 1, -1)) %>%
  group_by(month) %>%
  summarise(
    avg_sentiment = mean(sentiment_score, na.rm = TRUE),
    tweet_volume = n()
  )

ggplot(monthly_sentiment_fl, aes(x = month, y = avg_sentiment)) +
  geom_line(color = "darkgreen") +
  labs(title = "Average Monthly Sentiment (Florida)")

# Top words in positive vs negative tweets
top_words_fl <- joined_fl %>%
  count(sentiment.y, word, sort = TRUE) %>%
  group_by(sentiment.y) %>%
  slice_max(n, n = 15)

ggplot(top_words_fl, aes(x = reorder(word, n), y = n, fill = sentiment.y)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment.y, scales = "free") +
  coord_flip() +
  labs(title = "Top Words in Positive vs Negative Florida Tweets")

