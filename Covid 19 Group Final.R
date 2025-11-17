# Loads libraries
library(tidyverse)
library(tidytext)
library(skimr)
library(lubridate)

# Load and combines Twitter datasets
tweets_apr_jun <- read_csv("C:/Users/Polly/Downloads/archive/Covid-19 Twitter Dataset (Apr-Jun 2020).csv")
tweets_aug_sep <- read_csv("C:/Users/Polly/Downloads/archive/Covid-19 Twitter Dataset (Aug-Sep 2020).csv")

tweets <- bind_rows(tweets_apr_jun, tweets_aug_sep)

# Convert created_at to date
tweets <- tweets %>%
  mutate(date = as.Date(created_at, format = "%m/%d/%Y"),
         month = floor_date(date, "month"))   # add month column

# Create sentiment classification
tweets <- tweets %>%
  mutate(sentiment_class = case_when(
    pos > neg ~ "positive",
    neg > pos ~ "negative",
    TRUE ~ "neutral"
  ))

# Exploratory Analysis

# Quick overview
skim(tweets)

# Overall sentiment distribution
tweets %>%
  count(sentiment_class) %>%
  ggplot(aes(x = sentiment_class, y = n, fill = sentiment_class)) +
  geom_col() +
  labs(title = "Overall Distribution of Tweet Sentiment")

# Monthly sentiment distribution
tweets %>%
  count(month, sentiment_class) %>%
  ggplot(aes(x = month, y = n, fill = sentiment_class)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Sentiment Distribution")

# Monthly tweet volume
tweets %>%
  filter(!is.na(month)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "lightblue") +
  labs(title = "Monthly Tweet Volume")

# Average monthly sentiment score (pos - neg)
monthly_sentiment <- tweets %>%
  group_by(month) %>%
  summarise(
    avg_sentiment = mean(pos - neg, na.rm = TRUE),
    tweet_volume = n()
  )

ggplot(monthly_sentiment, aes(x = month, y = avg_sentiment)) +
  geom_line(color = "darkgreen") +
  labs(title = "Average Monthly Sentiment")

# Top words in positive vs negative tweets
top_words <- tweets %>%
  filter(sentiment_class %in% c("positive", "negative")) %>%
  unnest_tokens(word, clean_tweet) %>%
  count(sentiment_class, word, sort = TRUE) %>%
  group_by(sentiment_class) %>%
  slice_max(n, n = 15)

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment_class)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment_class, scales = "free") +
  coord_flip() +
  labs(title = "Top Words in Positive vs Negative Tweets")

