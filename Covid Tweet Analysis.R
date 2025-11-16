# Text Mining Covid Tweet Data
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)

# Covid Tweet Data from April to June of 2020
tweets1 <- read.csv("COVIDTweetsAprilToJune2020.csv") %>% 
  select(id, clean_tweet)

tweets1RT <- read.csv("COVIDTweetsAprilToJune2020.csv") %>% 
  select(id, retweet_count) # Will use inner_join() and use the retweet variable later.

tidy_tweets1 <- tweets1 %>% 
  unnest_tokens(word, clean_tweet) %>% 
  anti_join(stop_words) %>% 
  count(id, word) %>% 
  filter(n > 100)

tweets1_dtm <- tidy_tweets1 %>% 
  cast_dtm(id, word, n)

lda_out <- LDA(
  tweets1_dtm,
  k = 4, 
  method = "Gibbs",
  control = list(seed = 67)
)

glimpse(lda_out)

lda_topics <- lda_out %>% 
  tidy(matrix = "beta")

lda_topics <- lda_topics %>% 
  arrange(desc(beta)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 15) %>% 
  mutate(term2 = fct_reorder(term, beta))

ggplot(lda_topics, aes(term2, beta, fill = as.factor(topic))) + geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + coord_flip()

# Covid Tweet Data from August to September of 2020
tweets2 <- read.csv("COVIDTweetsAugustToSeptember2020.csv")

# Covid Tweet Data from April to June of 2021
tweets3 <- read.csv("COVIDTweetsAprilToJune2021.csv")
