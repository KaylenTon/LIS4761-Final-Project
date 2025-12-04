# Text Mining Covid Tweet Data
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)


data <- read.csv("COVIDTweetsAprilToJune2020.csv") %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, original_text)

my_dictionary <- data %>% 
  select(original_text) %>% 
  unnest_tokens(word, original_text) %>% 
  anti_join(full_stop_words) %>%
  distinct(word) %>% 
  pull(word)

more_stop_words <- tribble(
  ~word, ~lexicon,
  "covid19", "CUSTOM",
  "coronavirus", "CUSTOM",
  "virus", "CUSTOM",
  "rt", "CUSTOM",
  "covid", "CUSTOM",
  "19", "CUSTOM",
  "florida", "CUSTOM",
  "coronaviru", "CUSTOM",
  "http", "CUSTOM"
)

full_stop_words <- stop_words %>% 
  bind_rows(more_stop_words)

# Covid Tweet Data from April to June of 2020
tweets1 <- read.csv("COVIDTweetsAprilToJune2020.csv") %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, clean_tweet)

# tidy_tweets1 <- tweets1 %>% 
#   unnest_tokens(word, clean_tweet) %>% 
#   anti_join(full_stop_words) %>% 
#   count(id, word) %>% 
#   filter(n > 10)

SCtest2 <- tweets1 %>% 
  unnest_tokens(word, clean_tweet) %>% 
  anti_join(full_stop_words) %>%
  mutate(
    word = unlist(lapply(word, stemCompletion, dictionary = my_dictionary))
  ) %>% 
  count(id, word) %>% 
  filter(n > 10)

tidy_tweets1 <- SCtest2 %>% 
  drop_na() %>% 
  anti_join(full_stop_words)

tweets1_dtm <- tidy_tweets1 %>% 
  cast_dtm(id, word, n)


# k = 20 ------------------------------------------------------------------

twenty <- LDA(
  tweets1_dtm,
  k = 20, 
  method = "Gibbs",
  control = list(seed = 67)
)

glimpse(twenty)

twenty <- twenty %>% 
  tidy(matrix = "beta")

twenty <- twenty %>% 
  arrange(desc(beta)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  mutate(term2 = fct_reorder(term, beta))

ggplot(twenty, aes(term2, beta, fill = as.factor(topic))) + geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + coord_flip()

# k = 25 ------------------------------------------------------------------

twentyfive <- LDA(
  tweets1_dtm,
  k = 25, 
  method = "Gibbs",
  control = list(seed = 67)
)

glimpse(twentyfive)

twentyfive <- twentyfive %>% 
  tidy(matrix = "beta")

twentyfive <- twentyfive %>% 
  arrange(desc(beta)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  mutate(term2 = fct_reorder(term, beta))

ggplot(twentyfive, aes(term2, beta, fill = as.factor(topic))) + geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + coord_flip()

# k = 30 ------------------------------------------------------------------

thirty <- LDA(
  tweets1_dtm,
  k = 30, 
  method = "Gibbs",
  control = list(seed = 67)
)

glimpse(thirty)

thirty <- thirty %>% 
  tidy(matrix = "beta")

thirty <- thirty %>% 
  arrange(desc(beta)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  mutate(term2 = fct_reorder(term, beta))

ggplot(thirty, aes(term2, beta, fill = as.factor(topic))) + geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + coord_flip()

# k = 15 ------------------------------------------------------------------

fifteen <- LDA(
  tweets1_dtm,
  k = 15, 
  method = "Gibbs",
  control = list(seed = 67)
)

glimpse(fifteen)

fifteen <- fifteen %>% 
  tidy(matrix = "beta")

fifteen <- fifteen %>% 
  arrange(desc(beta)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  mutate(term2 = fct_reorder(term, beta))

ggplot(fifteen, aes(term2, beta, fill = as.factor(topic))) + geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + coord_flip()
















# Covid Tweet Data from August to September of 2020
tweets2 <- read.csv("COVIDTweetsAugustToSeptember2020.csv")

# Covid Tweet Data from April to June of 2021
tweets3 <- read.csv("COVIDTweetsAprilToJune2021.csv")