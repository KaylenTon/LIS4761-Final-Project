# Comparing topics discussed in 4-6/20 vs 4-6/21
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)

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

# April to June of 2020 ---------------------------------------------------

  data2020 <- read.csv("COVIDTweetsAprilToJune2020.csv")
  
  original_2020 <- data2020 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, original_text)
  
  dictionary_2020 <- original_2020 %>% 
    select(original_text) %>% 
    unnest_tokens(word, original_text) %>% 
    anti_join(full_stop_words) %>%
    distinct(word) %>% 
    pull(word)
  
  cleaned_2020 <- data2020 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, clean_tweet)
  
  stemCompete_2020 <- cleaned_2020 %>% 
    unnest_tokens(word, clean_tweet) %>% 
    anti_join(full_stop_words) %>%
    mutate(
      word = unlist(lapply(word, stemCompletion, dictionary = dictionary_2020))
    ) %>% 
    count(id, word) %>% 
    filter(n > 10)
  
  words_2020 <- stemCompete_2020 %>% 
    drop_na() %>% 
    anti_join(full_stop_words)
  
  # LDA Modeling
  dtm2020 <- words_2020 %>% 
    cast_dtm(id, word, n)
  
  lda2020_k15 = LDA(
    tweets1_dtm,
    k = 15, 
    method = "Gibbs",
    control = list(seed = 67)
  )
  
  glimpse(lda2020_k15)
  
  lda2020_k15 <- lda2020_k15 %>% 
    tidy(matrix = "beta")
  
  lda2020_k15 <- lda2020_k15 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 10) %>% 
    mutate(term2 = fct_reorder(term, beta))
  
  ggplot(lda2020_k15, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) + facet_wrap(~topic, scales = "free") + 
    coord_flip()

# April to June of 2021 ---------------------------------------------------

data2021 <- read.csv("COVIDTweetsAprilToJune2021.csv")

original_2021 <- data2021 %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, original_text)

cleaned_2021 <- data2021 %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, clean_tweet)
