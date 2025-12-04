# Comparing topics discussed in 4-6/20 vs 4-6/21
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(qdap)
library(tm)
library(SnowballC)

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
  "http", "CUSTOM",
  "covidnineteen", "CUSTOM",
  "don’t", "CUSTOM",
  "it’s", "CUSTOM"
)

full_stop_words <- stop_words %>% 
  bind_rows(more_stop_words)

# April to June of 2020 ---------------------------------------------------

  data2020 <- read.csv("COVIDTweetsAprilToJune2020.csv")
  
  original_2020 <- data2020 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, original_text)
  
  # cleaning original tweets FROM SCRATCH
  clean2020 <- original_2020 %>% 
    mutate(original_text = as.character(original_text)) %>% 
    mutate(original_text = replace_contraction(original_text)) %>% 
    mutate(original_text = replace_number(original_text)) %>% 
    mutate(original_text = str_remove_all(original_text, "https?://.*$")) %>% 
    mutate(original_text = str_remove_all(original_text, "RT ")) %>% 
    mutate(original_text = str_remove_all(original_text, "@\\w+")) %>% 
    mutate(original_text = removePunctuation(original_text)) %>%
    mutate(original_text = str_remove_all(original_text, "'")) %>% 
    mutate(original_text = removeNumbers(original_text)) %>% 
    unnest_tokens(word, original_text) %>% 
    anti_join(full_stop_words)

  dictionary_2020 <- original_2020 %>% 
    select(original_text) %>% 
    unnest_tokens(word, original_text) %>% 
    anti_join(full_stop_words) %>%
    distinct(word) %>% 
    pull(word)
  
  stem2020 <- clean2020 %>% 
    mutate(word = wordStem(word)) %>% 
    mutate(
      word = unlist(lapply(word, stemCompletion, dictionary = dictionary_2020))
    )
  
  words_2020 <- stem2020 %>% 
    drop_na() %>% 
    anti_join(full_stop_words) %>% 
    mutate(word = recode(word,
                         "viru" = "virus",
                         "informat" = "information",
                         "posit" = "positive",
                         "mani" = "humanity",
                         "commun" = "community",
                         "preside" = "president", 
                         "respons" = "response",
                         "raise" = "fundraise",
                         "busin" = "business",
                         "hospita" = "hospital")) %>% 
    count(id, word) %>% 
    filter(n > 10)
  
  # LDA Modeling
  dtm2020 <- words_2020 %>% 
    cast_dtm(id, word, n)
  
  lda2020_k15 = LDA(
    dtm2020,
    k = 12, 
    method = "Gibbs",
    control = list(seed = 67)
  )
  
  glimpse(lda2020_k15)
  
  lda2020_k15 <- lda2020_k15 %>% 
    tidy(matrix = "beta")
  
  lda2020_k15 <- lda2020_k15 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>%
    slice_max(beta, n = 8) %>% 
    mutate(term2 = fct_reorder(term, beta))
  
  ggplot(lda2020_k15, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) + scale_y_continuous(breaks = seq(0, 1, by = 0.05)) + 
    facet_wrap(~topic, scales = "free") + 
    coord_flip()

# April to June of 2021 ---------------------------------------------------

data2021 <- read.csv("COVIDTweetsAprilToJune2021.csv")

original_2021 <- data2021 %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, original_text)

cleaned_2021 <- data2021 %>% 
  filter(str_detect(place, "Florida|FL")) %>% 
  select(id, clean_tweet)
