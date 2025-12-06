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
  "rt", "CUSTOM",
  "covid", "CUSTOM",
  "19", "CUSTOM",
  "florida", "CUSTOM",
  "coronaviru", "CUSTOM",
  "http", "CUSTOM",
  "covidnineteen", "CUSTOM",
  "don’t", "CUSTOM",
  "it’s", "CUSTOM",
  "hundreds", "CUSTOM",
  "thousands", "CUSTOM",
  "millions", "CUSTOM",
  "thousandth", "CUSTOM",
  "fauci", "CUSTOM",
  "amp", "CUSTOM",
  "port", "CUSTOM"
)

full_stop_words <- stop_words %>% 
  bind_rows(more_stop_words)

# April to June of 2020 ---------------------------------------------------

  data2020 <- read.csv("COVIDTweetsAprilToJune2020.csv")
  
  original_2020 <- data2020 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, original_text) %>% 
    mutate(id = row_number())
  
  # cleaning 2020 original tweets FROM SCRATCH
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
                         "hospita" = "hospital",
                         "vaccin" = "vaccine",
                         "accounc" = "announce",
                         "countie" = "county",
                         "populati" = "population",
                         "emerge" = "emergency",
                         "announc" = "announce")) %>% 
    count(id, word)
  
  # LDA Modeling
  dtm2020 <- words_2020 %>% 
    cast_dtm(id, word, n)
  
  lda2020_k16 = LDA(
    dtm2020,
    k = 16, 
    method = "Gibbs",
    control = list(seed = 67)
  )
  
  glimpse(lda2020_k16)
  
  beta_2020_k16 <- lda2020_k16 %>% 
    tidy(matrix = "beta")
  
  beta_2020_k16 <- beta_2020_k16 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>%
    slice_max(beta, n = 8) %>% 
    mutate(term2 = fct_reorder(term, beta))
  
  ggplot(beta_2020_k16, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") +
    coord_flip() +
    labs(title = "April - June 2020 Topics",
         x = "Words",
         y = "Beta")
  
  # Labeling human-identified topics using k = 16
  beta_2020_k16 <- beta_2020_k16 %>% 
    filter(topic %in% 1:12) %>% 
    mutate(
      topic = as.character(topic),
      topic = recode(topic,
                     "1" = "American Healthcare",
                     "2" = "Government Response",
                     "3" = "Mortality & Global Severity",
                     "4" = "Daily Reports on Covid Numbers",
                     "5" = "Community Support",
                     "6" = "Mask Usage",
                     "7" = "Public Media Criticism",
                     "8" = "Vaccine Advancement Progress",
                     "9" = "Economic Crisis",
                     "10" = "Trump and Stimulus Checks",
                     "11" = "CDC Virus Information",
                     "12" = "Quarantine",
                     "13" = "Global Situation",
                     "14" = "Reopening the Workplace",
                     "15" = "Reopening Impact",
                     "16" = "Strain on Hospitals & Healthcare"))
  
  # Re-plot w/ topic titles
  ggplot(beta_2020_k16, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") +
    coord_flip() +
    labs(title = "April - June 2020 Topics",
         x = "Words",
         y = "Beta")
  
  # gamma to find most dominant/prevalent topic during 2020
  gamma_2020_k16 <- lda2020_k16 %>% 
    tidy(matrix = "gamma") %>% 
    mutate(
      topic = recode(topic,
                     "1" = "American Healthcare",
                     "2" = "Government Response",
                     "3" = "Mortality & Global Severity",
                     "4" = "Daily Reports on Covid Numbers",
                     "5" = "Community Support",
                     "6" = "Mask Usage",
                     "7" = "Public Media Criticism",
                     "8" = "Vaccine Advancement Progress",
                     "9" = "Economic Crisis",
                     "10" = "Trump and Stimulus Checks",
                     "11" = "CDC Virus Information",
                     "12" = "Quarantine",
                     "13" = "Global Situation",
                     "14" = "Reopening the Workplace",
                     "15" = "Reopening Impact",
                     "16" = "Strain on Hospitals & Healthcare"))
  
  dominant_topic_2020 <- gamma_2020_k16 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  topic_counts_2020 <- dominant_topic_2020 %>%
    count(topic, sort = TRUE) %>% 
    mutate(topic = reorder(topic, n))
  
  ggplot(topic_counts_2020, aes(x = topic, y = n, fill = topic)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = n)) +
    coord_flip() +
    labs(
      title = "The Biggest Themes of April - June 2020",
      subtitle = "Relating to Covid-19",
      fill = "Themes",
      x = NULL,
      y = "Tweets"
    )
  
# April to June of 2021 ---------------------------------------------------

  data2021 <- read.csv("COVIDTweetsAprilToJune2021.csv")
  
  original_2021 <- data2021 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, original_text) %>% 
    mutate(id = row_number())
  
  # cleaning 2021 original tweets FROM SCRATCH
  clean2021 <- original_2021 %>% 
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
  
  dictionary_2021 <- original_2021 %>% 
    select(original_text) %>% 
    unnest_tokens(word, original_text) %>% 
    anti_join(full_stop_words) %>%
    distinct(word) %>% 
    pull(word)
  
  stem2021 <- clean2021 %>% 
    mutate(word = wordStem(word)) %>% 
    mutate(
      word = unlist(lapply(word, stemCompletion, dictionary = dictionary_2021))
    )
  
  words_2021 <- stem2021 %>% 
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
                         "hospita" = "hospital",
                         "vaccin" = "vaccine",
                         "accounc" = "announce",
                         "countie" = "county",
                         "populati" = "population",
                         "emerge" = "emergency",
                         "announc" = "announce")) %>% 
    count(id, word)
  
  # LDA Modeling
  dtm2021 <- words_2021 %>% 
    cast_dtm(id, word, n)
  
  lda2021_k10 = LDA(
    dtm2021,
    k = 16, 
    method = "Gibbs",
    control = list(seed = 67)
  )
  
  glimpse(lda2021_k10)
  
  beta_2021_k10 <- lda2021_k10 %>% 
    tidy(matrix = "beta")
  
  beta_2021_k10 <- beta_2021_k10 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>%
    filter(beta >= .0125) %>% 
    slice_max(beta, n = 8) %>% 
    mutate(term2 = fct_reorder(term, beta))
  
  ggplot(beta_2021_k10, aes(term, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") + 
    coord_flip() +
    labs(
      title = "April - June 2021 Topics",
      x = "Words",
      y = "Beta"
    )
  
  # Labeling human-identified topics using k = 10
  beta_2021_k10 <- beta_2021_k10 %>% 
    filter(topic %in% 1:12) %>% 
    mutate(
      topic = as.character(topic),
      topic = recode(topic,
                     "1" = "Vaccine Announcements & Expert Advice",
                     "2" = "Vaccine Distribution & County-level Reporting",
                     "3" = "Negative Effects of the Pandemic",
                     "4" = "Vaccine Transmission Messageing",
                     "5" = "Market & Economy",
                     "6" = "Vaccine & Public Health Measures",
                     "7" = "Political Framing of Vaccines",
                     "8" = "Vaccine Approval",
                     "9" = "Health Reports",
                     "10" = "State of Covid Vaccines in June"))
  
  # # Re-plot w/ topic titles
  ggplot(beta_2021_k10, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") + 
    coord_flip() +
    labs(
      title = "April - June 2021 Topics",
      x = "Words",
      y = "Beta"
    )
  
  # gamma to find most dominant/prevalent topic during 2020
  gamma_2021_k10 <- lda2021_k10 %>% 
    tidy(matrix = "gamma") %>% 
    mutate(
    topic = recode(topic,
                   "1" = "Vaccine Announcements & Expert Advice",
                   "2" = "Vaccine Distribution & County-level Reporting",
                   "3" = "Negative Effects of the Pandemic",
                   "4" = "Vaccine Transmission Messageing",
                   "5" = "Market & Economy",
                   "6" = "Vaccine & Public Health Measures",
                   "7" = "Political Framing of Vaccines",
                   "8" = "Vaccine Approval",
                   "9" = "Health Reports",
                   "10" = "State of Covid Vaccines in June"))
  
  dominant_topic_2021 <- gamma_2021_k10 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  topic_counts_2021 <- dominant_topic_2021 %>%
    count(topic, sort = TRUE) %>% 
    mutate(topic = reorder(topic, n))
  
  ggplot(topic_counts_2021, aes(x = topic, y = n, fill = topic)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = n)) +
    coord_flip() +
    labs(
      title = "The Biggest Themes of April - June 2021",
      subtitle = "Relating to Covid-19",
      fill = "Themes",
      x = NULL,
      y = "Tweets"
    )