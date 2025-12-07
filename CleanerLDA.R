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
  "port", "CUSTOM",
  "due", "CUSTOM",
  "stainless", "CUSTOM",
  "include", "CUSTOM",
  "video", "CUSTOM",
  "dyannleroy", "CUTSOM",
  "ten", "CUSTOM",
  "million", "CUSTOM",
  "di", "CUSTOM",
  "pm", "CUSTOM",
  "dianeswonk", "CUSTOM",
  "residency", "CUSTOM"
)

full_stop_words <- stop_words %>% 
  bind_rows(more_stop_words)

# April to June of 2020 ---------------------------------------------------

  data2020 <- read.csv("COVIDTweetsAprilToJune2020.csv")
  
  original_2020 <- data2020 %>% 
    filter(str_detect(place, "Florida|FL")) %>% 
    select(id, original_text) %>% 
    mutate(id = row_number()) # Reassign unique ids because the scientific notation made documents appears to be the same docs/source.
  
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
  # This dictionary will be used to stem-complete the words after I stem them.
  
  stem2020 <- clean2020 %>% 
    mutate(word = wordStem(word)) %>% 
    mutate(
      word = unlist(lapply(word, stemCompletion, dictionary = dictionary_2020))
    )
  # The words did not 100% stem the way I had hoped, so I manually fixed some of them after searching/filtering through the original tweets to find the best match if needed. I repeat these same words for manual stem completion in the 2021 data processing (next) too. Still, stem completion saved a lot of time and cleaned up the data well enough.
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
                         "announc" = "announce",
                         "updat" = "update")) %>% 
    count(id, word)
  
  # LDA Modeling
  dtm2020 <- words_2020 %>% 
    cast_dtm(id, word, n)
  
  lda2020_k9 = LDA(
    dtm2020,
    k = 9, # After many trial and errors, I found this k value to be the most humanly interpret-able for the 2020 covid data.
    method = "Gibbs",
    control = list(seed = 67)
  )
  
  glimpse(lda2020_k9)
  
  beta_2020_k9 <- lda2020_k9 %>% 
    tidy(matrix = "beta") # Beta to find out the words within a topic to analyze them.
  
  beta_2020_k9 <- beta_2020_k9 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>%
    slice_max(beta, n = 8) %>% # 8 words at a time in a column chart, but if any beta values tie, they will both (or more) show, which is fine.
    mutate(term2 = fct_reorder(term, beta))
  
  ggplot(beta_2020_k9, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") + # free_y scale so that the beta ticks change in the same increments across plots.
    coord_flip() +
    labs(title = "April - June 2020 Topics",
         x = "Words",
         y = "Beta")
  
  # Labeling human-identified topics using k = 9
  beta_2020_k9 <- beta_2020_k9 %>% 
    mutate(
      topic = as.character(topic),
      topic = recode(topic,
                     "1" = "Severity Reports",
                     "2" = "Trump Response",
                     "3" = "Public Health Reports",
                     "4" = "Global Supplies",
                     "5" = "Vaccine Development",
                     "6" = "Early Reopening Effects",
                     "7" = "Government Action",
                     "8" = "Virus Exposure to Workers",
                     "9" = "Closing of American Public Spaces"))
  
  # Re-plot w/ topic titles
  ggplot(beta_2020_k9, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") + 
    coord_flip() +
    labs(title = "April - June 2020 Topics",
         x = NULL,
         y = "Beta") + theme_bw()
  
  # gamma to find most dominant/prevalent topic during 2020
  gamma_2020_k16 <- lda2020_k9 %>% 
    tidy(matrix = "gamma") %>%
    mutate(
      topic = recode(topic,
                     "1" = "Severity Reports",
                     "2" = "Trump Response",
                     "3" = "Public Health Reports",
                     "4" = "Global Supplies",
                     "5" = "Vaccine Development",
                     "6" = "Early Reopening Effects",
                     "7" = "Government Action",
                     "8" = "Virus Exposure to Workers",
                     "9" = "Closing of American Public Spaces"))
  
  dominant_topic_2020 <- gamma_2020_k16 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1, with_ties = FALSE) %>%
    ungroup() # finding the best fit topic per document
  
  topic_counts_2020 <- dominant_topic_2020 %>% 
    count(topic, sort = TRUE) %>% 
    mutate(topic = reorder(topic, n)) # adding up documents per topic
  
  # Bar charts > Pie charts
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
    ) + theme_bw()
  
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
                         "communi" = "community",
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
                         "announc" = "announce",
                         "updat" = "update",
                         "requir" = "require")) %>% 
    count(id, word)
  
  # LDA Modeling
  dtm2021 <- words_2021 %>% 
    cast_dtm(id, word, n)
  
  lda2021_k10 = LDA(
    dtm2021,
    k = 8, 
    method = "Gibbs",
    control = list(seed = 10)
  )
  
  glimpse(lda2021_k10)
  
  beta_2021_k10 <- lda2021_k10 %>% 
    tidy(matrix = "beta")
  
  beta_2021_k10 <- beta_2021_k10 %>% 
    arrange(desc(beta)) %>% 
    group_by(topic) %>%
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
    mutate(
      topic = as.character(topic),
      topic = recode(topic,
                     "1" = "Investigating Covid Origins",
                     "2" = "Global Vaccine Rollout",
                     "3" = "Variants and Lab Findings",
                     "4" = "Research Updates",
                     "5" = "Administrating Vaccines",
                     "6" = "Student Vaccination",
                     "7" = "National Covid Death Reports",
                     "8" = "Vaccine Approval"))
  
  # # Re-plot w/ topic titles
  ggplot(beta_2021_k10, aes(term2, beta, fill = as.factor(topic))) + 
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free_y") + 
    coord_flip() +
    labs(
      title = "April - June 2021 Topics",
      x = NULL,
      y = "Beta"
    ) + theme_bw()
  
  # gamma to find most dominant/prevalent topic during 2020
  gamma_2021_k10 <- lda2021_k10 %>% 
    tidy(matrix = "gamma") %>% 
    mutate(
    topic = recode(topic,
                   "1" = "Investigating Covid Origins",
                   "2" = "Global Vaccine Rollout",
                   "3" = "Variants and Lab Findings",
                   "4" = "Research Updates",
                   "5" = "Administrating Vaccines",
                   "6" = "Student Vaccination",
                   "7" = "National Covid Death Reports",
                   "8" = "Vaccine Approval"))
  
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
    ) + theme_bw()
  