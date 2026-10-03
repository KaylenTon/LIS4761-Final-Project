# LIS4761 Final Project — COVID-19 Twitter Analytics (Group 3)

A group project for LIS4761 analyzing COVID-19-related tweets from Florida across two windows — **April–June 2020** and **April–June 2021** — using topic modeling, exploratory/sentiment analysis, regression, and interactive Shiny dashboards.

This repo is the group submission; my individual contribution was the **LDA topic modeling** component, detailed below.

## Project Scope

The group pulled Twitter datasets filtered to Florida-origin tweets (plus a county-level Florida COVID-19 case dataset) and split the analysis across a few angles:

| Component | File(s) | Description |
|---|---|---|
| **LDA Topic Modeling** (mine) | [CleanerLDA.R](CleanerLDA.R), [LDA-Topic-Modeling.Rmd](LDA-Topic-Modeling.Rmd) / [.html](LDA-Topic-Modeling.html), [LDA.pdf](LDA.pdf) | Unsupervised topic discovery on 2020 vs. 2021 tweets to see how conversation themes shifted year-over-year. |
| Exploratory & Sentiment Analysis | [Covid 19 Group Final.R](Covid%2019%20Group%20Final.R) | `skimr` overview plus Bing lexicon sentiment scoring and monthly sentiment trends. |
| Linear Regression | [Covid Linear Regression](Covid%20Linear%20Regression) | Models `NewPercPos` against county-level variables (median age, monitoring status). |
| Shiny Dashboards | [ShinyApp](ShinyApp) (tweet sentiment explorer), [CovidDataShinyApp](CovidDataShinyApp) / [NewCovidShinyApp](NewCovidShinyApp) (two iterations of a county-level case metrics explorer) | Interactive dashboards for tweet sentiment and county-level case metrics. |
| Combined Write-up | [data/Group 3.Rmd](data/Group%203.Rmd) / [.html](data/Group-3.html), [Covid-19 Analytics Project Group 3.pptx](Covid-19%20Analytics%20Project%20Group%203.pptx) | Full group report and presentation. |

**Data** (in [data/](data/)): raw tweet CSVs for Apr–Jun 2020, Aug–Sep 2020, and Apr–Jun 2021, plus a partial Florida county-level COVID-19 case CSV.

> Note: of the four analytical components above, the LDA analysis is the one I'd stand behind as rigorous and well-documented — see below. The EDA/sentiment, regression, and dashboard pieces were built quickly by other group members and are included here for completeness, not as a reflection of my own work.

## My Contribution: LDA Topic Modeling

**Goal:** uncover what Floridians were actually tweeting about regarding COVID-19, and compare how those themes shifted between April–June 2020 (early pandemic/reopening) and April–June 2021 (vaccine rollout).

### Approach

1. **Cleaning** — stripped URLs, mentions, RT tags, punctuation, and numbers from raw tweet text; expanded contractions.
2. **Tokenizing & stop words** — `unnest_tokens()` to split into words, `anti_join()` against `tidytext`'s stop word list plus a custom list (e.g. `covid`, `coronavirus`, `rt`, `fauci`) that would otherwise dominate every topic.
3. **Stemming + stem completion** — `wordStem()` to reduce words to roots, then `stemCompletion()` against a per-year dictionary of distinct words to turn stems back into readable words. Some completions didn't resolve cleanly and were manually corrected with `mutate()` + `recode()`.
4. **Document-term matrix** — counted word frequency per tweet (`count(id, word)`) and cast to a DTM (`cast_dtm(id, word, n)`).
5. **LDA modeling** — fit with the `topicmodels` package, trying several values of *k* and keeping the model whose topics were most humanly interpretable.
6. **Interpretation** — used `tidy(matrix = "beta")` to pull top terms per topic (and label them by hand), and `tidy(matrix = "gamma")` to assign each tweet to its dominant topic, then counted tweet volume per topic to find the biggest themes each year.

### Findings

**April–June 2020** was dominated by:
- County Health Reports & COVID Testing
- Home Life & Pandemic Updates
- Trump Administration & Government Response

This tracks with the context at the time: Florida had just ended quarantine (April 30, 2020) and was moving through reopening phases, there was no vaccine yet, and daily case/death counts were the main source of news.

**April–June 2021** was dominated by vaccine-related themes:
- Vaccination Sites & Global Aid
- Health Plans & News
- DeSantis Prohibiting COVID Vaccine Mandates

This reflects the vaccine rollout (first vaccines arrived Dec 2020), the Biden administration joining COVAX for global vaccine distribution, and the growing political/social pushback around vaccine and mask mandates (including the Delta variant emerging as a concern).

**Takeaway:** the one-year gap shows a clear pivot from *"how bad is it and what do we do now"* (testing, reporting, early reopening) to *"vaccines — getting them, mandating them, or resisting them"* as the defining conversation of the pandemic.

Full topic breakdowns, beta/gamma charts, and slide-by-slide commentary are in [LDA.pdf](LDA.pdf).

### Libraries Used

`tidyverse`, `tidytext`, `qdap`, `tm`, `SnowballC`, `topicmodels`, `ggplot2`

## Running It

Open [LIS4761 Final Project.Rproj](LIS4761%20Final%20Project.Rproj) in RStudio so relative paths resolve correctly, then run [CleanerLDA.R](CleanerLDA.R) (expects the CSVs in [data/](data/)) or knit [LDA-Topic-Modeling.Rmd](LDA-Topic-Modeling.Rmd) for the full write-up with charts.
