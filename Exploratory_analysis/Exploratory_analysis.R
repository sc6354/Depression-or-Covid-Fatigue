library(quanteda)
library(readtext)
require(stringr)
require(tidytext)
require(tokenizers)
require(plyr)
library(dplyr)
library(SnowballC)

###### read in tweets datasets ######
depression_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210312_0330_cleaned_depression_tweets.csv')
fatigue_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210312_0330_cleaned_covidFatigue_tweets.csv')

###### top occurring words pre-stemming and no custom stop words ######
depression_tweets %>%
      unnest_tokens(word, tweet_text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)

###### Add custom stop words ######
custom <- add_row(stop_words, word = "https", lexicon = "custom")
custom <- add_row(custom, word = "http", lexicon = "custom")
custom <- add_row(custom, word = "t.co", lexicon = "custom")
#custom <- add_row(custom, word = "amp", lexicon = "custom")

# top occurring words with custom stop words
word_counts <- depression_tweets %>%
      unnest_tokens(word, tweet_text) %>%
      anti_join(custom) %>%
      count(word, sort = TRUE)

###### Stemming ######
stemmed_d_tweets <- depression_tweets %>%
      unnest_tokens(word, tweet_text) %>%
      anti_join(custom) %>%
      mutate(word = wordStem(word))

###### Most occurring words after stemming ###### 
stemmed_d_tweets %>% 
      count(word, sort = TRUE)
