library(quanteda)
library(readtext)
require(stringr)
require(tidytext)
require(tokenizers)
require(plyr)
library(dplyr)

library(SnowballC)
library(tm)
library(wordcloud)
library(RColorBrewer)

###### read in tweets datasets ######
depression_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_depression_tweets.csv')
fatigue_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_covidFatigue_tweets.csv')

###### top occurring words pre-stemming ######
depression_tweets %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)

fatigue_tweets %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)

###### Add custom stop words ######
custom <- add_row(stop_words, word = "https", lexicon = "custom")
custom <- add_row(custom, word = "http", lexicon = "custom")
custom <- add_row(custom, word = "t.co", lexicon = "custom")
#custom <- add_row(custom, word = "amp", lexicon = "custom")

###### Stemming ######
stemmed_d_tweets <- depression_tweets %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(custom) %>%
      mutate(word = wordStem(word))

stemmed_f_tweets <- fatigue_tweets %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(custom) %>%
      mutate(word = wordStem(word))

###### Most occurring words after stemming ###### 
stemmed_d_tweets %>% 
      group_by(word) %>%
      count(word, sort = TRUE)

stemmed_f_tweets %>% 
      group_by(word) %>%
      count(word, sort = TRUE)

###### Visualizations ######
depressed <- Corpus(VectorSource(depression_tweets$clean_text))
depressed <- tm_map(depressed, removeWords, stopwords("english"))
depressed <- tm_map(depressed, removeWords, c("can")) 
depressed <- tm_map(depressed, stemDocument)

dtm <- TermDocumentMatrix(depressed)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# depression word cloud w/the top 5 stems
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdBu"))

# depression word cloud w/o the top 5 stems
wordcloud(words = d$word[-(1:5)], freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))


fatigued <- Corpus(VectorSource(fatigue_tweets$clean_text))
fatigued <- tm_map(fatigued, removeWords, stopwords("english"))
fatigued <- tm_map(fatigued, removeWords, c("can")) 

Fdtm <- TermDocumentMatrix(fatigued)
Fm <- as.matrix(Fdtm)
Fv <- sort(rowSums(Fm),decreasing=TRUE)
Fd <- data.frame(word = names(Fv),freq=Fv)
head(Fd, 10)

#fatigue word cloud w/the top 5 word
wordcloud(words = Fd$word, freq = Fd$freq, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "RdBu"), scale=c(2,.75))

# fatigue word cloud w/o the top 5 word
wordcloud(words = Fd$word[-(1:5)], freq = Fd$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))

###### Average words per tweet ######
mean(depression_tweets$word_count, na.rm = TRUE)  
mean(fatigue_tweets$word_count, na.rm = TRUE)  

###### Emojis ######
mean(depression_tweets$emoji_count, na.rm = TRUE)  
mean(fatigue_tweets$emoji_count, na.rm = TRUE)  

