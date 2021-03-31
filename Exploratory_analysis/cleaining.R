library(readtext)
require(stringr)
require(tidytext)
require(tokenizers)
require(plyr)
library(dplyr)

###### read in tweets datasets ######
depression_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210312_0330_all_depression_tweets.csv')
colnames(depression_tweets)[2] <- "created_at"
fatigue_tweets <- readtext('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210317_0330all_covid_tweets.csv')
colnames(fatigue_tweets)[2] <- "created_at"

####### Replace common characters on Twitter with their ASCII equivalents #######
d_tweets <- depression_tweets[!duplicated(depression_tweets$tweet_text),]
f_tweets <- fatigue_tweets[!duplicated(fatigue_tweets$tweet_text),]

pattern <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
replacement <- c('e', '...', '-', "'", '~', '＞', '+', 'and')

d_tweets <- d_tweets[, c('doc_id','username', 'tweet_text')]
d_tweets$tweet_text <- qdap::mgsub(pattern= pattern, replacement = replacement, d_tweets$tweet_text)

f_tweets <- f_tweets[, c('doc_id','username', 'tweet_text')]
f_tweets$tweet_text <- qdap::mgsub(pattern= pattern, replacement = replacement, f_tweets$tweet_text)

####### Remove html symbols #######
d_tweets$tweet_text <- str_replace_all(d_tweets$tweet_text, '&[a-z]{1,6};', '' )
f_tweets$tweet_text <- str_replace_all(f_tweets$tweet_text, '&[a-z]{1,6};', '' )

####### Remove emojis and save their counts ######
emojis <- str_extract_all(d_tweets$tweet_text,'[^[:alnum:][:punct:][:space:][\\$\\~\\=\\-\\|\\*]]+')
emojis2 <- str_extract_all(f_tweets$tweet_text,'[^[:alnum:][:punct:][:space:][\\$\\~\\=\\-\\|\\*]]+')

d_tweets$emojis <- sapply(emojis, function(x) paste(x, collapse =','))
d_tweets$emoji_count <- sapply(emojis, function(x) sum(str_length(x)))

f_tweets$emojis <- sapply(emojis2, function(x) paste(x, collapse =','))
f_tweets$emoji_count <- sapply(emojis2, function(x) sum(str_length(x)))

rm(emojis)
rm(emojis2)

d_tweets$tweet_text <- iconv(d_tweets$tweet_text, 'UTF-8', 'ASCII', '')
f_tweets$tweet_text <- iconv(f_tweets$tweet_text, 'UTF-8', 'ASCII', '')

###### Remove links ######
d_tweets$tweet_text <- str_replace_all(d_tweets$tweet_text, 'https://t.co/[a-zA-Z0-9]*', '')
f_tweets$tweet_text <- str_replace_all(f_tweets$tweet_text, 'https://t.co/[a-zA-Z0-9]*', '')


###### Remove trailing white spaces ######
d_tweets$tweet_text <- str_replace_all(d_tweets$tweet_text , '( )+', ' ')
d_tweets$tweet_text <- str_trim(d_tweets$tweet_text )
f_tweets$tweet_text <- str_replace_all(f_tweets$tweet_text , '( )+', ' ')
f_tweets$tweet_text <- str_trim(f_tweets$tweet_text )

###### Tokenization ######
words <- d_tweets %>%
      unnest_tokens(word, tweet_text)

words$word <- tolower(words$word)
words <- words[!grepl('#', words$word), ]
words <- words[!grepl('@', words$word), ]

clean_d_tweets <- words %>%
      group_by(doc_id, username) %>%
      dplyr::summarize(clean_text = paste(word, collapse = ' '), word_count = n()) %>%
      as.data.frame()

fatigue_words <- f_tweets %>%
      unnest_tokens(word, tweet_text)

fatigue_words$word <- tolower(fatigue_words$word)
fatigue_words <- fatigue_words[!grepl('#', fatigue_words), ]
fatigue_words <- fatigue_words[!grepl('@', fatigue_words), ]

clean_f_tweets <- fatigue_words  %>%
      group_by(doc_id, username) %>%
      dplyr::summarize(clean_text = paste(word, collapse = ' '), word_count = n()) %>%
      as.data.frame()

###### Join the cleaned tweets with the d_tweets dataframe
final <- join(d_tweets, clean_d_tweets, type = 'inner', by = 'doc_id')
final <- join(depression_tweets, final, type = 'inner', by = 'doc_id')

fatigue <- join(f_tweets, clean_f_tweets, type = 'inner', by = 'doc_id')
fatigue <- join(fatigue_tweets, fatigue, type = 'inner', by = 'doc_id')

###### Export to CSV ######
write.csv(final, '/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210312_0330_cleaned_depression_tweets.csv', row.names =  TRUE)
write.csv(fatigue, '/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/20210312_0330_cleaned_covidFatigue_tweets.csv', row.names =  TRUE)






