require(quanteda)
library(tm)
require(topicmodels)
require(quanteda.textstats)
require(wordcloud2)



depression_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_depression_tweets.csv', header = TRUE)
fatigue_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_covidFatigue_tweets.csv')

## depression_tweets <- data.frame(depression_tweets)
depression_tweets$clean_text <- as.character(depression_tweets$clean_text)
depression_corpus <- corpus(depression_tweets$clean_text,
                            docnames = depression_tweets$X)

depression_tokens <- depression_corpus %>%
      tokens(remove_punct =TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = stopwords("en"), selection = "remove") 
 
depression_collocations <- textstat_collocations(depression_tokens, min_count =25)
      
depression_corpus_tokens <- tokens_compound(depression_tokens, depression_collocations)


## Model Calculation 
DTM <- depression_corpus_tokens %>%
      tokens_remove('') %>%
      dfm() %>%
      dfm_trim(min_docfreq = .01, max_docfreq =1, docfreq_type = 'prop')  

# the number of documents and terms in the matrix 
dim(DTM)

## remove the 10 most common terms 
textstat_frequency(DTM)[1:10]
top10_terms <- c('depression', 'mentalhealth', 'can', 'anxiety_depression', 'life', 'anxiety', 'just', 'depression_anxiety', 'people', 'help')
DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
depression_tweets <- depression_tweets[sel_idx,]

# number of topics 
K <- 5

topicModel <- LDA(DTM, K, method= 'Gibbs', control = list(iter=500, seed=1, verbose=25))

tmResult <- posterior(topicModel)

attributes(tmResult)

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=' ')

# visualize topics as Word Clouds 
topicToViz <- 5
top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <-names(top40terms)
prob <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, prob), shuffle = TRUE, size = .5)




################
fatigue_tweets$clean_text <- as.character(fatigue_tweets$clean_text)
fatigue_corpus <- corpus(fatigue_tweets$clean_text,
                            docnames = fatigue_tweets$X)

fatigue_tokens <- fatigue_corpus %>%
      tokens(remove_punct =TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = stopwords("en"), selection = "remove") 

fatigue_collocations <- textstat_collocations(fatigue_tokens, min_count =25)

fatigue_corpus_tokens <- tokens_compound(fatigue_tokens, fatigue_collocations)

## Model Calculation 
DTM2 <- fatigue_corpus_tokens %>%
      tokens_remove('') %>%
      dfm() %>%
      dfm_trim(min_docfreq = .01, max_docfreq =1, docfreq_type = 'prop')  

# the number of documents and terms in the matrix 
dim(DTM2)

## remove the 10 most common terms 
textstat_frequency(DTM2)[1:10]
top10_terms2 <- c('pandemicfatigue', 'covidfatigue', 'pandemic', 'year', 'covid', 'just', 'covid19', 'people', 'one', 'can')
DTM2 <- DTM2[, !(colnames(DTM2) %in% top10_terms2)]

sel_idx <- rowSums(DTM2) > 0
DTM2 <- DTM2[sel_idx, ]
fatigue_tweets <- fatigue_tweets[sel_idx,]


topicModel2 <- LDA(DTM2, K, method= 'Gibbs', control = list(iter=500, seed=1, verbose=25))

tmResult2 <- posterior(topicModel2)

attributes(tmResult2)

terms(topicModel2, 10)

top5termsPerTopic2 <- terms(topicModel2, 5)
topicNames2 <- apply(top5termsPerTopic2, 2, paste, collapse=' ')
