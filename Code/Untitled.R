# Load packages
library(tm)
library(SnowballC)
library(RColorBrewer)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)


# 1. Read in data sets 
depression_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_depression_tweets.csv')
fatigue_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_covidFatigue_tweets.csv')

# 2.1 Create a corpus
depression_corpus <-Corpus(VectorSource(depression_tweets$clean_text))
depression_corpus [[4]][1]
fatigue_corpus <-Corpus(VectorSource(fatigue_tweets$clean_text))

# 2.2 Funtion to clean the corpus 
clean_corpus <- function(corp) {
      corp <- tm_map(corp, removeWords, stopwords("english"))
      corp <- tm_map(corp, stemDocument)
}

depression_corpus <- clean_corpus(depression_corpus)
fatigue_corpus <- clean_corpus(fatigue_corpus)

# 3.1 Create a term document matrix
frequencies <- TermDocumentMatrix(depression_corpus)
fatigue_freq <- TermDocumentMatrix(fatigue_corpus)
      
# Inspect the TDM
inspect(frequencies[505:515, 1000:1005])
inspect(fatigue_freq[105:115, 100:105])

#3.2 What are the frequent terms?
findFreqTerms(frequencies, lowfreq = 300)
findFreqTerms(fatigue_freq, lowfreq = 300)

#3.3 What words are associated with depression?
findAssocs(frequencies, c('depression', 'depress'), corlimit = .4)
findAssocs(fatigue_freq, c('pandemicfatigu', 'covidfatigu'), corlimit = .1)



#3.4 Create a matrix from TDM 
freq.matrix <- as.matrix(frequencies)
term.freq <- sort(rowSums(freq.matrix), decreasing = TRUE)
head(term.freq)


fatique.matrix <- as.matrix(fatigue_freq)
fatigue.term.freq <- sort(rowSums(fatique.matrix), decreasing = TRUE)
head(fatigue.term.freq)

#3.5 Create a bar plot of words with frequencies > 400 and < 900
term.df <- data.frame(term=names(term.freq), freq=term.freq)
str(term.df)

plot <- ggplot(subset(term.df, term.df$freq > 400 & term.df$freq < 900), aes(term, freq, fill=freq)) + geom_bar(stat='identity') + labs(x='Terms', y='Count', title='Term Frequencies') 
plot + coord_flip()


fatigue.term.df <- data.frame(term=names(fatigue.term.freq), freq=fatigue.term.freq)
str(fatigue.term.df)

plot <- ggplot(subset(fatigue.term.df, fatigue.term.df$freq > 25 & fatigue.term.df$freq < 200), aes( x= reorder(term,freq), y= freq, fill=freq)) + geom_bar(stat='identity') + labs(x='Terms', y='Count', title='Term Frequencies') 
plot + coord_flip()

# 4.1 Create another DTM 
depression_dtm <- DocumentTermMatrix(depression_corpus)

#4.2 Remove Sparse terms (terms that only appear in less than .5% of tweets)
sparseData <- removeSparseTerms(depression_dtm, sparse = .995)
sparseData

# 4.3 convert to data frame
sparsedf <- as.data.frame(as.matrix(sparseData))

head(sparsedf[,1:6])

#4.4 Add the dependent variable 
sparsedf$tag <- 'depression'

#4.5 Split the Data
set.seed(1)
split <- sample.split(sparsedf$tag, SplitRatio = 0.7)
trainSparse <- subset(sparsedf, split==TRUE)
testSparse <- subset(sparsedf, split==FALSE)

# 5.1 Create a Baseline Model
tweet.CART <- rpart(tag~., trainSparse, method='class')

# Plot Model
prp(tweet.CART)


