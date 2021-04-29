# Read in data
depression <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_depression_tweets.csv', header = TRUE)
depression$label = 'depression'
set.seed(1234)
depression_tweets <- depression[sample(nrow(depression), 1626), ]
fatigue_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_covidFatigue_tweets.csv')
fatigue_tweets$label = 'covid fatigue'

all_data <- rbind(depression_tweets, fatigue_tweets)
all_data$label <- factor(all_data$label, labels = c('covid_fatigue','depression'))


###### Extract text features to be used later ######
library(textfeatures)
text <- all_data$tweet_text
features <- textfeatures(text, verbose = FALSE, normalize = FALSE)
all_data$n_first_person <- features$n_first_person
all_data$n_uq_words <- features$n_uq_chars
all_data$n_charsperword <- features$n_charsperword
all_data$n_prepositions <- features$n_prepositions


###### Split and Save data sets ######
all_data$clean_text <- as.character(all_data$clean_text)
all_data_split <- initial_split(data = all_data, strata = label, prop = .8)
all_data_split
data_train <- training(all_data_split)
data_test <- testing(all_data_split)

write.csv(data_train,"/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/trainingSet.csv")
write.csv(data_test,"/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/testingSet.csv")

