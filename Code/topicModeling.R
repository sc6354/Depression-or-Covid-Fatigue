require(quanteda)
require(topicmodels)
require(quanteda.textstats)
require(wordcloud2)
library(tidyverse)
library(tidymodels)
library(keras)
library(dplyr)
library(ggplot2)
library(purrr)
library(textrecipes)
library(recipes)
library(discrim)
library(workflows)
library(kernlab)
library(themis)
library(furrr)
library(glmnet)
library(naivebayes)


set.seed(1234)

depression_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_depression_tweets.csv', header = TRUE)
depression_tweets$label = 'depression'
depression_tweets <- depression_tweets[sample(nrow(depression_tweets), 650), ]
fatigue_tweets <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/cleaned_covidFatigue_tweets.csv')
fatigue_tweets$label = 'covid fatigue'

all_data <- rbind(depression_tweets, fatigue_tweets)
all_data$label <- factor(all_data$label, labels = c('covid_fatigue','depression'))

###### all data ######
all_data$clean_text <- as.character(all_data$clean_text)
all_data_split <- initial_split(data = all_data, strata = label, prop = .8)
all_data_split
data_train <- training(all_data_split)
data_test <- testing(all_data_split)


###### Functions to use later ######
# function to create tokens
create_tokens <- function(tweets) {
   corpus <- corpus(tweets)
   tokens <- corpus %>%
      tokens(remove_punct =TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = stopwords("en"), selection = "remove") 
   collocations <- textstat_collocations(tokens, min_count =25)
   tokens <- tokens_compound(tokens, collocations)
   return(tokens)
} 

# function to create Document Term Matrix 
create_DTM <- function (tokens) {
   DTM <- tokens %>%
      tokens_remove('') %>%
      dfm() %>%
      dfm_trim(min_docfreq = .01, max_docfreq =1, docfreq_type = 'prop')
   return(DTM)
}

# function to remove the 10 most common terms from DTM and data set 
remove_top10 <- function(dtm , d_set) {
   top10 <- textstat_frequency(dtm)[1:10]
   top10_terms <- list(top10$feature)
   dtm <- dtm[, !(colnames(dtm) %in% top10_terms)]
   sel_idx <- rowSums(dtm) > 0
   dtm <- dtm[sel_idx, ]
   data <- d_set[sel_idx,]
   return(list(dtm, data))
}

# function to visualize topic model terms as Word Clouds 
word_cloud <- function(topic_model, topic_num) {
   top40terms <- sort(topic_model$terms[topic_num, ], decreasing = TRUE)[1:40]
   words <-names(top40terms)
   prob <- sort(topic_model$terms[topic_num, ], decreasing = TRUE)[1:40]
   return(wordcloud2(data.frame(words, prob), shuffle = TRUE, size = .5))
}

# function to plot ROC curve 
plot_roc <- function (prediction, classifier_name){
   roc <- prediction %>%
      roc_curve(truth = label, `.pred_covid_fatigue`) %>%
      autoplot() +
      labs(
         color = NULL,
         title = paste("ROC curve for", classifier_name),
         subtitle = "Average curve across 10 folds")
   return(roc)
}

# function to plot confusion matrices
plot_confusion_matrix <- function(metric, classifier_name) {
   confusion_matrix <- as.data.frame(metric)
   ggplot(confusion_matrix, aes(x=Truth, y=Prediction, fill=Freq)) +
      geom_tile() + theme_bw() + coord_equal() +
      scale_fill_distiller(palette="Blues", direction=1) +
      guides(fill=F) + # removing legend for `fill`
      labs(title = paste("Confusion Matrix for", classifier_name))+ # using a title instead
      geom_text(aes(label=Freq), color="black") 
}


###### Training Set ###### 
train_tokens <- create_tokens(data_train$clean_text)
# the number of documents and terms in the matrix 
train_DTM <- create_DTM(train_tokens)
dim(train_DTM) #before removal
results <- remove_top10(train_DTM, data_train)
train_DTM <- results[[1]]
data_train <- results[[2]]
dim(train_DTM) #after removal 

###### Testing Set ###### 
test_tokens <- create_tokens(data_test$clean_text)
test_DTM <- create_DTM(test_tokens)
dim(test_DTM) 
results2 <- remove_top10(test_DTM, data_test)
test_DTM <- results2[[1]]
data_test <- results2[[2]]
dim(test_DTM) 


## Topic Modeling 
#### comparing topic models 
n_topics <- c(2:20)
LDA_compare <- n_topics %>%
   future_map(LDA, x = test_DTM, control = list(seed=TRUE))

tibble(
   k = n_topics,
   perplex = map_dbl(LDA_compare, perplexity)) %>%
   ggplot(aes(k, perplex)) +
   geom_point() +
   geom_line() +
   labs(
      title = "Evaluating LDA topic models",
      subtitle = "Optimal number of topics (a smaller perplexity is better)",
      x = "Number of topics",
      y = "Perplexity"
   )

##### Topic Model with the ideal k = 14 topics 
topicModel <- LDA(train_DTM, 14, method= 'Gibbs', control = list(iter=500, seed=1, verbose=25, alpha = .2))
tmResult <- posterior(topicModel)
terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=' ')
word_cloud(tmResult, 5)
   

###### Classifying tweets using topic model distribution as features
#N <- length(data_train$X)
#featureList <- vector("list", N)
#for(i in 1:N) {
 #  featureList[[i]] <- tmResult[['topics']][i,]}
#data_train$topic_prob <- featureList
#data_train$topic_prob[1:10]

df <- as.data.frame(tmResult[['topics']])
data_train <- cbind(data_train, new_col = df)  
colnames(data_train)[18:31] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                 'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                 'topic11', 'topic12', 'topic13', 'topic14')


### data recipe
data_rec <- recipe(label ~ topic1 + topic2 + topic3 + topic4 + topic5 + 
                      topic6 + topic7 + topic8 + topic9 + topic10 + topic11 + 
                      topic12 + topic13 + topic14, data = data_train)


## Model 1 : naives bayes with 10 folds cross validation 
nb_wf <- workflow() %>%
   add_recipe(data_rec) 

nb_clf <- naive_Bayes() %>%
   set_mode("classification") %>%
   set_engine("naivebayes")

data_folds <- vfold_cv(data= data_train, strata = label)
data_folds

nb_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(nb_clf)
   
nb_rs <- fit_resamples(
   nb_wf, 
   data_folds, 
   control = control_resamples(save_pred = TRUE)
)

nb_cv_metrics <- collect_metrics(nb_rs)
nb_cv_predictions <- collect_predictions(nb_rs)
nb_cv_metrics 
plot_roc(nb_cv_predictions, "Naives Bayes Classifier")

### Model 2 : SVM classifier
svm_clf <- svm_rbf() %>%
   set_mode("classification") %>%
   set_engine("kernlab")

svm_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(svm_clf)

svm_cv <- fit_resamples(
   svm_wf,
   data_folds,
   metrics = metric_set(accuracy, sensitivity, specificity),
   control = control_resamples(save_pred = TRUE)
)

svm_cv_metrics <- collect_metrics(svm_cv)
svm_cv_predictions <- collect_predictions(svm_cv)
svm_cv_metrics

# roc curve
svm_cv_predictions %>%
   group_by(id) %>%
   roc_curve(truth = label, .row) %>%
   autoplot() +
   labs(
      color = NULL,
      title = "ROC curve for SVM Classifier",
      subtitle = "Average curve across 10 folds"
   )

svm_cv_predictions %>%
   group_by(id) %>%
   recall(label, .pred_class)

SVM_classifier <- conf_mat_resampled(svm_cv)
plot_confusion_matrix(SVM_classifier, 'SVM Classifier')


#### Model 3 Lasso Classifier
lasso_clf <- logistic_reg(penalty =.01, mixture =1) %>%
   set_mode('classification') %>%
   set_engine('glmnet')

lasso_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(lasso_clf)

lasso_cv <- fit_resamples(
   lasso_wf, 
   data_folds, 
   control = control_resamples(save_pred = TRUE)
)

lasso_rs_metrics <- collect_metrics(lasso_cv)
lasso_rs_predictions <- collect_predictions(lasso_cv)
plot_roc(lasso_rs_predictions, 'Lasso Classifier')
lasso_classifier <- conf_mat_resampled(lasso_cv)
plot_confusion_matrix(lasso_classifier, 'Lasso Classifier')



# Model 4
tune_lasso <- logistic_reg(penalty = tune(), mixture =1) %>%
   set_mode("classification") %>%
   set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

tune_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(tune_lasso)

tune_rs <- tune_grid(
   tune_wf,
   data_folds,
   grid = lambda_grid,
   control = control_resamples(save_pred = TRUE)
)

collect_metrics(tune_rs)

autoplot(tune_rs) +
   labs(
      title = "Lasso model performance across regularization penalties",
      subtitle = "Performance metrics can be used to identity the best penalty"
   )

tune_rs %>%
   show_best("roc_auc")

chosen_auc <- tune_rs %>%
   select_by_one_std_err(metric = "roc_auc", -penalty)
# the optimal penatly is .0418
chosen_auc

# finalize workflow for model 4 
final_lasso <- finalize_workflow(tune_wf, chosen_auc)
fitted_lasso <- fit(final_lasso, data_train)

# what topics contribute the most to a tweet not 
# being about depression?
fitted_lasso %>%
   pull_workflow_fit() %>%
   tidy() %>%
   arrange(-estimate)
# topics 11, 13, 8, 10, 3 contribute the most to
# tweets about depression and topics 6, 9, 12, 14, 7 
# contribute the most to a tweets not being about depression












#### Does adding lingustic features improve the model
library(textfeatures)
text <- data_train$tweet_text
features <- textfeatures(text, normalize = FALSE)
data_train$n_first_person <- features$n_first_person
data_train$n_third_person <- features$n_third_person
data_train$n_second_personp <- features$n_second_personp
#features$n_mentions[13]


#### Model 4
data_rec2 <- recipe(label ~ topic1+topic2+topic3+topic4+topic5+topic6+
                       topic7+topic8+topic9+topic10+topic11+topic12+
                       topic13+topic14+n_first_person+n_third_person+
                       n_second_personp, data = data_train)

svm_wf2 <- workflow() %>%
   add_recipe(data_rec2) %>%
   add_model(svm_clf)

svm_cv2 <- fit_resamples(
   svm_wf2,
   data_folds,
   metrics = metric_set(accuracy, sensitivity, specificity),
   control = control_resamples(save_pred = TRUE))

svm_cv_metrics2 <- collect_metrics(svm_cv2)
svm_cv_predictions2 <- collect_predictions(svm_cv2)
svm_cv_metrics2

svm_cv_predictions2 %>%
   group_by(id) %>%
   recall(label, .pred_class)

SVM_classifier2 <- conf_mat_resampled(svm_cv2)
plot_confusion_matrix(SVM_classifier2, 'SVM Classifier Using just Tokens')










### Baseline Model 
data_rec3 <- recipe(label ~ topic1 + topic2 + topic3 + topic4 + topic5 + 
                      topic6 + topic7 + topic8 + topic9 + topic10 + topic11 + 
                      topic12 + topic13 + topic14, data = data_train)
data_rec3 <- recipe(label ~ clean_text, data = data_train)
data_rec3 <- data_rec3 %>%
   step_tokenize(clean_text) %>%
   step_stopwords(clean_text) %>%
   step_tokenfilter(clean_text, max_tokens = 500) %>%
   step_tfidf(clean_text)

svm_wf2 <- workflow() %>%
   add_recipe(data_rec3) %>%
   add_model(svm_clf)

svm_cv2 <- fit_resamples(
   svm_wf2,
   data_folds,
   metrics = metric_set(accuracy, sensitivity, specificity),
   control = control_resamples(save_pred = TRUE))

svm_cv_metrics2 <- collect_metrics(svm_cv2)
svm_cv_predictions2 <- collect_predictions(svm_cv2)
svm_cv_metrics2

svm_cv_predictions2 %>%
   group_by(id) %>%
   recall(label, .pred_class)

SVM_classifier2 <- conf_mat_resampled(svm_cv2)
plot_confusion_matrix(SVM_classifier2, 'SVM Classifier Using just Tokens')








