require(quanteda)
require(topicmodels)
require(quanteda.textstats)
require(wordcloud2)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(purrr)
library(textrecipes)
library(recipes)
library(workflows)
library(kernlab)
library(themis)
library(furrr)
library(glmnet)
library(doParallel)
library(doSNOW)
library(caret)

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
all_data$n_charsperword <- features$n_charsperword
all_data$n_prepositions <- features$n_prepositions


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
   collocations <- textstat_collocations(tokens, min_count = 10)
   tokens <- tokens_compound(tokens, collocations, case_insensitive= TRUE, join = TRUE)
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
   top10_terms <- c('depression', 'mentalhealth', 'can', 'anxiety_depression', 'life', 'can', 'depression_anxiety', 'people', 'just', 'mental_healt', 'pandemicfatigue','mentalhealthawareness', 'covidfatigue', 'anxiety')
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
      roc_curve(truth = label, .pred_covid_fatigue) %>%
      autoplot() +
      labs(
         color = NULL,
         title = paste("ROC curve for", classifier_name),
         subtitle = "Average curve across 10 folds")
   return(roc)
}

# function to plot confusion matrices
plot_cm <- function(metric, classifier_name) {
   confusion_matrix <- as.data.frame(metric)
   ggplot(confusion_matrix, aes(x=Truth, y=Prediction, fill=Freq)) +
      geom_tile() + theme_bw() + coord_equal() +
      scale_fill_distiller(palette="Blues", direction=1) + 
      guides(fill=F) + theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) + 
      labs(title = paste("Confusion Matrix for", classifier_name),
           subtitle = 'Cell counts are averages across 10 folds')+ 
      geom_text(aes(label=Freq), color="black", size=10) 
}

#function to plot test confusion matrices
plot_cm2 <- function(cmObject, classifier_name) {
   CM <- tidy(cmObject$table)
   colnames(CM)[2] <- c('Truth')
   confusion_matrix <- as.data.frame(CM)
   ggplot(confusion_matrix, aes(x=Truth, y=Prediction, fill=n)) +
      geom_tile() + theme_bw() + coord_equal() +
      scale_fill_distiller(palette="Greens", direction=1) +
      guides(fill=F) + theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 15)) +
      labs(title = paste("Confusion Matrix for", classifier_name),
           subtitle = 'Results from Testing Set')+ 
      geom_text(aes(label=n), color="black", size=10) 
}

###### Training Set ###### 
train_tokens <- create_tokens(data_train$clean_text)
# the number of documents and terms in the matrix 
train_DTM <- create_DTM(train_tokens)
dim(train_DTM) #before removal
res <- remove_top10(train_DTM, data_train)
train_DTM <- res[[1]]
data_train <- res[[2]]
dim(train_DTM) #after removal 
train_DTM = convert(train_DTM, to = "topicmodels") 

###### Testing Set ###### 
test_tokens <- create_tokens(data_test$clean_text)
test_DTM <- create_DTM(test_tokens)
dim(test_DTM) 
res2 <- remove_top10(test_DTM, data_test)
test_DTM <- res2[[1]]
data_test <- res2[[2]]
dim(test_DTM) 
test_DTM = convert(test_DTM, to = "topicmodels") 


###### Topic Modeling ######
# train topic model with cross validation
folds <- 5
splitfolds <- sample(1:folds, 100, replace = TRUE)
k_topics <- c(2:30)
# set Gibbs sampling parameters
burnin <- 100 
iter <- 500   
thin <- 5     
seed <-list(1,12,123,1234,12345)
nstart <- 5    
best <- TRUE    

# Performing Topic Modeling 
system.time({
   results <- foreach(j = 1:length(k_topics), .combine = rbind) %dopar%{
      k <- k_topics[j]
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
         train_set <- train_DTM[splitfolds != i , ]
         valid_set <- train_DTM[splitfolds == i, ]
         fitted <- LDA(train_DTM, k=k, method="Gibbs",
                       control=list(alpha=0.2,iter=iter,burnin=burnin, seed=1234, thin=thin))
         results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set,estimate_theta = TRUE, use_theta = TRUE))}
      return(results_1k)
      }
})

# plot perplexity
topic_resultsDF <- as.data.frame(results)
aggregate(perplexity ~ k, topic_resultsDF, mean) %>%
   ggplot(aes(x = k, y = perplexity)) +
   geom_point() + 
   geom_line() +
   labs(title = 'Evaluating LDA topic model with 5-fold cross validation',
        x = "Number of topics", 
        y = "Perplexity when fitting the trained model to the hold-out set",
        subtitle = 'Minimum perplexity is optimal' )

#Run LDA topic model with ideal number of topics
topicModel <- LDA(train_DTM,15 , method= 'Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin, alpha = .2))
tmResult <- posterior(topicModel)
terms(topicModel, 10)
tmResult2 <- posterior(topicModel, test_DTM)

#top5termsPerTopic <- terms(topicModel, 5)
#topicNames <- apply(top5termsPerTopic, 2, paste, collapse=' ')
#word_cloud(tmResult, 10)
#word_cloud(tmResult, 16)
   

###### Classifying tweets using topic model distribution as features
df <- as.data.frame(tmResult[['topics']])
data_train <- cbind(data_train, new_col = df)  
colnames(data_train)[21:35] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                 'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                 'topic11', 'topic12', 'topic13', 'topic14', 'topic15')

df2 <- as.data.frame(tmResult2[['topics']])
data_test <- cbind(data_test, new_col = df2)  
colnames(data_test)[21:35] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                'topic11', 'topic12', 'topic13', 'topic14', 'topic15')

# data recipe
data_rec <- recipe(label ~ topic1 + topic2 + topic3 + topic4 + topic5 +
                      topic6 + topic7 + topic8 + topic9 + topic10 + topic11+
                      topic12 + topic13 + topic14 + topic15,
                   data = data_train)

data_folds <- vfold_cv(data= data_train, strata = label)

### Model 1 : SVM classifier
svm_clf <- svm_rbf() %>%
   set_mode("classification") %>%
   set_engine("kernlab") %>%
   translate()

svm_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(svm_clf)

svm_cv <- fit_resamples(
   svm_wf,
   data_folds,
   control = control_resamples(save_pred = TRUE)
)

svm_cv_metrics <- collect_metrics(svm_cv)
svm_cv_predictions <- collect_predictions(svm_cv)
svm_cv_metrics
SVM_classifier <- conf_mat_resampled(svm_cv)
plot_cm(SVM_classifier, 'Classifier A')
plot_roc(svm_cv_predictions, "Classifier A")


# Make predictions on test data
fit <- svm_wf %>%
   fit(data_train)

final_svm <- fit %>%
   pull_workflow_fit()
fitted_svm <- predict(fit, data_test)
summary(fitted_svm)
summary(data_test$label)
output <- tibble(data.frame(original = data_test$label, predicted = fitted_svm$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output$original, output$predicted) 
plot_cm2(confusionMatrix(output$original, output$predicted), 'Classifier A')
#Accuracy : 0.7706 
#Kappa : 0.2751 
#Sensitivity : 0.61702         
#Specificity : 0.79179 


#### Model 2 Lasso Classifier
lasso_clf <- logistic_reg(penalty =.01, mixture =1) %>%
   set_mode('classification') %>%
   set_engine('glmnet') %>%
   translate()

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
lasso_classifier <- conf_mat_resampled(lasso_cv)
plot_cm(lasso_classifier, 'Lasso Classifier')
plot_roc(lasso_rs_predictions, 'Lasso Classifier')


# Lasso after tuning penalty
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

tune_metrics <- collect_metrics(tune_rs)
tune_predictions <- collect_predictions(tune_rs)
plot_roc(tune_predictions, "Classifier B")

autoplot(tune_rs) +
   labs(
      title = "Lasso model performance across regularization penalties",
      subtitle = "Performance metrics can be used to identity the best penalty"
   )

tune_rs %>%
   show_best("roc_auc")

chosen_auc <- tune_rs %>%
   select_by_one_std_err(metric = "roc_auc", -penalty)
# the optimal penalty is .0189
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
# topics 6, 13, 8, 2 contribute the most to
# tweets about depression and topics 14, 7, 3, 15
# contribute the most to a tweets not being about depression

# train using the chosen auc
lasso_clf <- logistic_reg(penalty =.0189, mixture =1) %>%
   set_mode('classification') %>%
   set_engine('glmnet') %>%
   translate()

lasso_wf <- workflow() %>%
   add_recipe(data_rec) %>%
   add_model(lasso_clf)

lasso_cv <- fit_resamples(
   lasso_wf, 
   data_folds, 
   control = control_resamples(save_pred = TRUE))

lasso_rs_metrics <- collect_metrics(lasso_cv)
lasso_rs_predictions <- collect_predictions(lasso_cv)
lasso_classifier <- conf_mat_resampled(lasso_cv)
plot_cm(lasso_classifier, 'Classifier B')
plot_roc(lasso_rs_predictions, 'Classifier B')

fit2 <- lasso_wf %>%
   fit(data_train)
fitted_lasso <- predict(fit2, data_test)
output2 <- tibble(data.frame(original = data_test$label, predicted = fitted_lasso$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output2$original, output2$predicted) 
plot_cm2(confusionMatrix(output2$original, output2$predicted) , 'Classifier B ')
#Accuracy : 0.7603  
#Kappa : 0.1914      
#Sensitivity : 0.61290         
#Specificity : 0.77311 

#### Does adding linguistic and other features improve the model?
#### Model 3
data_rec2 <- recipe(label ~ topic1 + topic2 + topic3 + topic4 + topic5 +
                       topic6 + topic7 + topic8 + topic9 + topic10 +
                       topic11+ topic12 + topic13 + topic14 + topic15 + word_count + emoji_count + n_charsperword +
                       n_first_person + n_prepositions, data = data_train)

data_rec2 <- data_rec2 %>%
   step_normalize(word_count, emoji_count, n_charsperword, n_first_person, n_prepositions)

svm_wf2 <- workflow() %>%
   add_recipe(data_rec2) %>%
   add_model(svm_clf)

svm_cv2 <- fit_resamples(
   svm_wf2,
   data_folds,
   control = control_resamples(save_pred = TRUE))

svm_cv_metrics2 <- collect_metrics(svm_cv2)
svm_cv_predictions2 <- collect_predictions(svm_cv2)
SVM_classifier2 <- conf_mat_resampled(svm_cv2)
plot_cm(SVM_classifier2, 'Classifier C')
plot_roc(svm_cv_predictions2, 'Classifier C')

fit3 <- svm_wf2 %>%
   fit(data_train)
fitted_svm2 <- predict(fit3, data_test)
summary(fitted_svm2)
summary(data_test$label)
output3 <- tibble(data.frame(original = data_test$label, predicted = fitted_svm2$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output3$original, output3$predicted) 
plot_cm2(confusionMatrix(output3$original, output3$predicted), 'Classifier C')
#Accuracy : 0.7784  
#Kappa : 0.3023 
#Sensitivity : 0.6458          
#Specificity : 0.7971 

# Model 4 
lasso_clf2 <- logistic_reg(penalty =.0189, mixture =1) %>%
   set_mode('classification') %>%
   set_engine('glmnet') %>%
   translate()

lasso_wf2 <- workflow() %>%
   add_recipe(data_rec2) %>%
   add_model(lasso_clf2)

lasso_cv2 <- fit_resamples(
   lasso_wf2, 
   data_folds, 
   control = control_resamples(save_pred = TRUE)
)

lasso_rs_metrics2 <- collect_metrics(lasso_cv2)
lasso_rs_predictions2 <- collect_predictions(lasso_cv2)
plot_roc(lasso_rs_predictions2, 'Classifier D')
lasso_classifier2 <- conf_mat_resampled(lasso_cv2)
plot_cm(lasso_classifier2, 'Classifier D')


# Make predictions on test data
fit3 <- lasso_wf2 %>%
   fit(data_train)
fitted_lasso3 <- predict(fit3, data_test)
output3 <- tibble(data.frame(original = data_test$label, predicted = fitted_lasso3$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output3$original, output3$predicted)
plot_cm2(confusionMatrix(output3$original, output3$predicted), 'Classifier D')
#Accuracy : 0.7603  
#Kappa : 0.1559
#Sensitivity : 0.66667         
#Specificity : 0.76567  