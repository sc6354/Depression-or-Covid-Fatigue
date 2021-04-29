#######################################################a########
# Title: Classifying Depression tweets from Covid Fatigue tweets 
# Author: Susan Chen 
# Data: 4/27/21
# NYUSH Data Science 2021 Capstone 
###############################################################

require(quanteda)
require(topicmodels)
require(quanteda.textstats)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(themis)
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


###### Set seed for reproducibility ######
set.seed(1234)

# Read in data
data_train <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/trainingSet.csv')
data_test <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/testingSet.csv')
data_test$label <- factor(data_test$label, labels = c('covid_fatigue','depression'))
data_train$label <- factor(data_train$label, labels = c('covid_fatigue','depression'))

###### Functions to use later ######
# function to create tokens
create_tokens <- function(tweets) {
   corpus <- corpus(tweets)
   tokens <- corpus %>%
      tokens(remove_punct =TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = stopwords("en"), selection = "remove") 
   collocations <- textstat_collocations(tokens, min_count = 5)
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
   top10_terms <- c('depression', 'mentalhealth', 'pandemicfatigue', 'covidfatigue', 'depression_anxiety')
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
seed <- list(1,12,123,1234,1235)
thin <- 5     
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
                       control=list(alpha=.2,iter=iter,burnin=burnin, seed=1234, thin=thin))
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

# Alternative way to evaluate k 
library(ldatuning)
# Compute the K value "scores" from K=2 to K=15
optimalK <- FindTopicsNumber(
   train_DTM,
   topics = seq(from = 2, to = 30, by = 1),
   metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control =list(alpha=0.2,iter=iter,burnin=burnin, seed=1234, thin=thin),
   mc.cores = 2L,
   verbose = TRUE)
# Plot the scores
FindTopicsNumber_plot(optimalK)


# Run LDA topic model with ideal number of topics
topicModel <- LDA(train_DTM, 15, method= 'Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin, alpha = .2))
tmResult <- posterior(topicModel)
terms(topicModel, 10)
tmResult2 <- posterior(topicModel, test_DTM)

#top5termsPerTopic <- terms(topicModel, 5)
#topicNames <- apply(top5termsPerTopic, 2, paste, collapse=' ')
#word_cloud(tmResult, 10)
#word_cloud(tmResult, 16)
   

# Save topic model probabilities to train and test data sets 
df <- as.data.frame(tmResult[['topics']])
data_train <- cbind(data_train, new_col = df)  
colnames(data_train)[23:37] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                 'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                 'topic11', 'topic12','topic13', 'topic14', 'topic15')

df2 <- as.data.frame(tmResult2[['topics']])
data_test <- cbind(data_test, new_col = df2)  
colnames(data_test)[23:37] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                'topic11', 'topic12','topic13', 'topic14', 'topic15')


train_topics <- data_train %>%
   select(23:37)
write.csv(train_topics,"/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/trainingTM.csv", row.names =FALSE)

test_topics <- data_test %>%
   select(23:37)
write.csv(test_topics,"/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/testingTM.csv", row.names =FALSE)

# Plot the top N words by topic
library(tidytext)
topics <- tidy(topicModel, matrix = "beta")

terms_per_topic <- 10
top_terms <- topics %>%
   group_by(topic) %>%
   top_n(terms_per_topic, beta) %>%
   ungroup() %>%
   arrange(topic, -beta)

# manually resolve ties since top_n() doesn't handle ties 
top_terms <- top_terms %>%
   group_by(topic) %>%
   slice(1:terms_per_topic) %>%
   ungroup()

top_terms$topic <- factor(top_terms$topic)

top_terms %>%
   mutate(term = reorder(term, beta)) %>%
   ggplot(aes(term, beta)) +
   ggtitle("Top 10 terms by Topic") + 
   theme_grey()+
   geom_bar(stat = "identity") +
   facet_wrap(~ topic, scales = "free") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   coord_flip()


###### Classifying Tweets ######
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
plot_cm(SVM_classifier, 'SVM Classifier, Features List 1')
plot_roc(svm_cv_predictions, 'SVM Classifier, Features List 1')

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
plot_cm2(confusionMatrix(output$original, output$predicted), 'SVM Classifier, Features List 1')
#Accuracy :  0.7888 
#Kappa :0.3319           
#Sensitivity : 0.74419    
#Specificity : 0.79429 


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
   theme_gray() +
   theme(axis.text=element_text(size=10),
         axis.title=element_text(size=14)) +
   labs(title = "Lasso model performance across regularization penalties")

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
# topics 5, 13 contribute the most to
# tweets about depression and topics 6,9,4,8,2
# contribute the most to a tweets not being about depression
#term        estimate penalty
#<chr>          <dbl>   <dbl>
#2 topic5        1.47    0.0189
#3 topic13       1.10    0.0189
#12 topic6       -0.0871  0.0189
#13 topic9       -0.757   0.0189
#14 topic4       -1.35    0.0189
#15 topic8       -4.78    0.0189
#16 topic2       -5.33    0.0189


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
plot_cm(lasso_classifier, 'Lasso Classifier, Features List 1')
plot_roc(lasso_rs_predictions, 'Lasso Classifier, Features List 1')

fit2 <- lasso_wf %>%
   fit(data_train)
fitted_lasso <- predict(fit2, data_test)
output2 <- tibble(data.frame(original = data_test$label, predicted = fitted_lasso$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output2$original, output2$predicted) 
plot_cm2(confusionMatrix(output2$original, output2$predicted) , 'Lasso Classifier, Features List 1')
#Accuracy :0.7761 
#Kappa :0.2724     
#Sensitivity : 0.72222    
#Specificity : 0.78151 



#### Does adding linguistic and other features improve the model?
#### Model 3
data_rec2 <- recipe(label ~ topic1 + topic2 + topic3 + topic4 + topic5 +
                       topic6 + topic7 + topic8 + topic9 + topic10 +
                       topic11+ topic12 + topic13 + topic14 + topic15 + word_count + n_charsperword +
                       n_first_person + n_prepositions + n_uq_words, data = data_train)

data_rec2 <- data_rec2 %>%
   step_normalize(word_count, n_charsperword, n_first_person, n_prepositions, n_uq_words)

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
plot_cm(SVM_classifier2, 'SVM Classifier, Features List 2')
plot_roc(svm_cv_predictions2, 'SVM Classifier Features List 2')

fit3 <- svm_wf2 %>%
   fit(data_train)
fitted_svm2 <- predict(fit3, data_test)
summary(fitted_svm2)
summary(data_test$label)
output3 <- tibble(data.frame(original = data_test$label, predicted = fitted_svm2$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output3$original, output3$predicted) 
plot_cm2(confusionMatrix(output3$original, output3$predicted), 'SVM Classifier, Features List 2')
#Accuracy : 0.7786 
#Kappa : .2722 
#Sensitivity : 0.75758       
#Specificity :  0.78056 


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
plot_roc(lasso_rs_predictions2, 'Lasso Classifier, Features List 2')
lasso_classifier2 <- conf_mat_resampled(lasso_cv2)
plot_cm(lasso_classifier2, 'Lasso Classifier, Features List 2')


# Make predictions on test data
fit4 <- lasso_wf2 %>%
   fit(data_train)
fitted_lasso3 <- predict(fit4, data_test)
output4 <- tibble(data.frame(original = data_test$label, predicted = fitted_lasso3$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output4$original, output4$predicted)
plot_cm2(confusionMatrix(output4$original, output4$predicted), 'Lasso Classifier, Features List 2')
#Accuracy :0.7684 
#Kappa : 0.2387 
#Sensitivity : 0.69697        
#Specificity : 0.77500 


###### Additional Testing ######
test_data2 <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/second_testing_data.csv')
test_data2$label <- factor(test_data2$label, labels = c('covid_fatigue','depression'))

library(textfeatures)
text <- test_data2$tweet_text
features <- textfeatures(text, verbose = FALSE, normalize = FALSE)
test_data2$n_first_person <- features$n_first_person
test_data2$n_uq_words <- features$n_uq_chars
test_data2$n_charsperword <- features$n_charsperword
test_data2$n_prepositions <- features$n_prepositions

tokens2 <- create_tokens(test_data2$clean_text)
test_DTM2 <- create_DTM(tokens2)
dim(test_DTM2) 
res3 <- remove_top10(test_DTM2, test_data2)
test_DTM2 <- res3[[1]]
test_data2 <- res3[[2]]
dim(test_DTM2) 
test_DTM2 = convert(test_DTM2, to = "topicmodels") 

tmResult3 <- posterior(topicModel, test_DTM2)
df3 <- as.data.frame(tmResult3[['topics']])
test_data2 <- cbind(test_data2, new_col = df3)  
colnames(test_data2)[23:37] <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 
                                'topic6', 'topic7', 'topic8', 'topic9', 'topic10',
                                'topic11', 'topic12','topic13', 'topic14', 'topic15')

test2_fit <- predict(fit3, test_data2)
output5 <- tibble(data.frame(original = test_data2$label, predicted = test2_fit$.pred_class, stringsAsFactors = FALSE))
confusionMatrix(output5$original, output5$predicted) 
plot_cm2(confusionMatrix(output5$original, output5$predicted), 'Additional Testing Set')
#Accuracy : 0.6753 
#Kappa : 0.2778  
#Sensitivity : 0.8462 
#Specificity : 0.6453  