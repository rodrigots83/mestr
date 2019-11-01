rm(list = ls())
library(data.table)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)

#Descrição dos métodos: https://topepo.github.io/caret/train-models-by-tag.html#accepts-case-weights

set.seed(1986)

autoPrices = fread("./data/auto_prices_processed.csv")
str(autoPrices)

autoPrices =
  autoPrices %>%
  mutate_if(is.character, as.factor)

dummyVar_model = dummyVars(formula = ~ .,
                           data = autoPrices)

data = as.data.table(predict(dummyVar_model, newdata = autoPrices))


problematicVariables = nearZeroVar(data, names = T)
problematicVariables

data =
  data %>%
  select(-problematicVariables)

# Summary stats -----------------------------------------------------------

skimr::skim_to_wide(data)
xray::anomalies(data)

# Machine learning models -------------------------------------------------

control <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                        number = 10,
                        repeats = 5,
                        savePredictions = "final",
                        allowParallel = TRUE)


model_svm = caret::train(price ~ .,
                         data         = data,
                         trControl    = control,
                         metric       = "RMSE",
                         preProcess   = c("center", "knnImpute", "scale"),
                         tuneLength   = 10,
                         method       = 'svmRadial')

model_svm


models =
  caretList(price ~ .,
            data = data,
            trControl = control,
            metric = "RMSE",
            tuneList = list(svm               = caretModelSpec(method = "svmRadial"),
                            knn               = caretModelSpec(method = "knn"),
                            regressaoLinear   = caretModelSpec(method = "lm"),
                            elasticNet        = caretModelSpec(method = "enet"),
                            redeNeural        = caretModelSpec(method = "mlpML")),
            preProcess = c("center", "knnImpute", "scale"))



# Performance analysis ----------------------------------------------------

modelsPerformance = resamples(models)
modelsPerformance

bwplot(modelsPerformance, scales = list(x = list(relation = "free"),
                                        y = list(relation = "free")))
dotplot(modelsPerformance, scales = list(x = list(relation = "free"),
                                         y = list(relation = "free")))
xyplot(modelsPerformance, scales = list(x = list(relation = "free"),
                                        y = list(relation = "free")))


modelCor(modelsPerformance)

# Ensemble ----------------------------------------------------------------

ensembleModel <- caretEnsemble(models, metric = "RMSE", trControl = control)

summary(ensembleModel)

plot(ensembleModel)


# Feature selection 0 -----------------------------------------------------

featureSelection_svm = varImp(models$svm)
plot(featureSelection_svm)

featureSelection_knn = varImp(models$knn)
plot(featureSelection_knn)

featureSelection_regressaoLinear = varImp(models$regressaoLinear)
plot(featureSelection_regressaoLinear)

featureSelection_elasticNet = varImp(models$elasticNet)
plot(featureSelection_elasticNet)

featureSelection_redeNeural = varImp(models$redeNeural)
plot(featureSelection_redeNeural)

# Qual o rank médio?
featureSelection_svm_results =
  data.table(technique = "SVM",
             variable  = rownames(featureSelection_svm$importance),
             value     = featureSelection_svm$importance)

featureSelection_knn_results =
  data.table(technique = "k-NN",
             variable  = rownames(featureSelection_knn$importance),
             value     = featureSelection_knn$importance)

featureSelection_regressaoLinear_results =
  data.table(technique = "Linear regression",
             variable  = rownames(featureSelection_regressaoLinear$importance),
             value     = featureSelection_regressaoLinear$importance)

featureSelection_elasticNet_results =
  data.table(technique = "Elastic Net",
             variable  = rownames(featureSelection_elasticNet$importance),
             value     = featureSelection_elasticNet$importance)

featureSelection_redeNeural_results =
  data.table(technique = "Neural Network",
             variable  = rownames(featureSelection_redeNeural$importance),
             value     = featureSelection_redeNeural$importance)

featureSelection_results = rbindlist(l = list(featureSelection_svm_results,
                                              featureSelection_knn_results,
                                              featureSelection_regressaoLinear_results,
                                              featureSelection_elasticNet_results,
                                              featureSelection_redeNeural_results),
                                     use.names = T)

featureSelection_avg =
  featureSelection_results %>%
    group_by(technique) %>%
    arrange(-value.Overall) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    group_by(variable) %>%
    mutate(rankMedio = mean(rank)) %>%
    ungroup() %>%
    distinct(variable, rankMedio) %>%
    arrange(rankMedio)

featureSelection_avg %>%
    ggplot(aes(y = as.factor(rankMedio), x = rankMedio)) +
    geom_segment( aes(x = 0, xend = rankMedio, yend = as.factor(rankMedio))) +
    scale_y_discrete(labels = featureSelection_avg$variable) +
    geom_point() +
    labs(title = "Rank médio dos atributos mais importantes",
         subtitle = "Quanto menor o rank, mais importante",
         x = "Rank médio",
         y = "") +
    theme_bw()


