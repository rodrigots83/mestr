# install.packages("mlbench")
# install.packages("caretEnsemble")
# install.packages("rattle")
install.packages("xray")

library(data.table)
library(tidyverse)
library(ggplot2)
library(caret)
library(skimr)
library(mlbench)
library(caretEnsemble)
library(rattle)

#Descrição dos métodos: https://topepo.github.io/caret/train-models-by-tag.html#accepts-case-weights


set.seed(1986)

data(BreastCancer)

str(BreastCancer)

BreastCancer =
  BreastCancer %>%
  mutate_if(is.ordered, as.numeric) %>%
  mutate(CancerType = as.numeric(Class)) %>%
  select(-Id, -Class) %>%
  drop_na() %>%
  as.data.table()

str(BreastCancer)

# View(BreastCancer)

dummyVar_model = dummyVars(formula = ~ .,
                           data = BreastCancer)
data = as.data.table(predict(dummyVar_model, newdata = BreastCancer))

data

problematicVariables = nearZeroVar(data, names = T)
problematicVariables

data =
  data %>%
  select(-problematicVariables) %>%
  mutate(CancerType = case_when(
                            CancerType == 1 ~ "Benign",
                            CancerType == 2 ~ "Malignant"))

table(data$CancerType)

# Summary stats -----------------------------------------------------------

stargazer::stargazer(data, type = "text")
skimr::skim_to_wide(data)
xray::anomalies(data)

# Machine learning models -------------------------------------------------

control <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                        number = 10,
                        repeats = 20,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final",
                        allowParallel = TRUE)

# Algoritmos disponíveis no caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

# Rodando uma árvore de decisão com repeated k-fold cross-validation
# Função de maximização: ROC
model_arvoreDecisao1 = caret::train(CancerType ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart',
                                    tuneLength   = 20)

model_arvoreDecisao1

# A cara da árvore de decisão...
rattle::fancyRpartPlot(model_arvoreDecisao1$finalModel)


# Vamos ver os parâmetros da árvore de decisão (que podem ser otimizados)
modelLookup('rpart')

# Testando mais valores para o parâmetro de complexity (cp)
model_arvoreDecisao2 = caret::train(CancerType ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart',
                                    tuneLength   = 20)

model_arvoreDecisao2

# A cara da árvore de decisão...
rattle::fancyRpartPlot(model_arvoreDecisao2$finalModel)



grid.search = expand.grid(cp = seq(0, 0.01, length.out = 20))

model_arvoreDecisao3 = caret::train(CancerType ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart',
                                    tuneGrid     = grid.search)

model_arvoreDecisao3

# A cara da árvore de decisão...
rattle::fancyRpartPlot(model_arvoreDecisao3$finalModel)



# Juntando todos os classificadores em um comando só ----------------------

models =
      caretList(CancerType ~ .,
                data = data,
                trControl = control,
                metric = "ROC",
                tuneList = list(#adaboost     = caretModelSpec(method = "adaboost"), #muito demorado para rodar
                                arvoreDecisao = caretModelSpec(method = "rpart",
                                                               tuneGrid = expand.grid(cp = seq(0, 10, length = 20))),
                                knn           = caretModelSpec(method = "knn"),
                                logit         = caretModelSpec(method = "glm", family = "binomial"),
                                elasticnet    = caretModelSpec(method = "glmnet"),
                                redeNeural    = caretModelSpec(method = "mlpML"),
                                rf            = caretModelSpec(method = "ranger")),
                preProcess = c("knnImpute", "nzv", "center", "scale"))



# Performance analysis ----------------------------------------------------

modelsPerformance = resamples(models)

bwplot(modelsPerformance)
dotplot(modelsPerformance)


modelCor(modelsPerformance)

xyplot(modelsPerformance)


# Ensemble ----------------------------------------------------------------

ensembleModel <- caretEnsemble(models, metric = "ROC", trControl = control)

summary(ensembleModel)

plot(ensembleModel)


# Feature selection 0 -----------------------------------------------------

featureSelection_arvoreDecisao = varImp(models$arvoreDecisao)
plot(featureSelection_arvoreDecisao)

featureSelection_logit = varImp(models$logit)
plot(featureSelection_logit)

featureSelection_elasticNet = varImp(models$elasticnet)
plot(featureSelection_elasticNet)


# Qual o rank médio?
featureSelection_arvoreDecisao_results =
  data.table(technique = "Decision tree",
             variable  = rownames(featureSelection_arvoreDecisao$importance),
             value     = featureSelection_arvoreDecisao$importance)

featureSelection_logit_results =
  data.table(technique = "logit",
             variable  = rownames(featureSelection_logit$importance),
             value     = featureSelection_logit$importance)

featureSelection_elasticNet_results =
  data.table(technique = "elasticnet",
             variable  = rownames(featureSelection_elasticNet$importance),
             value     = featureSelection_elasticNet$importance)


featureSelection_results = rbindlist(l = list(featureSelection_arvoreDecisao_results,
                                              featureSelection_logit_results,
                                              featureSelection_elasticNet_results),
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
