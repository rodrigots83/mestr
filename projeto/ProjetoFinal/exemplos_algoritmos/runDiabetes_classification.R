library(data.table)
library(tidyverse)
library(ggplot2)
library(caret)
library(skimr)
library(mlbench)
library(caretEnsemble)

#Descrição dos métodos: https://topepo.github.io/caret/train-models-by-tag.html#accepts-case-weights

set.seed(1986)

data(PimaIndiansDiabetes2)

str(PimaIndiansDiabetes2)

PimaIndiansDiabetes2 =
  PimaIndiansDiabetes2 %>%
  mutate_if(is.ordered, as.numeric) %>%
  mutate(diabetes = as.numeric(diabetes)) %>%
  as.data.table()

dummyVar_model = dummyVars(formula = ~ .,
                           data = PimaIndiansDiabetes2)
data = as.data.table(predict(dummyVar_model, newdata = PimaIndiansDiabetes2))


problematicVariables = nearZeroVar(data, names = T)

data =
  data %>%
  select(-problematicVariables) %>%
  mutate(diabetes = case_when(
     diabetes == 1 ~ "Normal",
     diabetes == 2 ~ "Diabetes")) %>%
  mutate(diabetes = as.factor(diabetes))

table(data$diabetes)

# Summary stats -----------------------------------------------------------

stargazer::stargazer(data, type = "text")
skimr::skim_to_wide(data)
xray::anomalies(data)

knnImpute_preProcess = preProcess(data, "knnImpute")
imputtedData = predict(knnImpute_preProcess, newdata = data)


# Machine learning models -------------------------------------------------

control <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                        number = 10,
                        repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final",
                        allowParallel = TRUE)


model_svm = caret::train(diabetes ~ .,
                         data         = imputtedData,
                         trControl    = control,
                         metric       = "ROC",
                         tuneLength   = 10,
                         method       = 'svmRadial')

# Parece que o modelo ótimo é de canto
model_svm


# Vamos fazer um grid search focado nessa área para ver se conseguimos melhorar
grid.search = expand.grid(C = seq(0.28, 0.32, length.out =  5),
                          sigma = seq(0, 0.1, length.out = 5))

model_svm2 = caret::train(diabetes ~ .,
                         data         = imputtedData,
                         trControl    = control,
                         metric       = "ROC",
                         method       = 'svmRadial',
                         tuneGrid     = grid.search)

model_svm2

models =
  caretList(diabetes ~ .,
            data = imputtedData,
            trControl = control,
            metric = "ROC",
            tuneList = list(logit      = caretModelSpec(method = "glm", family = "binomial"),
                            elasticnet = caretModelSpec(method = "glmnet",
                                                        tuneGrid = expand.grid(alpha = 0:1,
                                                                               lambda = seq(0.0001, 1, length = 20))),
                            rf         = caretModelSpec(method = "ranger")),
            preProcess = c("center", "knnImpute", "scale"))



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

featureSelection_logit = varImp(models$logit)
plot(featureSelection_logit)

featureSelection_elasticNet = varImp(models$elasticnet)
plot(featureSelection_elasticNet)


# Qual o rank médio?
featureSelection_logitResults =
  data.table(technique = "logit",
             variable  = rownames(featureSelection_logit$importance),
             value     = featureSelection_logit$importance)

featureSelection_elasticNetResults =
  data.table(technique = "elasticnet",
             variable  = rownames(featureSelection_elasticNet$importance),
             value     = featureSelection_elasticNet$importance)

featureSelection_results = rbindlist(l = list(featureSelection_logitResults,
                                              featureSelection_elasticNetResults),
                                     use.names = T)


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


# Random forest não possui a funcionalidade de varImpl


# Feature selection 1 -------------------------------------------------------
# RFE (Recursive feature elimination)

subsets <- c(1:length(data))

lrFuncs$summary <- twoClassSummary

rfe.ctrl = rfeControl(functions = lrFuncs,
                      method = "boot",
                      number = 25,
                      allowParallel = TRUE,
                      verbose = TRUE)

predictors = data %>% select(-CancerType) %>% as.data.frame()
targetVariable = as.factor(data$CancerType)

rfe <- rfe(x = predictors,
           y = targetVariable,
           sizes = subsets,
           metric = "ROC",
           rfeControl = rfe.ctrl)
rfe

# variáveis selecionadas
rfe$optVariables


# Feature Selection 2 -----------------------------------------------------
# Simulated Annealing

caretSA$fitness_extern <- twoClassSummary

safs.ctrl = safsControl(functions = caretSA, method = "boot", number = 10,
                        metric = c(internal = "ROC", external = "ROC"),
                        maximize = c(internal = TRUE, external = TRUE),
                        holdout = .2, improve = 5,
                        allowParallel = TRUE, verbose = TRUE)


sa <- safs(x = predictors,
           y = targetVariable,
           iters = 10,
           method = "glm",
           family = "binomial",
           metric = "ROC",
           trControl = control,
           safsControl = safs.ctrl)

# variáveis selecionadas
sa$optVariables

# Feature Selection 3 -----------------------------------------------------
# Genetic algorithm

caretGA$fitness_extern <- twoClassSummary

gafs.ctrl = gafsControl(functions = caretGA, method = "boot", number = 10,
                        metric = c(internal = "ROC", external = "ROC"),
                        maximize = c(internal = TRUE, external = TRUE),
                        holdout = .2,
                        allowParallel = TRUE, genParallel = TRUE, verbose = TRUE)

ga <- gafs(x = predictors,
           y = targetVariable,
           iters = 5,
           popSize = 2,
           elite = 0,
           differences = TRUE,
           method = "glm",
           family = "binomial",
           metric = "ROC",
           trControl = control,
           gafsControl = gafs.ctrl)

# variáveis selecionadas
ga$optVariables
