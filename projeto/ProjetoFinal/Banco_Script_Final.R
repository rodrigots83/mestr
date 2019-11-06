# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("stargazer")
# install.packages("skimr")
# install.packages("Rcpp")
# install.packages("mlbench")
# install.packages("caretEnsemble")
# install.packages("rattle")
# install.packages("xray")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("psych")

library(ggplot2)
library(data.table)
library(stargazer)
library(skimr)
library(Rcpp)
library(readxl)
library(dplyr)
library(tidyverse)
library(caret)
library(mlbench)
library(caretEnsemble)
library(rattle)

###########################################################################
###########################################################################
### FUNÇÃO DE CRIAÇÃO DE ARQUIVO
# gera_arquivo_grafico = function(tituloGrafico){
#   nomeArquivo <- str_c(tituloGrafico,".jpeg")
#   ggsave(filename = nomeArquivo, plot = last_plot(), path = getwd(), width = )
# }

###########################################################################
###########################################################################
### FUNÇÃO
# box_plot = function(data, atributos, atributosSubgrupo, variavelDependente = 'Evadiu'){
#   ### SETADO LOCAL PARA GUARDAR OS GRAFICOS GERADOS
#   setwd("/Users/rodrigosantos/Documents/GitHub/weak_signals/projeto/ProjetoFinal/graficos_gerados/")
#   for (atributo in atributos) {
#     if(length(atributosSubgrupo) > 0){
#       for (atributo2 in atributosSubgrupo){
#         result = ggplot(data = dtAmostra, aes_string(variavelDependente, atributo)) +
#           geom_boxplot(aes_string(fill = atributo2))
#         # geom_boxplot() +
#         labs(x = variavelDependente,
#              y = atributo,
#              title = "Boxplot",
#              subtitle = paste(atributo, variavelDependente, sep = " vs "),
#              caption = "") +
#           theme_bw()
#         # facet_wrap(~ atributo2)
#         print(result)
#         ### FUNCAO PARA GRAVAR OS RELATORIOS EM ARQUIVOS
#         gera_arquivo_grafico(str_c(atributo, variavelDependente, atributo2))
#       }
#     }
#     else{
#       result = ggplot(data = dtAmostra, aes_string(variavelDependente, atributo)) +
#         geom_boxplot() +
#         labs(x = variavelDependente,
#              y = atributo,
#              title = "Boxplot",
#              subtitle = paste(atributo, variavelDependente, sep = " vs "),
#              caption = "") +
#         theme_bw()
#       # facet_wrap(~ atributo2)
#       print(result)
#       ### FUNCAO PARA GRAVAR OS RELATORIOS EM ARQUIVOS
#       gera_arquivo_grafico(str_c(atributo, variavelDependente, atributo2))
#     }
#   }
# }

###########################################################################
###########################################################################
### Leitura do arquivo feita e análise dos dados

dtAmostra <- fread("dadosBanco/Amostra_Modelo_Evasao_Correntistas_v3.csv")
str(dtAmostra)

###########################################################################
###########################################################################
### Transformação das variáveis em fatores

# dtAmostra = 
#   dtAmostra %>% 
#   mutate(Evadiu = ifelse(Evadiu == 1, "Sim", "Não")) %>% 
#   mutate(Segmento = ifelse(Segmento == "GV", "Renda_Basica", ifelse(Segmento == "GC", "Classe_Media", ifelse(Segmento == "GR", "Alta_Renda", "Exclusivo"))))
# 
# dtAmostra$Evadiu <- as.factor(dtAmostra$Evadiu)
# dtAmostra$Segmento <- as.factor(dtAmostra$Segmento)
# dtAmostra$Debito_Automatico <- as.factor(dtAmostra$Debito_Automatico)
# dtAmostra$Credito_Salario <- as.factor(dtAmostra$Credito_Salario)
# dtAmostra$Credenciamento <- as.factor(dtAmostra$Credenciamento)
# dtAmostra$Caixa_Seguradora <- as.factor(dtAmostra$Caixa_Seguradora)
# dtAmostra$Pediu_Portabilidade <- as.factor(dtAmostra$Pediu_Portabilidade)
# dtAmostra$Abriu_Reclamacao <- as.factor(dtAmostra$Abriu_Reclamacao)
# dtAmostra$Debito_Automatico_DIF <- as.factor(dtAmostra$Debito_Automatico_DIF)
# dtAmostra$Credito_Salario_DIF <- as.factor(dtAmostra$Credito_Salario_DIF)
# dtAmostra$Caixa_Seguradora_DIF <- as.factor(dtAmostra$Caixa_Seguradora_DIF)
# 
# str(dtAmostra)

###########################################################################
###########################################################################
### Aplicação da função Winsor para diminuir os outliers existentes na base de dados

# dtAmostra = dtAmostra %>% 
#   mutate(winsored_Rentabilidade_PERC = psych::winsor(Rentabilidade_PERC, 0.1)) %>%
#   mutate(winsored_Produtos = psych::winsor(Produtos, 0.01)) %>%
#   mutate(winsored_Produtos_Qualificados_PERC = psych::winsor(Produtos_Qualificados_PERC, 0.01)) %>%
#   mutate(winsored_Produtos_PERC = psych::winsor(Produtos_PERC, 0.01)) %>%
#   mutate(winsored_Movimentacoes = psych::winsor(Movimentacoes, 0.1)) %>%
#   mutate(winsored_Movimentacoes_Anterior = psych::winsor(Movimentacoes_Anterior, 0.1)) %>%
#   mutate(winsored_Movimentacoes_DIF = psych::winsor(Movimentacoes_DIF, 0.1)) %>%
#   mutate(winsored_Movimentacoes_PERC = psych::winsor(Movimentacoes_PERC, 0.1)) %>%
#   mutate(winsored_Aplicacao = psych::winsor(Aplicacao, 0.1)) %>%
#   mutate(winsored_Aplicacao_Anterior = psych::winsor(Aplicacao_Anterior, 0.1)) %>%
#   mutate(winsored_Aplicacao_DIF = psych::winsor(Aplicacao_DIF, 0.1)) %>% 
#   mutate(winsored_Aplicacao_PERC = psych::winsor(Aplicacao_PERC, 0.1)) %>% 
#   mutate(winsored_Credito = psych::winsor(Credito, 0.1)) %>% 
#   mutate(winsored_Credito_Anterior = psych::winsor(Credito_Anterior, 0.1)) %>% 
#   mutate(winsored_Credito_DIF = psych::winsor(Credito_DIF, 0.1)) %>% 
#   mutate(winsored_Credito_PERC = psych::winsor(Credito_PERC, 0.1)) %>% 
#   mutate(winsored_Rentabilidade = psych::winsor(Rentabilidade, 0.1)) %>% 
#   mutate(winsored_Rentabilidade_Anterior = psych::winsor(Rentabilidade_Anterior, 0.1)) %>% 
#   mutate(winsored_Rentabilidade_DIF = psych::winsor(Rentabilidade_DIF, 0.1)) %>% 
#   mutate(winsored_Rentabilidade_PERC = psych::winsor(Rentabilidade_PERC, 0.1))

###########################################################################
###########################################################################
### Execução dos graficos com as variáveis "melhoradas" (winsorizadas)

# variaveisIniciais <- c("Produtos",
#                        "Produtos_Anterior",
#                        "Produtos_DIF",
#                        "winsored_Produtos_Qualificados_PERC",
#                        "Produtos_Qualificados_DIF",
#                        "Produtos_Qualificados_Anterior",
#                        "Produtos_Qualificados",
#                        "winsored_Produtos_PERC",
#                        "winsored_Aplicacao",
#                        "winsored_Aplicacao_Anterior",
#                        "winsored_Aplicacao_PERC", 
#                        "winsored_Aplicacao_DIF",
#                        "winsored_Movimentacoes",
#                        "winsored_Movimentacoes_DIF",
#                        "winsored_Movimentacoes_PERC", 
#                        "winsored_Movimentacoes_Anterior", 
#                        "winsored_Credito", 
#                        "winsored_Credito_Anterior", 
#                        "winsored_Credito_PERC", 
#                        "winsored_Credito_DIF", 
#                        "winsored_Rentabilidade", 
#                        "winsored_Rentabilidade_DIF", 
#                        "winsored_Rentabilidade_Anterior", 
#                        "winsored_Rentabilidade_PERC")
# 
# box_plot(credit, variaveisIniciais, NULL)

###########################################################################
###########################################################################
### Crialçao de uma lista contendo aquelas variaveis que são fatores (não tiveram boa relação com a variável preditora)

# variaveisSubgrupo <- c("Segmento", "Credenciamento", 
#                        "Caixa_Seguradora", "Caixa_Seguradora_DIF", 
#                        "Credito_Salario", "Debito_Automatico", 
#                        "Credito_Salario_DIF", "Debito_Automatico_DIF")
# 
# box_plot(credit, c("winsored_Movimentacoes", "Produtos_Qualificados", 
#                    "winsored_Aplicacao_PERC", "winsored_Credito_DIF"), variaveisSubgrupo)

###########################################################################
###########################################################################
### Início dos trabalhos para a regressão... criação de variáveis dummies

str(dtAmostra)

set.seed(1986)

dtAmostra =
  dtAmostra %>%
  mutate_if(is.ordered, as.numeric) %>%
  mutate(EvadiuTipo = as.numeric(Evadiu)) %>%
  select(-Id, -Pediu_Portabilidade, -Abriu_Reclamacao, -Evadiu) %>%
  drop_na() %>%
  as.data.table()

dummyVar_model = dummyVars(formula = ~ .,
                           data = dtAmostra)

data = as.data.table(predict(dummyVar_model, newdata = dtAmostra))
str(data)

problematicVariables = nearZeroVar(data, names = T)
problematicVariables

data =
  data %>%
  # select(-problematicVariables) %>%
  mutate(EvadiuTipo = case_when(
    EvadiuTipo == 0 ~ "Não",
    EvadiuTipo == 1 ~ "Sim"))
str(data)
table(data$EvadiuTipo)

### Summary stats

# stargazer::stargazer(data, type = "text")
# skimr::skim_to_wide(data)
# xray::anomalies(data)

### Rodando o modelo de machine learning
control <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                        number = 10,
                        repeats = 1,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final")
control

# model_preProcess = preProcess(x = data, method = c("center", "scale"))
# preprocessed_data = predict(model_preProcess, newdata = data)
# str(data)
# str(preprocessed_data)
# stargazer::stargazer(data, type = "text")

###########################################################################
###########################################################################
### Rodando uma árvore de decisão com repeated k-fold cross-validation-- 1ª FORMA
### Função de maximização: ROC
model_arvoreDecisao1 = caret::train(EvadiuTipo ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart',
                                    tuneLength   = 5)

### Rodando uma árvore de decisão com repeated k-fold cross-validation -- 2ª FORMA
model_arvoreDecisao2 = caret::train(EvadiuTipo ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart')

### Rodando uma árvore de decisão com repeated k-fold cross-validation -- 3ª FORMA
grid.search = expand.grid(cp = seq(0, 0.01, length.out = 20))
model_arvoreDecisao3 = caret::train(EvadiuTipo ~ .,
                                    data         = data,
                                    trControl    = control,
                                    metric       = "ROC",
                                    method       = 'rpart',
                                    tuneGrid     = grid.search)

### Vamos ver os parâmetros da árvore de decisão (que podem ser otimizados)
# modelLookup('rpart')

### A cara da árvore de decisão...
rattle::fancyRpartPlot(model_arvoreDecisao1$finalModel)
rattle::fancyRpartPlot(model_arvoreDecisao2$finalModel)
rattle::fancyRpartPlot(model_arvoreDecisao3$finalModel)

###########################################################################
###########################################################################
### Juntando todos os classificadores em um comando só
models =
  caretList(EvadiuTipo ~ .,
            data = data,
            trControl = control,
            metric = "ROC",
            tuneList = list(#adaboost     = caretModelSpec(method = "adaboost"), #muito demorado para rodar
              arvoreDecisao = caretModelSpec(method = "rpart"), 
              knn           = caretModelSpec(method = "knn"),
              logit         = caretModelSpec(method = "glm", family = "binomial"),
              elasticnet    = caretModelSpec(method = "glmnet"),
              redeNeural    = caretModelSpec(method = "mlpML"),
              rf            = caretModelSpec(method = "ranger")), 
            preProcess = c("knnImpute", "nzv", "center", "scale"))

### Performance analysis ----------------------------------------------------

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
