# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("stargazer")
# install.packages("skimr")

library(ggplot2)
library(data.table)
library(stargazer)
library(skimr)
library(readxl)
library(dplyr)

dtAmostra <- fread("dadosBanco/Amostra_Modelo_Evasao_Correntistas_v3.csv")

dtAmostra$Evadiu <- as.factor(dtAmostra$Evadiu)
dtAmostra$Debito_Automatico <- as.factor(dtAmostra$Debito_Automatico)
dtAmostra$Credito_Salario <- as.factor(dtAmostra$Credito_Salario)
dtAmostra$Credenciamento <- as.factor(dtAmostra$Credenciamento)
dtAmostra$Caixa_Seguradora <- as.factor(dtAmostra$Caixa_Seguradora)
dtAmostra$Pediu_Portabilidade <- as.factor(dtAmostra$Pediu_Portabilidade)
dtAmostra$Abriu_Reclamacao <- as.factor(dtAmostra$Abriu_Reclamacao)
dtAmostra$Debito_Automatico_DIF <- as.factor(dtAmostra$Debito_Automatico_DIF)
dtAmostra$Credito_Salario_DIF <- as.factor(dtAmostra$Credito_Salario_DIF)
dtAmostra$Caixa_Seguradora_DIF <- as.factor(dtAmostra$Caixa_Seguradora_DIF)


dtAmostra =
  dtAmostra %>%
  mutate(winsored_Rentabilidade_PERC = psych::winsor(Rentabilidade_PERC, 0.1))

dtAmostra =
  dtAmostra %>%
  mutate(winsored_Produtos = psych::winsor(Produtos, 0.01))

str(dtAmostra)

ggplot(data = dtAmostra, aes(Evadiu, winsored_Produtos)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.2) +
  geom_boxplot(aes(fill = Segmento)) +
  labs(title = "Box Plot",
       subtitle = "Evadiu vs Rentabilidade PERC",
       x = "Evadiu",
       y = "Rentabilidade PERC") +
  theme_bw() +
  facet_wrap(~ Credito_Salario)


box_plot = function(data, atributos, variavelDependente = 'Evadiu'){
  for (atributo in atributos) {
    # if (atributo != "Id") || (atributo != "Evadiu"){
      result = ggplot(data = dtAmostra, aes_string(variavelDependente, atributo)) + 
        geom_boxplot() +
        labs(x = variavelDependente, 
             y = atributo,
             caption = "XXX") +
        theme_bw()
      print(result)
    # }
  }
}

box_plot(credit, c(dtAmostra$Segmento))

