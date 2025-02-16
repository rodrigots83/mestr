## INSTALANDO PACOTES NECESSÁRIOS
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("textrank")
# install.packages("rvest")

## CARREGANDO OS PACOTES NECESSÁRIOS
library(tidyverse)
library(tidytext)
library(textrank)
library(rvest)

## LENDO O DOCUMENTO QUE NECESSITA SER SUMARIZADO

dados = read.csv(file = "Trends_pesquisa.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

palavras <- as.list(dados[1,], mode = "list")

palavras <- tm::removeWords(x = tolower(palavras),  words = stopwords::stopwords("en"))

palavras

final <- ""
resultado <- ""

# for(i in 1:length(dados)){
#   # final = tm::removeWords(x = tolower(dados[1,i]),  words = stopwords::stopwords("en"))
#   # final = tm::removeWords(x = tolower(dados[1,i]),  words = stopwords::stopwords("pt"))
#   # 
#   # final = tm::removeWords(x = tolower(dados[1,i]),  words = c('different', 'due', 'increase', 'issues', 'international', 'leading', 'major', 'model', 'movements', 'possibility', 'replacing', 'role', 'support', 'union', 'use', 'varios', 'various', 'continente', 'life', 'movements'))
#   # final = tm::removePunctuation(x = final)
#   # 
#   # final = gsub(' ai ', 'artificial_intelligence', final)
#   # final = gsub('artificial intelligence', 'artificial_intelligence', final)
#   # final = gsub('south america', 'south_america', final)
#   # final = gsub('south american', 'south_america', final)
#   # final = gsub('latin america', 'latin_america', final)
#   # final = gsub('latin american', 'latin_america', final)
#   # final = gsub('climate change', 'climate_change', final)
#   # final = gsub('financial crises', 'financial_crises', final)
#   # final = gsub('middle east', 'middle_east', final)
#   # final = gsub('energy matrix', 'matrix', final)
#   # final = gsub('energy model', 'matrix', final)
#   # 
#   # final = gsub('tecnological', 'technologies', final)
#   # final = gsub('technological', 'technologies', final)
#   # final = gsub('wars', 'war', final)
#   # final = gsub('growing', 'growth', final)
#   # final = gsub('migratory', 'migration', final)
#   # final = gsub('chineses', 'china', final)
#   # final = gsub('chinese', 'china', final)
#   # final = gsub('powers', 'power', final)
#   # final = gsub('break', 'disruption', final)
#   # resultado <- paste(resultado, final)
# }

# print(resultado)

sentencas <- tibble(line = 1:length(palavras), text = palavras) %>% unnest_tokens(word, text) %>% count(word, sort = TRUE) %>% filter(n > 10) %>% mutate(word = reorder(word, n))

ggplot(data = sentencas, aes(word, n)) + geom_col() + coord_flip() 

View(sentencas)

