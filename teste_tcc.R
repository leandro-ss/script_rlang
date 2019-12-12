
library(corrplot)
library(foreign)
library(psych)
library(dplyr)
library(corrplot)
library(GPArotation)
library(tidyr)
library(ggfortify)

# Entrada dos dados e extra??o das componentes principais com a matriz de correla??o 
# https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

dataset_ori <- read.csv('dataset/tcc_dataset.csv' ) 
dataset <- dataset_ori %>%
  scale %>%
  filter(grepl('FURTO', Natureza),grepl('1410', delegacia_id)) %>%
  rename_all(tolower) 

dataset$natureza <- tolower(dataset$natureza)

transpose_t <- dataset %>%
  select (-delegacia_id, -x, -total, -natureza) %>% 
  t %>% as.data.frame(row.names = NULL)

transpose_t$V1

correl <- cor(t(transpose_t))

corrplot(correl, order = "hclust", tl.col='black', tl.cex=.75) 
