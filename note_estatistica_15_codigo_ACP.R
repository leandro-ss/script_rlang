###############################################################################################
#                         Análise de Componentes Principais
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/Análise de Componentes Principais/ACP1 - Alunos")

# Entrada dos dados e extração das componentes principais com a matriz de correlação 
dados <- read.csv('dados-evaporador-industrial.csv', sep = ';', dec = ',')
glimpse(dados)

# Teste de esfericidade de Bartllet
bartlett.test(dados)

# Ajuste do modelo - escolha do número de componentes
ajuste1 <- princomp(dados, cor=TRUE)
# Impressão da variância explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines") 

# Ajuste do modelo com número de componentes definido
ajuste2 <- principal(dados, covar = FALSE, nfactors = 3)

# Interpretação (correlações)
ajuste2

# Comunalidades
ajuste2$communality

# Gráficos
library(ggfortify)
autoplot(prcomp(scale(dados)))
autoplot(prcomp(scale(dados)), data = dados, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)






