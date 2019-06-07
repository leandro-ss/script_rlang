###############################################################################################
#                         An?lise de Componentes Principais
###############################################################################################

system('defaults write org.R-project.R force.LANG en_US.UTF-8')

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)
library(ggfortify)

# Entrada dos dados e extra??o das componentes principais com a matriz de correla??o 
dados <- read.csv('dataset/dados-evaporador-industrial.csv', sep = ';', dec = ',')
glimpse(dados)

# Teste de esfericidade de Bartllet
bartlett.test(dados)

# Ajuste do modelo - escolha do n?mero de componentes
ajuste1 <- princomp(dados, cor=TRUE)
# Impress?o da vari?ncia explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines") 

# Ajuste do modelo com n?mero de componentes definido
ajuste2 <- principal(dados, covar = FALSE, nfactors = 3)

# Interpreta??o (correla??es)
ajuste2

# Comunalidades
ajuste2$communality

# Gr?ficos
autoplot(prcomp(scale(dados)))
autoplot(prcomp(scale(dados)), data = dados, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)