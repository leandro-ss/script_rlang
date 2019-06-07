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
dados_ori <- read.csv('dataset/dados_mc.csv', sep = ';', dec = '.');glimpse(dados)

dados <- dados_ori[,4:24]
glimpse(dados)
# Teste de esfericidade de Bartllet
bartlett.test(dados)

# Ajuste do modelo - escolha do n?mero de componentes
ajuste1_matriz_cor   <- princomp(dados, cor=TRUE)

# Impress?o da vari?ncia explicada
summary(ajuste1_matriz_cor) 

#kaiser 4/ vaar 2 /plot 3

# Scree Plot
plot(ajuste1_matriz_cor, type="lines") 

# Ajuste do modelo com n?mero de componentes definido # Interpreta??o (correla??es)
ajuste2_cor   <- principal(dados, covar = FALSE, nfactors = 3);ajuste2_cor

# Comunalidades
cbind(ajuste2_cor$communality)

# Gr?ficos
autoplot(prcomp(scale(dados)))
autoplot(prcomp(scale(dados)), data = dados, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



teste_data <- cbind(dados_ori , ajuste2_cor$scores) ; view (teste_data)
names(teste_data)

fit <- lm(RC1 ~ Calories + Calories.from.Fat , data=teste_data[teste_data$Category == 'Beef & Pork',]); summary(fit)
# Ainda falta exemplificar da categoria no processo