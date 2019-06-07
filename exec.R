###############################################################################################
#                        Modelagem de equa??es estruturais
###############################################################################################

##### Lavaan modeling
library(lavaan)
library(dbplyr)

################### Exerc?cio 1 ##################
dat <- read.csv("dataset/dados_canada.csv", sep=";", dec=",")  %>% select(ends_with(".2")) 

# Step 1: Specify model
mod.1 <- 'score =~ MBSA2.2 + MBSA7.2 + MBSA8.2 + MBSA9.2'
mod.1.fit <- sem(mod.1, data=dat, )
summary(mod.1.fit, fit.measures = TRUE) 
standardizedSolution(mod.1.fit, type = 'std.all')
#Identificação do Modelo
#Limite: 4(4+1)/2 = 10;
# 4 variancias + 4 caminhos 1 (fixando o valor do fator por padrao)
#Identificacao do mdl 3

#R: Felicidade varia em 0.745 em relacao ao Hemisferio, sendo a variancia seguindo o sentido alfabetico, reduzindo em -0.74 no sentido norte sul.

mod.2 <- 'Happiness.Score ~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity'
mod.2.fit <- sem(mod.2, data=dat)
summary(mod.2.fit, fit.measures = TRUE, modindices = TRUE) 
standardizedSolution(mod.2.fit, type = 'std.all')
#Felicidade em razao dos estimadores,
#Felicidade = 0.86 * economia + 1.41 * Familia + 0.97 * Saude + 1.33 * Liberdade +  0.78 * confianca + 0.39 * Generosidade
# Poderiamos desconsiderar a generosidade visto que ela nao eh significante para o modelo

# Step 3: Extract results

# Step 4: Request Modification Indices
summary(mod.1.fit, modindices = TRUE)

####


