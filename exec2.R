###############################################################################################
#                        Modelagem de equa??es estruturais
###############################################################################################

##### Lavaan modeling
library(lavaan)
library(dbplyr)
library(semplot)

################### Exerc?cio 1 ##################
dat <- read.csv("dataset/dados-democracia.csv", sep=";", dec=",")  %>% scale

#Identificação do Modelo
#Limite: 14(14+1)/2 = 14*15/2 = 105;
#Identificacao do moodelo  caminho 18 + 14 = 32

mod.1 <- 'inds60 =~ 1*x1 + x2 + x3
          demo60 =~ 1*y1 + y2 + y3 + y4
          demo65 =~ 1*y5 + y6 + y7 + y8 
          demo60 ~ inds60
          demo65 ~ inds60 + demo60
          y1 ~~ y5
          y2 ~~ y4
          y2 ~~ y6
          y6 ~~ y8'

# Step 1: Specify model

mod.1.fit <- sem(mod.1, data=dat, std.lv =FALSE)
summary(mod.1.fit, fit.measures = TRUE, modindices = TRUE) 
standardizedSolution(mod.1.fit, type = 'std.all')
