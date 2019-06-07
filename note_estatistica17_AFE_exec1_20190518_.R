###############################################################################################
#                        Modelagem de equa??es estruturais
###############################################################################################

##### Lavaan modeling
library(lavaan)
################### Exerc?cio 1 ##################
dat <- read.csv("dataset/dados-alimentacao.csv", sep=";" , dec = ",") %>% scale()
mod.1.fit <- sem('Q~P+D', data=dat); summary(mod.1.fit, fit.measures = TRUE) 
standardizedSolution(mod.1.fit, type = 'std.all')


#CFI = 1 ; Se maior q 0.8  ajuste bom
#TLI = 1 ; ajuste perfeito
#RMSEA = 0; necessario abaixo 0.05
#SRMR = 0; necessario abaixo 0.05

