###############################################################################################
#                        Modelagem de equa??es estruturais
###############################################################################################

##### Lavaan modeling
library(lavaan)
################### Exerc?cio 1 ##################
dat <- read.csv("dataset/dados-mentais.csv", sep =";") %>% na.omit %>% scale
mod.1 <- 'verb =~ x1 + x2 + x3
          math =~ y1 + y2 + y3
          verb ~~ math'
mod.1.fit <- sem(mod.1, data=dat, std.lv= TRUE) #FIXANDO AS CARGAS FATORIAIS
summary(mod.1.fit, fit.measures = TRUE, modindices = TRUE) 
standardizedSolution(mod.1.fit, type = 'std.all')
