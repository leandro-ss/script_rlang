##############################################################################################
#
#                                     An?lise Discriminante
#
##############################################################################################

# Pacotes para an?lise

library(foreign)
library(MASS)
library(dplyr)
library(heplots)
library(DiscriMiner)
library(ggplot2)
library(readxl)

dados <- read_excel("dataset/dados_insetos.xlsx") %>% select (raca, cp,cd)

################################### An?lise Discriminante Linear ##############################
###################################      M?todo de Fisher        ##############################

############ Verificando Suposi??es

# Verificando tamanho dos grupos
table(dados$job)
dados$Raça
# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,1:3])

# Verificando suposi??o de normalidade
shapiro.test(t(dados[,-4]))

# Verificando suposi??o de vari?ncias iguais
boxM(dados[, 1:3], dados[,"job"])

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,1:3], group = dados$job)

######## Ajuste das fun??es discriminantes

# Modelo discriminante linear
discrim_1 <- desDA(variables = dados[, c("outdoor", "social", "conservative")], 
                   group = dados$job)

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

# Centr?ides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(job) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
ggplot(data = dados, aes(x = DF1, y = DF2, colour = job)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
# Classifica??o real
Y_real <- dados$job

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")
summary(manova(escores ~ Y_real),test="Hotelling-Lawley")
summary.aov(manova(escores ~ Y_real),test="Hotelling-Lawley")

############### Classifica??o - Fun??es de classifica??o
# Ajuste total
fit <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
             group = dados$job)

# Ajuste com cross-validaton
fit2 <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
              group = dados$job, validation = 'crossval')

# Ajuste com amostras treinamento/teste
treinamento <- sample(seq_len(nrow(dados)), size = 183) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
              group = dados$job, validation = 'learntest', learn = treinamento,
              test = teste)

# Fun??es de classifica??o
fit$functions

# Matriz de confus?o
fit$confusion

# Taxa de erro
fit$error_rate
fit2$error_rate
fit3$error_rate

# Classifica??o prevista
Y_previsto <- fit$classification

# Classificando um novo objeto
classify(fit, newdata = data.frame(outdoor = 15, social = 10, conservative = 2))$pred_class

# Modelo discriminante quadr?tico
discrim_quad <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                      group = dados$job)
discrim_quad2 <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                       group = dados$job, validation = 'crossval')
discrim_quad3 <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                       group = dados$job, validation = 'learntest',  learn = treinamento,
                       test = teste)
discrim_quad$error_rate
discrim_quad2$error_rate
discrim_quad3$error_rate





