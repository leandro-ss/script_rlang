##############################################################################################
#
#                                     An?lise Discriminante
#
##############################################################################################
install.
# Pacotes para an?lise
library(foreign)
library(MASS)
library(dplyr)
library(biotools)
library(DiscriMiner)
library(mvnormtest)
library(ggplot2)

# Leitura dos dados
library(readxl)
dados <- read_excel("dados_insetos.xlsx") 

################################### An?lise Discriminante Linear ##############################
###################################      M?todo de Fisher        ##############################

############ Verificando Suposi??es

# Verificando tamanho dos grupos
table(dados$RACA)

# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,2:3])

# Verificando suposi??o de normalidade
shapiro.test(t(dados[,-1]))

# Verificando suposi??o de vari?ncias iguais
boxM(data = dados[, -1], grouping = dados$RACA)

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$RACA)

######## Ajuste das fun??es discriminantes

# Modelo discriminante linear
discrim_1 <- desDA(variables = dados[, c("CP", "CD")], group = dados$RACA)

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values
  
# Matriz de fatores para interpreta??o
discrim_1$discor

# Centr?ides
dados$DF <- discrim_1$scores[,1]
dados %>% group_by(RACA) %>% summarise(C = mean(DF))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes( x = DF, y = DF, colour = RACA)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
# Classifica??o real
Y_real <- dados$RACA

escores <- discrim_1$scores
summary(aov(escores ~ Y_real),test="Wilks")
summary(aov(escores ~ Y_real),test="Hotelling-Lawley")
summary(aov(escores ~ Y_real),test="Hotelling-Lawley")

################################
# Exercicios
################################

# 3) Considere os dados referentes a duas ra?as de insetos (A e B) com
# vari?veis independentes o n?mero m?dio de cerdas primordiais (CP) e o
# n?mero m?dio de cerdas distais (CD). Os dados se encontram na
# planilha dados_insetos.xlsx.
# ??? (a) Antes de inicializar a an?lise, quantas fun??es discriminantes ser?o
# obtidas?
### R: Ser? obtida somente uma fun??o discriminante
#   ??? (b) Determine e interprete as fun??es discriminantes dessas duas ra?as pelo
# m?todo de Fisher selecionando todas as vari?veis para a an?lise.
### R: Foi gerada somente uma fun??o discriminante com poder igual a 100
# ??? (c) Analise o poder de discrimina??o das vari?veis.
### R: O poder de discrimina??o das variaveis segue com proporcoes similares entre as variaveis  
# ??? (d) Analise a signific?ncias das fun??es discriminantes.
### R: No caso a fun??o discriminante ? significativo 

