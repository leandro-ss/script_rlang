##############################################################################################
#
#                                     Análise Discriminante
#
##############################################################################################
install.
# Pacotes para análise
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

################################### Análise Discriminante Linear ##############################
###################################      Método de Fisher        ##############################

############ Verificando Suposições

# Verificando tamanho dos grupos
table(dados$RACA)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,2:3])

# Verificando suposição de normalidade
shapiro.test(t(dados[,-1]))

# Verificando suposição de variâncias iguais
boxM(data = dados[, -1], grouping = dados$RACA)

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$RACA)

######## Ajuste das funções discriminantes

# Modelo discriminante linear
discrim_1 <- desDA(variables = dados[, c("CP", "CD")], group = dados$RACA)

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values
  
# Matriz de fatores para interpretação
discrim_1$discor

# Centróides
dados$DF <- discrim_1$scores[,1]
dados %>% group_by(RACA) %>% summarise(C = mean(DF))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes( x = DF, y = DF, colour = RACA)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Avaliação do ajuste
# Testes da significância das funções discriminantes
# Classificação real
Y_real <- dados$RACA

escores <- discrim_1$scores
summary(aov(escores ~ Y_real),test="Wilks")
summary(aov(escores ~ Y_real),test="Hotelling-Lawley")
summary(aov(escores ~ Y_real),test="Hotelling-Lawley")



################################
# Exercicios
################################

# 3) Considere os dados referentes a duas raças de insetos (A e B) com
# variáveis independentes o número médio de cerdas primordiais (CP) e o
# número médio de cerdas distais (CD). Os dados se encontram na
# planilha dados_insetos.xlsx.
# ??? (a) Antes de inicializar a análise, quantas funções discriminantes serão
# obtidas?
### R: Será obtida somente uma função discriminante
#   ??? (b) Determine e interprete as funções discriminantes dessas duas raças pelo
# método de Fisher selecionando todas as variáveis para a análise.
### R: Foi gerada somente uma função discriminante com poder igual a 100
# ??? (c) Analise o poder de discriminação das variáveis.
### R: O poder de discriminação das variaveis segue com proporcoes similares entre as variaveis  
# ??? (d) Analise a significâncias das funções discriminantes.
### R: No caso a função discriminante é significativo 

