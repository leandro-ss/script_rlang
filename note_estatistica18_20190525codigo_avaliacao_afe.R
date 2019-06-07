###############################################################################################
#                                   Análise Fatorial
#                                      Avaliação
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/Análise Fatorial/1-AFE/Dados")

#------------------------------------------------------------------------------------------#
#                                    Exercício 1                                           #

# Item A: ACP é indicada para redução de dimensionalidade do dados, normalmente etapa
# de uma análise (explica variabilidade total). AFE é indicada para redução de 
# dimensionalidade buscando fatores latentes associados às variáveis observadas (explica
# variabilidade comum). Exemplo de ACP: muito comum em Machine Learning com o objetivo
# de tornar modelos mais simples com menor número de variávei. Exemplo de AFE: muito comum
# em psiquiatria e psicologia para encontrar fatores associados à saúde mental de pacientes.

# Item B: os autovalores podem se alterar, uma vez que se busca rotacionar os fatores
# tornando-os mais interpretáveis. No entanto, as comunalidades se mantém, pois garanta-se
# a explicação do modelo escolhido inicialmente.

#------------------------------------------------------------------------------------------#
#                                    Exercício 2                                           #

# Item A
F1 <- c(-0.71, -0.72, -0.11, 0.80, 0.13, -0.75, -0.78)
F2 <- c(0.01, -0.21, -0.73, -0.20, -0.91, -0.05, -0.03)

autovalor_F1 <- sum(F1^2); autovalor_F1
autovalor_F2 <- sum(F2^2); autovalor_F2
# ambos importantes, maiores do que 1.

# Item B
var_F1 <- autovalor_F1/7; var_F1
var_F2 <- autovalor_F2/7; var_F2
var_F1+var_F2
# o primeiro fator explica 40% e o segundo explica 21%. Conjuntamente, 60% da variabilidade
# dos dados é explicada.

# Item C
comunalidade1 <- F1[1]^2 + F2[1]^2; comunalidade1 #menos explicada: hedônico
comunalidade2 <- F1[2]^2 + F2[2]^2; comunalidade2
comunalidade3 <- F1[3]^2 + F2[3]^2; comunalidade3
comunalidade4 <- F1[4]^2 + F2[4]^2; comunalidade4
comunalidade5 <- F1[5]^2 + F2[5]^2; comunalidade5 #mais explicada: açúcar
comunalidade6 <- F1[6]^2 + F2[6]^2; comunalidade6
comunalidade7 <- F1[7]^2 + F2[7]^2; comunalidade7

# Item D
# F1: associado a vinhos ácidos e hedônico, com alto teor de álcool, não tão caros e 
# indicados para carne.
# F2: associados a vinhos doces adequados para sobremesa.

#------------------------------------------------------------------------------------------#
#                                    Exercício 3                                           #

dados <- read.csv('dados_jovens.csv', sep=';', dec=',')
glimpse(dados)

# Item A
KMO(dados) #ótimo
bartlett.test(dados) #hipotése rejeitada
# sim.

# Item B
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # Em torno de 24 fatores

# Item C
fit1 <- fa(dados, 24, rotate="oblimin", fm = "pa")
fit1
# maior parte das correlações entre fatores baixas, então mais indicado varimax

fit2 <- fa(dados, 24, rotate="varimax", fm = "pa")
fit2

# Item D
# F1: Clássico: música clássica, musicais, leitura
# F2: Compras: shopping, carros, aparência
# F3: Roqueiro: rock, metal, punk
# F4: Romântico: pop e filmes românticos
# F5: Biológicas: biologia, química e medicina 
# F6: geral (folk)
# F7: História: história, filmes de guerra, científicos, documentários
# F8: Geek: PC e internet
# F9: documentários e linguas estrangeiras
# F10: folk e filmes românticos
# F11: carro
# F12: Político: política e geografia
# F13: Dançarino: dança e músicas latinas
# F14: geral
# F15: Exatas: matemática e física
# F16: geral
# F17: Gótico: filmes de terror e suspense
# F18: Alegre: filmes românticos, de comédia e fantasia
# F19: geral
# F20: Linguístico: línguas estrangeiras
# F21: Psicológico: psicologia
# F22: rock e jazz
# F23: documentários
# F24: ópera
# Como muitos fatores ficaram sem interpretação, seria indicado diminuir o número 
# de fatores mesmo com perda de variabilidade explicada. Além disso, como houve 
# 5 correlações perto de 0,30 ao realizar a rotação Oblimin, poderia-se tentar
# reduzir a rotação varimax com 19 fatores.



