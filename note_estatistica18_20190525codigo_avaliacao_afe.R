###############################################################################################
#                                   An�lise Fatorial
#                                      Avalia��o
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/An�lise Fatorial/1-AFE/Dados")

#------------------------------------------------------------------------------------------#
#                                    Exerc�cio 1                                           #

# Item A: ACP � indicada para redu��o de dimensionalidade do dados, normalmente etapa
# de uma an�lise (explica variabilidade total). AFE � indicada para redu��o de 
# dimensionalidade buscando fatores latentes associados �s vari�veis observadas (explica
# variabilidade comum). Exemplo de ACP: muito comum em Machine Learning com o objetivo
# de tornar modelos mais simples com menor n�mero de vari�vei. Exemplo de AFE: muito comum
# em psiquiatria e psicologia para encontrar fatores associados � sa�de mental de pacientes.

# Item B: os autovalores podem se alterar, uma vez que se busca rotacionar os fatores
# tornando-os mais interpret�veis. No entanto, as comunalidades se mant�m, pois garanta-se
# a explica��o do modelo escolhido inicialmente.

#------------------------------------------------------------------------------------------#
#                                    Exerc�cio 2                                           #

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
# dos dados � explicada.

# Item C
comunalidade1 <- F1[1]^2 + F2[1]^2; comunalidade1 #menos explicada: hed�nico
comunalidade2 <- F1[2]^2 + F2[2]^2; comunalidade2
comunalidade3 <- F1[3]^2 + F2[3]^2; comunalidade3
comunalidade4 <- F1[4]^2 + F2[4]^2; comunalidade4
comunalidade5 <- F1[5]^2 + F2[5]^2; comunalidade5 #mais explicada: a��car
comunalidade6 <- F1[6]^2 + F2[6]^2; comunalidade6
comunalidade7 <- F1[7]^2 + F2[7]^2; comunalidade7

# Item D
# F1: associado a vinhos �cidos e hed�nico, com alto teor de �lcool, n�o t�o caros e 
# indicados para carne.
# F2: associados a vinhos doces adequados para sobremesa.

#------------------------------------------------------------------------------------------#
#                                    Exerc�cio 3                                           #

dados <- read.csv('dados_jovens.csv', sep=';', dec=',')
glimpse(dados)

# Item A
KMO(dados) #�timo
bartlett.test(dados) #hipot�se rejeitada
# sim.

# Item B
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # Em torno de 24 fatores

# Item C
fit1 <- fa(dados, 24, rotate="oblimin", fm = "pa")
fit1
# maior parte das correla��es entre fatores baixas, ent�o mais indicado varimax

fit2 <- fa(dados, 24, rotate="varimax", fm = "pa")
fit2

# Item D
# F1: Cl�ssico: m�sica cl�ssica, musicais, leitura
# F2: Compras: shopping, carros, apar�ncia
# F3: Roqueiro: rock, metal, punk
# F4: Rom�ntico: pop e filmes rom�nticos
# F5: Biol�gicas: biologia, qu�mica e medicina 
# F6: geral (folk)
# F7: Hist�ria: hist�ria, filmes de guerra, cient�ficos, document�rios
# F8: Geek: PC e internet
# F9: document�rios e linguas estrangeiras
# F10: folk e filmes rom�nticos
# F11: carro
# F12: Pol�tico: pol�tica e geografia
# F13: Dan�arino: dan�a e m�sicas latinas
# F14: geral
# F15: Exatas: matem�tica e f�sica
# F16: geral
# F17: G�tico: filmes de terror e suspense
# F18: Alegre: filmes rom�nticos, de com�dia e fantasia
# F19: geral
# F20: Lingu�stico: l�nguas estrangeiras
# F21: Psicol�gico: psicologia
# F22: rock e jazz
# F23: document�rios
# F24: �pera
# Como muitos fatores ficaram sem interpreta��o, seria indicado diminuir o n�mero 
# de fatores mesmo com perda de variabilidade explicada. Al�m disso, como houve 
# 5 correla��es perto de 0,30 ao realizar a rota��o Oblimin, poderia-se tentar
# reduzir a rota��o varimax com 19 fatores.



