#Avaliacao exercicio 3
require(tidyverse)
library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

dados_jovens <- read.csv("dataset/dados_jovens.csv", sep=";")
glimpse(dados_jovens)

# 3. No ano de 2013, 1010 jovens eslovacos de 15 a 30 anos participaram de uma
# pesquisa acerca de hobbies, personalidades, opiniões, hábitos de consumo e
# preferências quanto a música e filmes. Um questionário com 90 questões foi
# aplicado, em que as respostas eram escores de 1 a 5, com 1 correspondendo a
# “Discordo totalmente”, “Não gosto” ou “Não tenho interesse” e 5 correspondendo
# a “Concordo totalmente”, “Gosto muito” ou “Muito interessado”. Na planilha
# “variáveis_jovens.xlsx” encontram�se as questões aplicadas na pesquisa e seus
# respectivos nomes correspondentes na planilha “dados_jovens.csv”.

# a) Analisando o KMO e o teste de esfericidade, podemos descrever essa grande
# quantidade de interesses humanos em um número menor de conceitos latentes?
bartlett.test(dados_jovens)
# hipotése rejeitada, as correlações sao diferentes de 0
KMO(dados_jovens) #ótimo
KMO(dados_jovens)$MSAi %>% plot; abline(h=0.7, col='green')
# ok pois estão acima de 0.7
# b) Quantos fatores podem ser utilizados no modelo?
fit_acp <- princomp(na.omit(dados_jovens), cor=TRUE)
summary(fit_acp) # variâncias (autovalores)
summary(fit_acp)$sdev %>% plot(main='Standart Deviation')
abline(h=1, col='green')
# 24 fatores


loadings(fit_acp) # cargas fatoriais
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot

# c) Estime as cargas dos fatores por Eixos Principais e escolha
# um método de rotação (Varimax ou Oblimin) e explique por que é mais
# adequado para a análise dos dados.
#oblíquo
fa_oblimin <- fa(dados_jovens, 24, rotate="oblimin", fm = "pa");fa_oblimin
sum(if_else(fa_oblimin$score.cor > 0.7,1,0))
# não tem correlação entre os fatores, entao seguir com a rotação ortogonal
#ortogonal
fa_varimax <- fa(dados_jovens, 24, rotate="varimax", fm = "pa")

loadings <- fa_varimax$loadings
load <- loadings %>% as.matrix.data.frame()
write.table(load,file="loadings_jovens.csv", sep=';')