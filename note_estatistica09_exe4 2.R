##############################################################################################
#
#                                     An?lise Discriminante
#
##############################################################################################

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
dados <- read_excel("dados_cereais.xlsx") 

################################### An?lise Discriminante Linear ##############################
###################################      M?todo de Fisher        ##############################

############ Verificando Suposi??es
summary (dados)
# Verificando tamanho dos grupos
table(dados$Fabricante)

# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,4:11])

# Verificando suposi??o de normalidade
mshapiro.test(t(dados[,4:11])) # Dados nao seguem uma normalidade

# Verificando suposi??o de vari?ncias iguais

boxM(data = dados[,4:11], grouping = dados$Fabricante) # Ele segue abaixo de 1% ou seja Nao h? homoginidade na Covariancia das Metricas

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,4:11], group = dados$Fabricante) # Calorias, 

######## Ajuste das fun??es discriminantes

# Modelo discriminante linear
#discrim_1 <- desDA(variables = dados[, c("Sodio", "Carboidrato")], group = dados$Fabricante)
discrim_1 <- desDA(variables = dados[, c("Calorias","Proteina","Gordura","Sodio","Fibra","Carboidrato","Acucar","Potassio")], group = dados$Fabricante)

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

# Centr?ides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Fabricante) %>% summarise(C1= mean(DF1), c2=mean(DF2) )

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()


# 4) A planilha dados_cereais.xlsx cont?m dados sobre cereais matinais
# produzidos por tr?s diferentes fabricantes americanos: General Mills
# (G), Kellogg (k) e Quaker (Q). Assuma que os dados seguem Normal
# Multivariada com uma matriz de covari?ncia comum. Queremos
# discriminar os tr?s fabricantes com as vari?veis Calorias, Prote?na,
# Gordura, S?dio, Fibra, Carboidratos, A??car e Pot?ssio. Responda:
#   ??? (a) Quais vari?veis discriminam melhor os grupos?
#### R: Sodio e Carboidrato
#   ??? (b) Antes de realizar a an?lise, quantas fun??es discriminantes voc? obter??
#### R: 2 funcoes discriminates considerando somente as variaveis que melhor representam o modelo
#   ??? (c) Determine e interprete as fun??es discriminantes. Avalie tamb?m a
# signific?ncia das fun??es e comente.
### R:
# F1 maior poder de correlacao com Carboidrato, Sodio e Calorias
# F2 sodio tem poder discriminante alto em relacao 
                  #              DF1   DF2
                  # Calorias    -0.51 -0.37
                  # Sodio       -0.52 -0.58
                  # Proteina    -0.08  0.09
                  # Gordura      0.35 -0.30
                  # Fibra       -0.26  0.21
                  # Acucar      -0.30 -0.26
                  # Potassio    -0.23 -0.09
                  # Carboidrato -0.56 -0.31
# ??? (d) Parece que algum fabricante est? associado com mais "cereais nutritivos"
# (maiores quantidades de prote?na e fibra, menores quantidades de gordura e
#  a??car e assim por diante) do que outros?

#   ??? (e) Construa o gr?fico dos cereais no espa?o bidimensional discriminante
# usando diferentes s?mbolos para identificar os tr?s fabricantes. Interprete.

# Scatterplot
# library(ggplot2)
# ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
#   geom_hline(yintercept = 0, colour="gray70") +
#   geom_vline(xintercept = 0, colour="gray70") +
#   geom_point()

