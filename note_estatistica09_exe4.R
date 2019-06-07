##############################################################################################
#
#                                     Análise Discriminante
#
##############################################################################################

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
dados <- read_excel("dados_cereais.xlsx") 

################################### Análise Discriminante Linear ##############################
###################################      Método de Fisher        ##############################

############ Verificando Suposições
summary (dados)
# Verificando tamanho dos grupos
table(dados$Fabricante)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,4:11])

# Verificando suposição de normalidade
mshapiro.test(t(dados[,4:11])) # Dados nao seguem uma normalidade

# Verificando suposição de variâncias iguais
boxM(data = dados[,4:11], grouping = dados$Fabricante) # Ele segue abaixo de 1% ou seja Nao há homoginidade na Covariancia das Metricas

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,4:11], group = dados$Fabricante) # Calorias, 

######## Ajuste das funções discriminantes

# Modelo discriminante linear
#discrim_1 <- desDA(variables = dados[, c("Sodio", "Carboidrato")], group = dados$Fabricante)
discrim_1 <- desDA(variables = dados[, c("Calorias","Proteina","Gordura","Sodio","Fibra","Carboidrato","Acucar","Potassio")], group = dados$Fabricante)

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpretação
discrim_1$discor

# Centróides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Fabricante) %>% summarise(C1= mean(DF1), c2=mean(DF2) )

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()


# 4) A planilha dados_cereais.xlsx contém dados sobre cereais matinais
# produzidos por três diferentes fabricantes americanos: General Mills
# (G), Kellogg (k) e Quaker (Q). Assuma que os dados seguem Normal
# Multivariada com uma matriz de covariância comum. Queremos
# discriminar os três fabricantes com as variáveis Calorias, Proteína,
# Gordura, Sódio, Fibra, Carboidratos, Açúcar e Potássio. Responda:
#   ??? (a) Quais variáveis discriminam melhor os grupos?
#### R: Sodio e Carboidrato
#   ??? (b) Antes de realizar a análise, quantas funções discriminantes você obterá?
#### R: 2 funcoes discriminates considerando somente as variaveis que melhor representam o modelo
#   ??? (c) Determine e interprete as funções discriminantes. Avalie também a
# significância das funções e comente.
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
# ??? (d) Parece que algum fabricante está associado com mais "cereais nutritivos"
# (maiores quantidades de proteína e fibra, menores quantidades de gordura e
#  açúcar e assim por diante) do que outros?

#   ??? (e) Construa o gráfico dos cereais no espaço bidimensional discriminante
# usando diferentes símbolos para identificar os três fabricantes. Interprete.

# Scatterplot
# library(ggplot2)
# ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
#   geom_hline(yintercept = 0, colour="gray70") +
#   geom_vline(xintercept = 0, colour="gray70") +
#   geom_point()

