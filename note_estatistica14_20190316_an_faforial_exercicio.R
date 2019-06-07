###############################################################################################
#                           An?lise Fatorial Explorat?ria
###############################################################################################

if(!require(foreign)){install.packages("foreign")}
if(!require(psych)){install.packages("psych")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(stats)){install.packages("stats")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(mvnormtest)){install.packages("mvnormtest")}
if(!require(GPArotation)){install.packages("GPArotation")}

dados = read.csv('dataset/dados_personalidade_32.csv', sep = ';', dec = ',')
glimpse(dados)

# Criterios para o uso
# mais de 50 observacoes
nrow(dados)
# 10 casos para cada variavel
nrow(dados)/ncol(dados) 
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(dados) #Otimo
bartlett.test(dados) #hipot?se rejeitada

#### M?todo de extra??o: An?lise de Componentes Principais (n?o rotacionado) ###
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # vari?ncias (autovalores)
loadings(fit_acp) # cargas fatoriais 
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot 
################################################################################


############# M?todo de extra??o: eixos principais - sem rota??o ###############
fit_ep <- fa(dados, 8, rotate="none", fm = "pa"); fit_ep
################################################################################


################## Componentes Principais com Rota??o Varimax ##################
fit_acp_var <- principal(na.omit(dados), nfactors=8, rotate="varimax")
fit_acp_var # print results

# comunalidades
fit_acp_var$communality

# vari?ncia n?o explicada
fit_acp_var$uniquenesses

# escores dos fatores
fit_acp_var$scores
################################################################################


###### M?todo de extra??o: eixos principais - com rota??o Varimax ##############
fit_ep <- fa(dados, 8, rotate="varimax", fm = "pa")
fit_ep

# autovalores
fit_ep$e.values

# cargas
fit_ep$loadings

fit_ep$scores

# comunalidades
fit_ep$communality

# vari?ncia n?o explicada
fit_ep$uniquenesses

# escores dos fatores
fit_ep$scores
################################################################################

