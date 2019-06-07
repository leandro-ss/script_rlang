tabela <- data.frame(Observ = 1:6,
                     F1 = c(0.12,0.07,0.18,0.93,0.77,0.62),
                     F2 = c(0.85,0.74,0.67,0.21,0.05,0.08)); tabela
exp(tabela[c('F1','F2')])

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

dados = na.omit(read.csv('dataset/dados_animais.csv', sep = ';', dec = ',')); dados
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
fit_ep <- fa(dados, 2, rotate="none", fm = "pa")
fit_ep$loadings
################## Componentes Principais com Rota??o Varimax ##################
fit_acp_var <- principal(na.omit(dados), nfactors=2, rotate="varimax"); fit_acp_var # print results
fit_acp_var$loadings
###### M?todo de extra??o: eixos principais - com rota??o Varimax ##############
fit_ep <- fa(dados, 2, rotate="varimax", fm = "pa")
fit_ep$loadings
################################################################################

bind_cols(fit_ep$loadings, fit_acp_var$loadings, fit_ep$loadings)
as.data.frame(unclass(fit_ep$loadings))[,order('PA1')]
as.data.frame(unclass(fit_ep$loadings))

order(mpg)
