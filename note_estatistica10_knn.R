#                                     An?lise Discriminante

# Pacotes para an?lise
library(foreign)
library(MASS)
library(dplyr)
library(heplots)
library(DiscriMiner)
library(mvnormtest)
library(ggplot2)
library(readxl)
############ Verificando Suposi??es
dados <- read_excel("dataset/dados_alunos.xlsx") #%>% select(candidato, grupo, nota, historico)
summary (dados)
colnames(dados)[2] = "Grupo"
colnames(dados)[3] = "Tecnica"
colnames(dados)[4] = "Historico"
dados$Grupo = as.factor(dados$Grupo)
table(dados$Grupo)

aggregate(dados[,3:4], list(dados$Grupo), mean)
################################### An?lise Discriminante Linear ##############################
###################################      M?todo de Fisher        ##############################
#7) Um programa de pós-graduação deseja alterar o método de seleção dos seus alunos para u
#ma prova de conhecimento técnico e uma nota atribuída ao histórico escolar do candidato. Para is
#o, os 63 candidatos do ano anterior foram divididos em três grupos: (1) constituídos dos
#candidatos aprovados no programa; (2) constituído pelos candidatos não aprovados,
#mas que ficaram na lista de espera e (3) constituído pelos candidatos não aprovados para o progama
#O objetivo do estudo é verificar se o novo método de seleção é capaz de discriminar bem os candidatos.
#Os dados desse exercício estão em dados_alunos.xlsx.
############ Verificando Suposi??es

#(A) Qual variável parece diferenciar mais o grupo analisando as médias? (
  #Tecnica contudo a diferenca se mostra muito baixa: summary (dados)
#(B) Obtenha e interprete as funções discriminantes. (
  # Segue somente uma funcao discriminante com representatividade
  # A primeira funcao foi significante para as 2 variaveis 
#(C) Qual o percentual da variância é explicada pelas funções discriminantes? (
  # A primeira DF e responsavel  99% da variancia explicada 
#(D) Avalie a significâncias das funções discriminantes. (
  # E de 2. 2e-16 que é significante
#(E) Construa e analise o gráfico scatterplot com as funções discriminantes. (
  # Foi construida uma reta visto a presenca de somente uma funcao descriminante,
  # Tendo os pontos sidos distribuidos sobre a reta e esses visualmente bem separados.
# (F) De acordo com as funções de classificação, onde você classificaria um aluno que apresentasse nota técnica igual a 15 e nota 8 no histórico escolar? (
  # Seria classificado no grupo 2.  
#(G) Por meio da matriz de classificação, qual a assertividade da análise? (
  # Minha assertividade eh de 91%.
#(H) A probabilidade de acerto ao classificar uma nova observação é igual à assertividade obtida pela matriz de classificação? (
  # Nao

# Matriz de correlaces (suposicao de nao multicolinearidade)# Considerar somente variaveis qualitativas
cor(dados[,3:4])
# Verificando suposi??o de normalidade
mshapiro.test(t(dados[,3:4]))
########### Sele??o das vari?veis # Medidas de qualidade
discPower(variables = dados[,3:4], group = dados$Grupo)
######## Ajuste das fun??es discriminantes # Modelo discriminante linear
discrim_1 <- desDA(variables = dados[, c("Tecnica","Historico")], group = dados$Grupo)

# Coeficientes das funcoes discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

# Centr?ides
dados$DF1 <- discrim_1$scores[,1]
dados$DF1 <- discrim_1$scores[,2]
dados %>% group_by(Grupo) %>% summarise(C1= mean(DF1) , C1= mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = Grupo)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_point()

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
Y_real <- dados$Grupo

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")
summary(manova(escores ~ Y_real),test="Hotelling-Lawley")
summary.aov(manova(escores ~ Y_real),test="Hotelling-Lawley")



############### Classifica??o - Fun??es de classifica??o

set.seed(1)

# Ajuste total
fit <- linDA(variables =  dados[, c("Tecnica","Historico")], group = dados$Grupo)
# Ajuste com cross-validaton
fit2 <- linDA(variables =  dados[, c("Tecnica","Historico")], group = dados$Grupo, validation = 'crossval')
# Ajuste com amostras treinamento/teste
treinamento <- sample(seq_len(nrow(dados)), size = 47) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- linDA(variables = dados[, c("Tecnica","Historico")], 
              group = dados$Grupo, validation = 'learntest', learn = treinamento,
              test = teste)

# Fun??es de classifica??o
fit$functions
# Matriz de confus?o
fit2$confusion
# Taxa de erro
fit$error_rate
fit2$error_rate
fit3$error_rate

# Classifica??o prevista
Y_previsto <- fit$classification

# Classificando um novo objeto
# Caso a variavel esteja escalonada ou seja com scale
# Sera necessario calucar a (media da amostra - VALOR) / Devio Padrao
classify(fit3, 
         newdata = data.frame(
           Tecnica = 15,
           Historico = 8))$pred_class

# Modelo discriminante quadr?tico
discrim_quad <- quaDA(variables = dados[, c("Tecnica","Historico")],
                      group = dados$Grupo)
discrim_quad2 <- quaDA(variables = dados[, c("Tecnica","Historico")], 
                       group = dados$Grupo, validation = 'crossval')
discrim_quad3 <- quaDA(variables = dados[, c("Tecnica","Historico")],
                       group = dados$Grupo, validation = 'learntest',  learn = treinamento, test = teste)

discrim_quad$error_rate
discrim_quad2$error_rate
discrim_quad3$error_rate

##############################################################################################
#
#                                  An?lise Discriminante - Aula 2
#
##############################################################################################

##################################### PROBLEMA ####################################
# No arquivo dados_empresa.csv encontram-se os dados referentes a 21 empresas 
# coletados dois anos antes de falirem (grupo 0) e a 25 empresas que n?o faliram 
# (grupo 1). As vari?veis observadas foram:
#  .	Fluxo de caixa com total de d?bitos
#  .	Rendimento da empresa/total do patrim?nio
#  .	Patrim?nio atual/total de d?bito
#  .	Patrim?nio atual/rendimento das vendas
###################################################################################

# Pacotes para an?lise
library(dplyr)
library(heplots)
library(DiscriMiner)

# Leitura dos dados
dados_treino <- read.csv('dataset/dados_empresa.csv', sep = ';', dec = ',') 
%>%
  select(Grupo, Fluxo = Fluxo.de.caixa..total.de.debitos, 
         Rendimento = Rendimento.da.empresa..total.de.patrim?nio,
         Patrimonio_debito = Patrim?nio.atual..total.de.debito,
         Patrimonio_vendas = Patrim?nio.atual..rendimento.das.vendas)

dados_teste <- data.frame(Grupo = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                          Fluxo = c(-0.560, -0.185, -0.070, 0.065, 0.370, -0.330, 0.140, 
                                    0.190, 0.425, 0.580),
                          Rendimento = c(-0.410, -0.185, -0.060, 0.020, 0.110, -0.090, 
                                         0.040, 0.060, 0.085, 0.140),
                          Patrimonio_debito = c(0.330, 1.165, 1.370, 1.535, 2.150, 0.460, 
                                                2.030, 2.350, 2.965, 5.060),
                          Patrimonio_vendas = c(0.160, 0.265, 0.400, 0.630, 0.950, 0.130, 
                                                0.315, 0.450, 0.545, 0.690))

################################### Regress?o Linear ######################################
ajuste_reg_lin <- lm(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas,
                     data = dados_treino)
prob_posteriori <- predict.lm(ajuste_reg_lin, newdata = dados_teste[, -1])
classificacoes <- ifelse(prob_posteriori > 0.5, 1, 0) 
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################## Regress?o Log?stica ######################################
ajuste_reg_log <- glm(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas, 
                      family = binomial(link = 'logit'), data = dados_treino)

# Propor??o de 1's na amostra de treino
p <- mean(dados_treino$Grupo)

# Predi??es da probabilidade a posteriori
log_chances <- predict.glm(ajuste_reg_log, newdata = dados_teste[, -1])
prob_posteriori <- exp(log_chances)/(1+exp(log_chances))
classificacoes <- ifelse(prob_posteriori > p, 1, 0) 
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### An?lise Discriminante Linear ##############################
fit <- linDA(variables = dados_treino[, -1], 
             group = dados_treino$Grupo)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################ An?lise Discriminante Quadr?tica ##############################
fit <- quaDA(variables = dados_treino[, -1], 
             group = dados_treino$Grupo)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### ?rvore de classifica??o ##############################
set.seed(0)
library(rpart)

# Ajustar a ?rvore:
fit <- rpart(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas,
             method="class", data = dados_treino)

# poda:
melhorCp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# cp ? uma medida de complexidade da ?rvore, essencialmente
# proporcional ao n?mero de folhas presentes. Este c?digo
# escolhe o melhor cp via valida??o cruzada.
pfit <- prune(fit, cp = melhorCp)

# plotar ?rvore podada
pdf('D:/UNICID/An?lise Discriminante/Aulas/grafico_teste_arvore.pdf', width = 9, height = 9)
plot(pfit)
text(pfit,use.n=FALSE,all=FALSE,cex=1.5)
dev.off()

# Classifica??o
classificacoes <- predict(pfit, dados_teste[,-1], type = 'class')
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### KNN ##############################
ajuste = knn(train = dados_treino[,-1], test = dados_teste[,-1], 
             cl = dados_treino$Grupo, k = 3)
tab_confusao <- table(ajuste, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos
