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
#3) No arquivo dados_doenca.xls,
#encontram-se dados provenientes de um estudo cuja finalidade é
#identificar fatores de risco para a doença coronariana (definida
#como obstrução de mais de 50% de pelo menos uma coronária). Compare os modelos 
#de classifica̧ão estudados para prever se um paciente tem ou não doença (LO3) a partir 
#das variáveis explicativas: presença de angina estável (ANGEST), antecedentes hereditários  
#(H), infarto do miocárdio prévio (IMP), nível de triglicérides para pacientes
#sem medicamento (TRIGS), nível de colesterol para pacientes sem medicamento
#(COLS), idade (IDADE1) e sexo (SEXO). Considere somente os participantes com dados 
#completos para as variáveis indicadas.

# Pacotes para an?lise
library(dplyr)
library(heplots)
library(DiscriMiner)
library(class)

# Leitura dos dados
set.seed(0)
library(readr)
dados <- read_delim("dataset/dados_doenca.csv", 
                           ";", escape_double = FALSE, col_types = cols_only(LO3 = col_guess(), 
                                                                             ANGEST = col_guess(), COLS = col_guess(), 
                                                                             IDADE1 = col_guess(), AH = col_guess(),
                                                                             SEXO = col_guess(), TRIGS = col_guess()), 
                           trim_ws = TRUE) %>% na.omit

subset(teste, select=-c(LO3))
treino <- dados[sample(seq_len(nrow(dados)), size = 1125),]
teste <- dados[setdiff(seq_len(nrow(dados)), treino),] ;

################################### Regress?o Linear ######################################
ajuste_reg_lin <- lm(LO3 ~ AH + ANGEST + COLS + IDADE1  + SEXO + TRIGS, data = treino)
prob_posteriori <- predict.lm(ajuste_reg_lin, newdata = subset(teste, select=-c(LO3)))
classificacoes <- ifelse(prob_posteriori > 0.5, 1, 0) 
tab_confusao <- table(classificacoes, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################## Regress?o Log?stica ######################################
ajuste_reg_log <- glm(LO3 ~ AH + ANGEST + COLS + IDADE1  + SEXO + TRIGS, 
                      family = binomial(link = 'logit'), data = dados)

# Propor??o de 1's na amostra de treino
p <- mean(dados$LO3)

# Predi??es da probabilidade a posteriori
log_chances <- predict.glm(ajuste_reg_log, newdata = subset(teste, select=-c(LO3)))
prob_posteriori <- exp(log_chances)/(1+exp(log_chances))
classificacoes <- ifelse(prob_posteriori > p, 1, 0) 
tab_confusao <- table(classificacoes, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### An?lise Discriminante Linear ##############################
fit <- linDA(variables = subset(treino, select=-c(LO3)), group = treino$LO3)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = subset(teste, select=-c(LO3)))$pred_class
tab_confusao <- table(classificacoes, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################ An?lise Discriminante Quadr?tica ##############################
fit <- quaDA(variables = subset(treino, select=-c(LO3)), group = treino$LO3)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = subset(teste, select=-c(LO3)))$pred_class
tab_confusao <- table(classificacoes, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### ?rvore de classifica??o ##############################
library(rpart)

# Ajustar a ?rvore:
fit <- rpart(LO3 ~ AH + ANGEST + COLS + IDADE1  + SEXO + TRIGS, data = treino, method="class")

# poda:
melhorCp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# cp ? uma medida de complexidade da ?rvore, essencialmente
# proporcional ao n?mero de folhas presentes. Este c?digo
# escolhe o melhor cp via valida??o cruzada.
pfit <- prune(fit, cp = melhorCp)

# plotar ?rvore podada
#pdf('trash/grafico_teste_arvore.pdf', width = 9, height = 9)
#plot(pfit);text(pfit,use.n=FALSE,all=FALSE,cex=1.5)
#dev.off()

# Classifica??o
classificacoes <- predict(pfit, subset(teste, select=-c(LO3)), type = 'class')
tab_confusao <- table(classificacoes, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### KNN ##############################
ajuste = knn(train = subset(treino, select=-c(LO3)), test =subset(teste, select=-c(LO3)), cl = treino$LO3, k = 3)
tab_confusao <- table(ajuste, teste$LO3)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos