###############################################################################################
#                         Analise de Componentes Principais
###############################################################################################

##########################################################################
#################### EXERCICIO 4 #########################################
##########################################################################

library(corrplot)
library(foreign)
library(psych)
library(dplyr)
library(corrplot)
library(GPArotation)
library(tidyr)
library(ggfortify)

# Entrada dos dados e extra??o das componentes principais com a matriz de correla??o 
dados_ori <- read.csv('dataset/tcc_dataset.csv') 

str(dados_ori)
dados     <- dados_ori %>% select (-Country);glimpse(dados) 
var.name  <- dados_ori %>% select (Country);glimpse(dados) 

# Teste de esfericidade de Bartllet
bartlett.test(dados)
# Ajuste do modelo - escolha do n?mero de componentes
ajuste1 <- princomp(dados);summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines") 
# Ajuste do modelo com n?mero de componentes definido
ajuste2 <- principal(dados, covar = FALSE, nfactors = 3);summary(ajuste2)

summary(cbind(var.name , ajuste2$scores))
transpose_t =  t(ajuste2$score)
colnames(transpose_t) <- as.character(unlist(var.name))
summary(transpose_t)
# Comunalidades
correla <- cor(t(ajuste2$score)) 
colnames(correla) <- as.character(unlist(var.name))

corrplot(correla, order = "hclust", tl.col='black', tl.cex=.75) 

##########################################################################
#################### EXERCICIO 5 #########################################
##########################################################################

dat <- read.csv('dataset/raw-material-characterization.csv', sep = ';', dec = '.')

#head(dat)
df <- dat[3:8]
head(df)

#Esfericidade de Bartlet
bartlett.test(df)

# Ajuste do modelo - escolha do numero de componentes
fit1 <- princomp(df, cor=TRUE)

summary(fit1)

plot(fit1, type="lines") 

fit2 <- principal(df, covar = FALSE, nfactors = 2)

fit2

fit2$communality

autoplot(prcomp(scale(df)), data = df,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

df.fatores <- as_tibble(cbind(Outcome=dat$Outcome, fit2$scores))
summary(df.fatores)

df.fatores$Outcome <- dat$Outcome
df.fatores.g <- gather(df.fatores, "Fator", "Score", 2:3)
head(df.fatores.g)

ggplot(df.fatores.g, aes(x=Fator, y=Score, fill=Outcome)) + 
  geom_boxplot(alpha=0.6)

glimpse(df.fatores)

mod.logi <- glm(formula = Outcome ~ RC1 + RC2,
                data = df.fatores,
                family = 'binomial')

summary(mod.logi)
