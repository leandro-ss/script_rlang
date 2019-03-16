if(!require(tidyverse)){install.packages("tidyverse")}

data("iris")
head(iris)
Y1 <- iris$Sepal.Length
X1 <- iris$Species

anova(lm(Y1~X1))
resulta <- aov(lm(Y1~X1))
summary(resulta)
31.606/0.265 # Valor de F = SQM_inter/SQM_intragrupos

# Isso quer dizer que: Y1 (sepal length) varia conforme X1 (species)
# Porque descartamos a H0, pois tem uma probabilidade de erro tipo I bem baixa (<2e-16)

TukeyHSD(resulta)

###
setwd(choose.dir())
#setwd("C:/Users/User/Documents/Disciplinas/P?s estatistica/11 - MANOVA e ANCOVA/aulas")
hatco<-read.csv('dataset/hatco.csv',sep=",", dec=".", header = T)
str(hatco)

# X1	Delivery speed - amount of time it takes to deliver the product once an order has been confirmed													
# X2	Price level-perceived level of price charged by product suppliers													
# X3	Price flexibility-perceived willingness of HATCO representatives to negotiate price on all types of purchases													
# X4	Manufacturer's image-overall image of the manufacturer or supplier 													
# X5	Overall service-overall level of service necessary for maintaining a satisfactory relationship between supplier and purchaser													
# X6	Salesforce image-overall image of the manufacturer's salesforce													
# X7	Product quality-perceived level of quality of a particular product (e.g., performance or yield													
# X8	Size of firm-size of the firm relative to others in this market.  This variable has two categories: 1 = large, 0 = small													
# X9	Usage level-how much of the firm's total product is purchased from HATCO, measured on a 100-point percentage scale, ranging from 0 to  100 percent													
# X10	Satisfaction level-how satisfied the purchaser is with past purchases from HATCO, measured on the same graphic rating scale as perceptions X1 to X7													
# X11	Specification buying-extent to which a particular purchaser evaluates each purchase separately (total value analysis) versus the use of specification buying, which details precisely the product characteristics desired.  This variable has two categories: 1 = employs total value analysis approach, evaluating each purchase separately; 0 = use of specification buying													
# X12	Structure of procurement-method of procuring or purchasing products within a particular company.  This variable has two categories: 1 = centralized procurement, 0 = decentralized procurement													
# X13	Type of industry-industry classification in which a product purchaser belongs.  This variable has two categories: 1 = industry A, 0 = other industries													
# X14	Type of buying situation-type of situation facing the purchaser.  This variable has three categories: 1 = new task, 2 = modified rebuy, 3 =  straight rebuy													

hatco$X8[hatco$X8==1] <- "large"
hatco$X8[hatco$X8==0] <- "small"
hatco$X8<-as.factor(hatco$X8)

################
## Exemplo 1 - teste t
################
# X9 - n?vel de uso (quanto que a empresa compra da hatco)
plot(X9 ~ X8,data=hatco, ylab="% de produtos comprados da HATCO" )
table(hatco$X8) # balanceado!
 # verificar as suposi??es
shapiro.test(hatco$X9)               # variavel normal no geral
shapiro.test(hatco$X9[hatco$X8=="small"]) # variavel normal para grupo 0
shapiro.test(hatco$X9[hatco$X8=="large"]) # variavel normal para grupo 1
if(!require(car)){install.packages("car")}
car::leveneTest(hatco$X9,hatco$X8)  # H0: dados com Homocedasticidade

t.test(X9 ~ X8,data=hatco)

anova(lm(X9 ~ X8,data=hatco))
summary(aov(lm(X9 ~ X8,data=hatco)))

#################
### Exemplo 2 ##
#################
# X9	Usage level-how much of the firm's total product is purchased from HATCO, measured on a 100-point percentage scale, ranging from 0 to  100 percent													
# X10	Satisfaction level-how satisfied the purchaser is with past purchases from HATCO, measured on the same graphic rating scale as perceptions X1 to X7													

# y1 + y2 ~ x1
# X9 + X10 ~ X8
shapiro.test(hatco$X10)                    
shapiro.test(hatco$X10[hatco$X8=="small"]) 
shapiro.test(hatco$X10[hatco$X8=="large"])
car::leveneTest(hatco$X10,hatco$X8)

if(!require(mvoutlier)){install.packages('mvoutlier')}
par(mfrow=c(2,2))
mvoutlier::aq.plot(hatco[c("X9",'X10')]) # nao tem outliers
par(mfrow=c(1,1))

if(!require(biotools)){install.packages("biotools")}
biotools::boxM(cbind(hatco$X9,hatco$X10), hatco$X8) # equivalencia das matrizes var-covar

if(!require(DescTools)){install.packages("DescTools")}
with(hatco, 
     DescTools::HotellingsT2Test(
       cbind(X9,X10) ~ X8))

summary(manova(lm(cbind(X9,X10) ~ X8, data=hatco)), 
        test='Hotelling')

summary(manova(lm(cbind(X9,X10) ~ X8, data=hatco)))

################
# Exemplo 3 - ANOVA
################
# Y1 ~ X1 + X2 
# X8	Size of firm-size of the firm relative to others in this market.  This variable has two categories: 1 = large, 0 = small													
# X9	Usage level-how much of the firm's total product is purchased from HATCO, measured on a 100-point percentage scale, ranging from 0 to  100 percent													
# X13: 1 = industry A, 0 = other industries
# 
# Nivel de uso (X9) em relacao a tamanho (X8: grande/pqno) 
#e tipo (X13: industria/nao) do comprador
hatco$X13[hatco$X13==0]<-"other"
hatco$X13[hatco$X13==1]<-"industry"
hatco$X13<-as.factor(hatco$X13)
table(hatco$X13, hatco$X8) # balanceado!
# Two way ANOVA
summary.aov(lm(X9 ~ X8+X13+X8:X13, data=hatco))
summary.aov(lm(X9 ~ X8*X13, data=hatco))
head(hatco)
ggplot(hatco, aes(x=X8, y=X9, fill=X13))+
  theme_classic()+
  geom_boxplot()+
  xlab("Tipo de Empresa")+ylab("N?vel de Uso")

# Teste Post HOC
modelo <- aov(lm(X9 ~ X8*X13, data=hatco)) # errado, pois a interacao nao foi significativa
TukeyHSD(modelo) # se desse ANOVA significativa pra interacao
modelo <- aov(lm(X9 ~ X8, data=hatco))
TukeyHSD(modelo) 
# como so tem dois niveis (gde/pqno) no fator X8, 
# nao tem o porque fazer teste post hoc
# Seria indicado se tivesse > niveis, pois TukeyHSD faz
# testes par-a-par

################
# Exemplo 4 - MANOVA
################
# Suposi?oes
# Tamanho amostral
table(hatco$X8, hatco$X13)

# Normalidade
if(!require(mvnormtest)){install.packages("mvnormtest")}
C <- t(hatco[,c("X9","X10")]) 
mvnormtest::mshapiro.test(C)

mvoutlier::aq.plot(hatco[c("X9",'X10')]) # Outliers

plot(hatco$X9, hatco$X10)                # Linearidade (>1 vd)

cor(hatco$X9, hatco$X10)                 # Multicolinearidade

# Homogeneidade da vari?ncia-covari?ncia (M de Box) (>1 vd)
biotools::boxM(cbind(hatco$X9,hatco$X10), hatco$X13) # OK!
biotools::boxM(cbind(hatco$X9,hatco$X10), hatco$X8)  # Nao ok...

# Homogeneidade das vari?ncias dos erros (Levene)
car::leveneTest(hatco$X9,hatco$X8)    # Homocedasticidade
car::leveneTest(hatco$X9,hatco$X13)   # Homocedasticidade
car::leveneTest(hatco$X10,hatco$X8)   # Homocedasticidade
car::leveneTest(hatco$X10,hatco$X13)  # Homocedasticidade

# Estimar, avaliar e interpretar
modelo<-lm(
  cbind(X9, X10)~X8*X13,
  data=hatco)
summary(manova(modelo))         # type II ANOVA table
car::Anova(modelo)
car::Anova(update(modelo, X9 ~ .))
car::Anova(update(modelo, X10 ~ .)) #mais significante para X10
# So que nao permite visualizar as relacoes das variaveis
if(!require(heplots)){install.packages('heplots')}
heplots::heplot(modelo, size="evidence",lwd=.5)
heplots::heplot(modelo, size="effect",
                add=T, lwd=4, term.labels=F)

## add interaction means
intMeans <- heplots::termMeans(modelo, 'X8:X13', abbrev.levels=5)
points(intMeans[,1], intMeans[,2], pch=16, cex=2, col="black")
text(intMeans[,1], intMeans[,2], rownames(intMeans), adj=c(0.5,1), col="brown")
lines(intMeans[c(1,3),1], intMeans[c(1,3),2], col="brown")
lines(intMeans[c(2,4),1], intMeans[c(2,4),2], col="brown")

##
# efeito sai da elipse "Error"
# A elipse X8 (main effect) ? junto com a elipse "Group"
print(car::linearHypothesis(modelo, 
                            c("X8small", "X13other"), 
                            title="Main effects"), SSP = FALSE)

print(car::linearHypothesis(modelo, c("X8small", "X13other", "X8small:X13other"),
                            title="Groups"), SSP = FALSE)
                              
heplots::heplot(modelo, hypotheses=list("Main effects" = 	c("X8small", "X13other")),
                col = "red", cex=1.25)
heplots::heplot(modelo, 
                hypotheses=list('Group'=
                              c("X8small", "X13other", "X8small:X13other")),
                              col = "blue",
                add=TRUE,
                cex = 1.25)
## Teste Post hoc
# Roy-bargmann Stepdown
# Ordem: X9 > X10

passo1<-lm(X9
           ~ X8*X13,
  data=hatco)
summary(passo1)       # Pegar interceptos e coef angulares
# 40.05, 8.883 (***), 4.1 (ns)
summary(aov(passo1))  # Pegar F e p-valor
# F= 15.14, p<0.001
passo2<-lm(X10
           ~X8*X13+X9, # X9 como covariavel
           data=hatco)
summary(passo2)       # Pegar interceptos
# 1.757
summary(aov(passo2))  # Pegar F e p-valor


#####################
## Exemplo 5 - ANCOVA
#####################

# X9 ~ X8*X7 (Nivel de uso ~ Tamanho * Qualidade percebida)

### Suposicoes
## Normalidade da VD (metrica) para cada nivel da VI 
# (categ?rica|FACTOR)
boxplot(X9 ~ X8, data = hatco)

# Homogeneidade dos residuos
plot(aov(X9 ~ X7 + X8, data = hatco), 
     which = 1)

# Linearidade e Homogeneidade dos declives
if(!require(lattice)){install.packages('lattice')}
xyplot(X9 ~ X7 | X8, 
       data = hatco, type = c("r", "p"))
# OU
if(!require(car)){install.packages('car')}
scatterplot(X9 ~ X7 | X8,  data = hatco)
# Teste de inferencia para as interacoes (declives n?o-homogeneos)
#anova(aov(X9 ~ X7 * X8, data = hatco))

### Executar e interpretar ANCOVA
mod <- lm(X9 ~ X7*X8, data = hatco) # Ordem dos fatores ? importante !
summary(mod)
anova(mod)

mod_sem_inter<-update(mod, ~. -X7:X8) # sem interacao (nao eh significativa)
anova(mod, mod_sem_inter) # Mostra que usar mod_sem_inter eh justificavel, pois reduz SS

# Devia ser p< 0.05 para justificar o modelo com interacao
# retas sao paralelas, pois X7:X8 nao eh significativa
mod_sem_cov<-update(mod_sem_inter, ~.-X7)
anova(mod_sem_inter, mod_sem_cov)
# Mesmo o uso da covariavel nao eh justificavel, pois p>0.05

######################
## Exemplo 6 - MANCOVA
######################
if(!require(jmv)){install.packages("jmv")}

mancova(data = hatco,
        deps = c('X10', 'X9'),
        factors = c('X8','X13'),
        covs=c('X7'),
        boxM = TRUE,
        shapiro = TRUE,
        qqPlot = TRUE)




####################################
## Exemplo - Repeated Measure MANOVA
####################################

## See OBrienKaiser for a description of the data set used in this example.
data(ObrienKaiser)
dados <- carData::OBrienKaiser
dados$ID <- 1:16

dados_wide <- subset(dados, select=c(ID, treatment, gender,
                                      pre.1, post.1, fup.1))
dados_long <- gather(dados_wide, 
                     key = tempo, 
                     value = valor_1, 
                     -c(ID, treatment, gender))

dados_wide1 <- subset(dados, select=c(ID, treatment, gender,
                                            pre.1, post.1, fup.1))
dados_long1 <- gather(dados_wide1, key=tempo, value=valor_1, 
                      -c(ID, treatment, gender))

dados_wide2 <- subset(dados, select=c(ID, treatment, gender,
                                            pre.2, post.2, fup.2))
dados_long2 <- gather(dados_wide2, key=tempo, value=valor_2, 
                      -c(treatment, gender, ID))
dados_wide3 <- subset(dados, select=c(ID, treatment, gender,
                                            pre.3, post.3, fup.3))
dados_long3 <- gather(dados_wide3, key=tempo, value=valor_3, 
                      -c(treatment, gender, ID))
dados_wide4 <- subset(dados, select=c(ID, treatment, gender,
                                            pre.4, post.4, fup.4))
dados_long4 <- gather(dados_wide4, key=tempo, value=valor_4, 
                      -c(treatment, gender, ID))
dados_wide5 <- subset(dados, select=c(ID, treatment, gender,
                                            pre.5, post.5, fup.5))
dados_long5 <- gather(dados_wide5, key=tempo, value=valor_5, 
                      -c(treatment, gender, ID))

dados_long1$tempo <- dados_long2$tempo <- dados_long3$tempo <- 
  dados_long4$tempo <-dados_long5$tempo <-c(rep("pre",16),
                                            rep("post",16),
                                            rep("fup",16))
dados_long <- left_join(dados_long1, dados_long2)
dados_long <- left_join(dados_long, dados_long3)
dados_long <- left_join(dados_long, dados_long4)
dados_long <- left_join(dados_long, dados_long5)

#############
#### ANOVA medidas repetidas one way
############
dados_long$tempo <- ordered(dados_long$tempo, levels= c("pre","post","fup"))

summary(aov(valor_1 ~ treatment +Error(ID/treatment),data=dados_long))
# usando car Anova, dados wide
modelo   <- lm(cbind(pre.1, post.1, fup.1) ~ 1, data=dados_wide1)
dados_wide1$treatment <- factor(dados_wide1$treatment)
iDado    <- data.frame(VarInd =levels(dados_wide1$treatment))
AnovaMR <- Anova(modelo, idata = iDado, idesign = ~VarInd)
summary(AnovaMR, multivariate = FALSE, univariate = TRUE)

summary(AnovaMR, multivariate = TRUE, univariate = F) # MANOVA

###################
##### ANOVA medidas repetidas two way
###################
summary(aov(valor_1 ~ treatment*gender +
              Error(ID/(treatment*gender)),
            data=dados_long))
# usando car Anova, dados wide
dados_wide <- subset(dados, 
                     select=c(treatment, gender,
                              pre.1, post.1, fup.1,
                              pre.2, post.2, fup.2))

dados_wide$treatment <- factor(dados_wide$treatment)
dados_wide$gender <- factor(dados_wide$gender)

modelo <- lm(cbind(pre.1, post.1, fup.1,
                   pre.2, post.2, fup.2) ~ 1, data = dados_wide)
iDado    <- expand.grid(VarInd.1 = levels(dados_wide$treatment),
                       VarInd.2 = levels(dados_wide$gender))
table(dados_wide$treatment, dados_wide$gender)

Anova2MR <- Anova(modelo, idata = iDado, idesign = ~VarInd.1*VarInd.2)

summary(Anova2MR, multivariate = FALSE, univariate = TRUE)

summary(AnovaMR, multivariate = TRUE, univariate = F)


################
# MANOVA medidas repetidas
################

summary(manova(cbind(valor_1,valor_2)~ 
                 treatment* gender + 
                 Error(ID/(treatment*gender)),
               data = dados_long))


dados_wide <- subset(dados, 
                     select=c(ID, treatment, gender,
                              pre.1, post.1, fup.1,
                              pre.2, post.2, fup.2))
modelo   <- lm(cbind(pre.1, post.1, fup.1, pre.2, post.2, fup.2) ~ 1, 
               data=dados_wide)
dados_wide$treatment <- factor(dados_wide$treatment)
dados_wide$gender <- factor(dados_wide$gender)

modelo <- lm(cbind(pre.1, post.1, fup.1,
                   pre.2, post.2, fup.2) ~ 1, data = dados_wide)
iDado    <- expand.grid(VarInd.1 =levels(dados_wide$treatment),
                        VarInd.2 =levels(dados_wide$gender))
AnovaMR <- Anova(modelo, idata = iDado, idesign = ~VarInd.1*VarInd.2)
summary(AnovaMR, multivariate = FALSE, univariate = TRUE)

summary(AnovaMR, multivariate = TRUE, univariate = F)

####################################################################################
