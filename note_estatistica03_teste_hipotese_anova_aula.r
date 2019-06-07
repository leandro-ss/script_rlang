## carregamento de pacotes
if(!require(dplyr)){install.packages("dplyr")}          # unite, filter, select, %>%
if(!require(tidyr)){install.packages("tidyr")}          # gather, spread
if(!require(ggplot2)){install.packages("ggplot2")}      # graficos

## Introducao
set.seed(101101)
dados <- data.frame(valor = rnorm(30)+20,
                    plantacao = rep("j",30))

ggplot(dados, aes(x=plantacao, y=valor))+
  geom_boxplot()+geom_point(color='red')+theme_bw()

# write.csv(dados, "dados.csv")
dados <- data.frame(valor = c(20+rnorm(30),
                              22+rnorm(30)),
                    plantacao = c(rep("p1",30),
                                  rep("p2",30)))
# write.csv(dados, "dados.csv")

ggplot(dados, aes(y=valor, x="geral"))+
  geom_boxplot()+geom_jitter(aes(color=plantacao))+theme_bw()

medias <- data.frame(valor= c(mean(dados$valor[dados$plantacao=="p1"]),
                              mean(dados$valor[dados$plantacao=="p2"])),
                     plantacao = c("p1","p2"))

ggplot(dados, aes(x=plantacao, y=valor))+
  geom_boxplot()+
  geom_point(data=medias, 
             aes(x=plantacao, y=valor, color=plantacao), size=5)+
  theme_bw()

ggplot(dados, aes(x=plantacao, y=valor))+
  geom_boxplot()+geom_point(aes(color=plantacao))+theme_bw()

## ANOVA 1 fator
ggplot(dados, aes(x=plantacao, y=valor, color=plantacao))+
  geom_point()+
  geom_abline(intercept = as.numeric(coef(lm(valor ~ plantacao, dados))[1])-2,
              slope = as.numeric(coef(lm(valor ~ plantacao, dados))[2]), color="red")+
  theme_bw()

# 2º forma 
#anova(lm(valor ~ plantacao, dados))

# 3 º forma 
#summary (aov(valor ~ plantacao, dados))



# Homocedasticidade
variancias <- tapply(dados$valor, dados$plantacao, FUN=var)
variancias
variancias[1]/variancias[2]
var.test(valor ~ plantacao, dados)

mod<-lm(valor ~ plantacao, dados)
mod
anova_mod<-aov(valor ~ plantacao, dados)
anova_mod
summary(anova_mod)
anova(mod)

residuos<-residuals(anova_mod)
sum(residuos)
sum(residuos^2) # SSDw (dentro dos grupos)

t.test(valor ~ plantacao, dados)

## Com 1 fator com 3 niveis
set.seed(123)
dados_1 <- data.frame(grupos = c(rep("A", 30),
                               rep("B", 30),
                               rep("C", 30)),
                    valores = c(rnorm(30, mean=4, sd=0.5),
                                rnorm(30, mean=3.5, sd=1),
                                rnorm(30, mean=3, sd=1)))

anova_mod<-aov(valores ~ grupos, dados_1)
summary(anova_mod)
TukeyHSD(anova_mod)
plot(TukeyHSD(anova_mod))

plot(anova_mod, which=1)
# Pontos 70, 72 e 44 foram sugeridos como outliers
car::leveneTest(valores ~ grupos, dados_1)
plot(anova_mod, which=2)

ggplot(dados_1, aes(x=grupos, y=valores, fill=grupos))+theme_bw()+
  geom_boxplot()+annotate("text", label=c("A", "A", "a"), x = c(1,2,3), y=6 )

### ANOVA two-way

data("ToothGrowth")
?ToothGrowth
dente<-ToothGrowth
table(dente$supp,dente$dose)
# write.csv(ToothGrowth,"ToothGrowth.csv")

ggplot(dente, aes(y=len, x=factor(dose), fill=supp))+
  geom_boxplot()+theme_bw()

## ANOVA com dois fatores: Aditivo
ANOVA_adit <- aov(len ~ factor(dose) + supp, data= dente)
summary(ANOVA_adit)

## Uma ANOVA com um LM
ANOVA_LM_mod <- aov(len ~ dose + supp, data= dente)
summary(ANOVA_LM_mod)

## Com interacao entre fatores:
ANOVA_int <- aov(len ~ factor(dose) + supp + factor(dose):supp, data= dente)
summary(ANOVA_int)
# ? a mesma coisa que:
ANOVA_int <- aov(len ~ factor(dose)*supp, data= dente)
summary(ANOVA_int)

## Teste post hoc
TukeyHSD(ANOVA_int)

if(!require(lsmeans)){install.packages("lsmeans")}
mod <- lm(len ~ supp*factor(dose), data= dente)
marginal <- lsmeans(mod, 
                   pairwise ~ supp:factor(dose), 
                   adjust="tukey")           ### Comparacao ajustada Tukey
marginal$contrasts
TukeyHSD(ANOVA_int)

cld(marginal,
    alpha=0.05,           ### Nivel de significancia da diferen?a dos grupos
    Letters=letters,      ### Usa letras caixa baixa para cada grupo
    adjust="tukey")       ### Comparacao ajustada de Tukey

ggplot(dente, aes(y=len, x=factor(dose), fill=supp))+
  geom_boxplot()+theme_classic()+
  annotate('text', 
           label=c("b","a", 
                   "c", "b", 
                   "c", "c"), 
           x=c(0.8, 1.2, 
               1.8, 2.2, 
               2.8, 3.2),
           y=max(dente$len)+1)

## AN?lise de COVAri?ncia: Resposta ~ Fator*VarContinua
ANCOVA_ <- aov(len ~ dose*supp, data= dente)
summary(ANCOVA_)

ggplot(dente, aes(y=len, x=dose, color=supp, group=supp))+
  geom_point()+theme_bw()+geom_smooth(method='lm', se=F)

# Comparacao de slopes:
if(!require(lsmeans)){install.packages("lsmeans")}
lstrends(ANCOVA_, 'supp', var='dose')

## Amostras desbalanceadas
if(!require(car)){install.packages("car")}
set.seed(32)
meus_dentes <- sample_n(dente, 32)
table(meus_dentes$supp, meus_dentes$dose)

minha_anova <- aov(len ~ supp * factor(dose), data = meus_dentes)
Anova(minha_anova, type = "III")
Anova(minha_anova, type = "II")

## Exerc?cio com CO2
data("CO2")


## ANOVA medidas repetidas
data("OBrienKaiser") 
?OBrienKaiser
if(!require(tidyr)){install.packages("tidyr")}
dados_wide <- subset(OBrienKaiser,
                     select = c(treatment, 
                                gender, 
                                pre.1, 
                                post.1,
                                fup.1))
dados_wide$ID <- 1:16

dados_long <- gather(dados_wide, 
                     key = tempo, 
                     value = valor_1, 
                     -c(ID, treatment, gender))
dados_long$tempo <- ordered(dados_long$tempo,
                            levels = c("pre","post","fup"))
# DADOS LONG
summary(aov(VD ~VI+ Error(ID/VI), 
            data=dados_long))
# DADOS WIDE
modelo <- lm(cbind(VD.1, VD.2, VD.3) ~ 1, 
             data = dados_wide)

dados_wide$VI <- factor(dados_wide$VI)

iDado <- data.frame(VarInd = levels(dados_wide$VI))

AnovaMR <- car::Anova(modelo, 
                      idata = iDado, 
                      idesign = ~VarInd)

summary(AnovaMR, multivariate = FALSE, 
        univariate = TRUE)

# EXERCICIO



## Kruskal-Wallis
Herbs <- c(14, 12.1, 9.6, 8.2, 10.2)
Shrubs <-c(8.4, 5.1, 5.5, 6.6, 6.3)
Trees <- c(6.9, 7.3, 5.8, 4.1, 5.4)
flies <- data.frame(local=c(rep('Herbs',5),
                            rep('Shrubs',5),
                            rep('Trees',5)),
                    obs=c(Herbs, Shrubs, Trees))
kruskal.test(obs~local, flies)
boxplot(Herbs, Shrubs, Trees, 
        names = c('Herbs', 'Shrubs', 'Trees'),
        ylab = "Flies/m3", col = 'red')

## Friedman
sample_n(CO2,5)
friedman.test(uptake~conc|Plant, data = CO2)
