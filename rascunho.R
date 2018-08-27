dataset <- read.csv("dataset/trabalho.csv")
head(dataset); tail (dataset); levels(dataset$Turno)
mod <- lm( TMP ~ Experiencia, data=dataset); summary(mod)
plot(mod, which = 2)

n <- nrow (dataset)

plot(mod, which=4)
critico <- 4/n
abline(a=critico, b=0, col="red", lty=2)
dataset <- dataset[1:205,]

cook <- as.vector(cooks.distance(mod))

which(cook > critico)

head(dataset); tail (dataset); levels(dataset$Turno)
summary(dataset)
mod <- lm( TMP ~ Experiencia, data=dataset)
plot(mod, which = 2)



predict.lm


trees_o <- cbind(g_outliers, h_outliers, v_outliers)
alfa <- 0.05         # probabilidade de erro tipo I para detectar outlier 
p_val <- 1-alfa      # 1 - alfa 
p <- ncol(trees_o)   # -1 pois nao conta variavel resposta
n <- nrow(trees_o)   # n observa??es

## Distancia de Cook
plot(fit2, which=4)
plot(fit2, which=5)
critico <- 4/n # Kenneth Bolin & Jackman (1990)
abline(a=critico, b=0, col="red", lty=2)
cook <- as.vector(cooks.distance(fit2))
which(cook > critic)o


require(MASS) #Exige o pacote de dados "MASS"
data(cats) #Carrega os dados
head(cats,11)
tail(cats,11)
# 
summary(cats)
# 
attach(cats) #Desanexa os vetores (colunas) do banco de dados.
# 
plot(Bwt~Hwt,cats)
abline(a=-0.3567, b=4.0341, lty=4,col="red")
title(main="Massa do Coração (g) vs. Massa Corpórea (Kg)\nde Gatos Domésticos")
abline(lm(Bwt~Hwt,cats),col="red")
# 
mod<-lm(Hwt~Bwt,cats); mod
summary(mod)
par(mfrow=c(2,2));plot(mod);par(mfrow=c(1,1))
plot(mod,which=1)
plot(mod,which=2)
shapiro.test(residuals(lm(Hwt~Bwt,cats)))
# 
# 
plot(mod,which=3)
plot(mod,which=4)
par(mfrow=c(2,2))
plot(lm(Hwt~Bwt,cats))
# 
# 
# 
# 
par(mfrow=c(1,1)); plot (Hwt ~ Bwt, col = factor(cats$Sex), data=cats)
plot (Hwt ~ Sex, cats)
plot (Bwt ~ Sex, cats)
# 
macho = cats[cats$Sex == "M",]
femea = cats[cats$Sex == "F",]
# 
mod_macho = lm(Hwt~Bwt,macho); summary(mod_macho);par(mfrow=c(2,2)); plot(mod_macho)
mod_femea = lm(Hwt~Bwt,femea); summary(mod_femea);par(mfrow=c(2,2)); plot(mod_femea)
mod_sexo = lm(Hwt ~Bwt *Sex,cats); summary(mod_sexo);par(mfrow=c(2,2)); plot(mod_sexo)
mod_test = lm(Hwt ~Bwt +Sex + Sex: Bwt,cats); summary(mod_sexo);par(mfrow=c(2,2)); plot(mod_test)
# 
ggplot(cats, aes (x=Bwt, y= Hwt, color=Sex, group = Sex))+
    geom_point()+
    geom_smooth(se=F,method="lm")+
    labs(subtitle="Altura vs Peso dos Gatos",
         y="Hwt",
         x="Bwt",
         title="Modelo de Covariancia Multipla")
# 
semente = read.csv("http://renatabrandt.github.io/EBC2015/data/semente.csv")
attach(semente)
mod_sem = lm (Sementes ~ Chuva * Fertilizante); summary(mod_sem)
ggplot(aes(x=Chuva, y=Sementes, colour = Fertilizante, group= Fertilizante))+
    geom_point()+
    theme_bw()+
    geom_smooth(se=F, method = "lm")+
    labs(subtitle="Relacao de Sementes por Chuva x Fertilizante",
         title="Modelo de Covariancia Multipla")
# 
quimica = read.csv2("wspc_rlang/dataset/quimica.csv" , sep = "\t")
attach(quimica)
par(mfrow=c (1,2))
plot(Rendimento ~ Temperatura)
plot(Rendimento ~ Tempo)
mod = lm (Rendimento ~ Temperatura * Tempo)
par(mfrow =c (2,2)); plot (mod)
Tempo1 = Tempo-mean (Tempo)
Temperatura1 = Temperatura-mean (Temperatura)
ajuste = lm(Rendimento ~ Tempo1 + I(Tempo1^2)+
                Temperatura1+I(Temperatura1^2)+
                Tempo1 * Temperatura1); summary (ajuste
                )
plot(ajuste)

install.packages ("rgl")
install.packages ("graphics")
install.packages ("lattice")
install.packages ("scatterplot3d")
require (datasets)
str(trees);attach(trees)

flines = function (x,y){
    points (x,y)
    abline (lm(x~y), col = "red")
}
fcor<- function(x,y){
    par(usr=c(0,1,0,1))
    txt<- as.character(round(cor(x,y),2))
    text(0.5, 0.5, txt, cex=3)
}

pairs(trees, lower.panel=flines, upper.panel = fcor)

library(scatterplot3d)
attach(trees)
par(mfrow=c (1,1))
graph<- scatterplot3d(Volume ~Girth+Height, pch=16, angle=60)
fit<- lm(Volume~Girth +Height)
graph$plane3d(fit, col="blue")


library(lattice)
cloud(Volume ~Girth*Height, data=trees, scales=list(arrows=FALSE))

library(rgl)
plot3d(trees)

require(graphics)
head(mtcars)
stars(mtcars[1:5, 1:2], nrow=2, key.loc=c(6.8, 1.8), draw.segments = TRUE, col.segments = 1:2)

