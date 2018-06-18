dataset
dataset <- data("USArrests")
head(USArrests) 
summary(USArrests)
plot(Rape  ~ UrbanPop , USArrests)
cor.test(x = USArrests$UrbanPop, y = USArrests$Rape, method = "spearman")

### emula um valor de correlação com metodo de spearman
x <- 0:99
y <- -1* exp(x)
plot(y~x)
cor.test(x = x,                  y = y,              method = "spearman")

### emula um valor de correlação um pouco abaixo do que o anterior
x <- 0:99
y <- -1* exp(c(x[1:99],1))
cor.test(x = USArrests$UrbanPop, y = USArrests$Rape, method = "spearman")

### emula um valor de correlação com metodo de pearson
x <- 0:99
y <- -1* exp(c(x[1:99],1))
cor.test(x = USArrests$UrbanPop, y = USArrests$Rape, method = "pearson")

### emula um valor de correlação com metodo de pearson
x <- 0:99
y <- -1* exp(x)
cor.test(x = x,                  y = y,              method = "pearson")

### calculo de R 
cor(x=x,y=y)

### calculo de R²
cor(x=x,y=y)^2

## adicionando uma linha de tendencia ao modelo
modelo <- lm(y~x)
plot(y~x)
abline(coef = coef(modelo), col="red")

summary(modelo)