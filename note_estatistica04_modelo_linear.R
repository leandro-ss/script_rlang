

data("anscombe")
plot(y1~x1, anscombe) 
abline(lm(y1~x1, anscombe), col="red") 

x1 = anscombe$x1
y1 = anscombe$y1

cor(y1,x1)

summary(lm(y1~x1, anscombe))
names(summary(lm(y1~x1, anscombe)))
summary(lm(y1~x1, anscombe))$coefficients

par(mfrow=c(2,2))
plot(lm(y1~x1, anscombe))

set.seed(1)
x <- sort(rnorm(n=50, mean=30, sd=3))
y <- 10 +2*x +0.1*x^2 +(rnorm(n=50, mean=0, sd=10))
shapiro.test(residuals(lm(y~x)))
summary(lm(y~x))

par(mfrow=c(2,2))
plot(lm(y~x))



data("airquality")
summary(airquality)
airquality = airquality[airquality$Month == 9]
mod1 = lm(airquality$Wind ~ airquality$Temp)
summary (mod1)
#What Is R-squared?
#   R-squared is a statistical measure of how close the data are to the fitted regression line. It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression.
#   The definition of R-squared is fairly straight-forward; it is the percentage of the response variable variation that is explained by a linear model. Or:
#   R-squared is always between 0 and 100%:
#   0% indicates that the model explains none of the variability of the response data around its mean.
#   100% indicates that the model explains all the variability of the response data around its mean.
#   In general, the higher the R-squared, the better the model fits your data. However, there are important conditions for this guideline that I’ll talk about both in this post and my next post.

#Correlacao e coeficiente de pearson
#   Em estatística descritiva, o coeficiente de correlação de Pearson, também chamado de "coeficiente de correlação produto-momento" ou simplesmente de "ρ de Pearson" mede o grau da correlação (e a direcção dessa correlação - se positiva ou negativa) entre duas variáveis de escala métrica (intervalar ou de rácio/razão).

#   Este coeficiente, normalmente representado por ρ assume apenas valores entre -1 e 1.

#   {\displaystyle \rho =1} {\displaystyle \rho =1} Significa uma correlação perfeita positiva entre as duas variáveis.
#   {\displaystyle \rho =-1} {\displaystyle \rho =-1} Significa uma correlação negativa perfeita entre as duas variáveis - Isto é, se uma aumenta, a outra sempre diminui.
#   {\displaystyle \rho =0} {\displaystyle \rho =0} Significa que as duas variáveis não dependem linearmente uma da outra. No entanto, pode existir uma dependência não linear. Assim, o resultado {\displaystyle \rho =0} {\displaystyle \rho =0} deve ser investigado por outros meios.

abline(mod1, col="red")
par(mfrow=c(2,2))
plot(mod1)

plot(hatvalues(mod1)) # alavancagem



########################## TEST ###########################################


#AULA
pokmeon <- read.csv2("dataset/pokemon.csv")
colnames(aula1) = c("idade","sexo","altura","peso","areconh","natsp","satiCel","timefut","satban","fuma")
aula1
