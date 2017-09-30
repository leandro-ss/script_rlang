
aula1 <- read.csv2("dataset/aula1.csv")
aula1

summary(aula1)
table (aula1)

mean(aula1$Idade.em.anos)
median(aula1$Idade.em.anos)

quantile(aula1$Idade.em.anos)
quantile(aula1$Idade.em.anos, probs = c(0.9))

summary(aula1$Idade.em.anos)

summary(aula1[,c(1,3,4)])

summary(data.frame(aula1$Idade.em.anos.,aula1$Peso..em.kg. ))

var(aula1[,1])

n = length(aula1[,1])
var (aula1[,1]) * ((n-1)/n)

sd(aula1[,1])

sd(aula1[,1])/mean(aula1[,1])*100

boxplot(aula1[,1])

cor (aula1[,c(3,4)])

idade_p = (aula1[,1] - mean (aula1[,1]))/sd(aula1[,1])

idade_p