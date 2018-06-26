
aula1 <- read.csv2("dataset/aula1.csv")
View (aula1)

summary(aula1)

aula1$Sexo
aula1$Altura..em.centímetros.
aula1[,c(1,5)]

table(aula1$Sexo)
prop.table(table(aula1$Sexo))

table(aula1$Área.de.Conhecimento)
prop.table(table(aula1$Área.de.Conhecimento))


colnames(aula1)

colnova = c("idade","sexo","altura","peso","areconh","natsp","satiCel","timefut","satban","fuma")

colnames(aula1) = colnova

table(aula1$areconh)
table( aula1$sexo,aula1$areconh)

table( aula1$satban,aula1$satiCel)
table( aula1$satban,aula1$natsp)

table (aula1$idade)
table(cut(aula1$idade, breaks = c(25,28,31,35), include.lowest = TRUE))

table (aula1$peso)
table(cut(aula1$peso, breaks = c(51,56,61,66,71,76,81), include.lowest = TRUE))

t=0
while (t < 10){
    print("teste");   t = t+1 
}

pie(table(aula1$sexo))

# 
hist(rnorm(1000,20,6))

hist(aula1$idade)
hist(aula1$idade, breaks = c(25,28,31,35))
hist(aula1$peso)
hist(aula1$peso,breaks = 4)

hist(AirPassengers)
hist(AirPassengers,breaks = 3)
plot(AirPassengers,type ="l")

lines(AirPassengers, type = "o")


