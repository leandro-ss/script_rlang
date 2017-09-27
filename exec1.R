#AULA
aula1 <- read.csv2("dataset/aula1.csv")
colnames(aula1) = c("idade","sexo","altura","peso","areconh","natsp","satiCel","timefut","satban","fuma")
aula1

# 1 As tabelas em R, copiar o comando executado e a tabela gerada (pode ser print)
#	a. Tabela para Sexo (frequência e proporção)
# https://stackoverflow.com/questions/11148868/how-to-generate-a-frequency-table-in-r-with-with-cumulative-frequency-and-relati
cbind( Freq=table(aula1$sexo), relative=prop.table(table(aula1$sexo)))
#	b. Tabela Cruzada Sexo x Natural de São Paulo
table(aula1$sexo,aula1$natsp)
#	c. Tabela para Altura, considerando as classes via Sturges (frequência e proporção)
hist(aula1$altura,breaks=nclass.Sturges(aula1$altura))


#EMPRESA RH
exec_1_1 <- read.csv2("dataset/exec_1_1.csv")
exec_1_1

#2 Para o conjunto de dados RH_empresa. Fazer em R
#  a. Tabela para o Numero do projeto
table(exec_1_1$num_projeto)
#b. Tabela da área da empresa (frequência e proporção)
cbind( Freq=table(exec_1_1$area), relative=prop.table(table(exec_1_1$area)))
#c. Tabela cruzada Tempo de trabalho X Nivel de salario
table(exec_1_1$tempo_trabalho,exec_1_1$nivel_salario)
#d. Tabela para avaliação do empregado. Considerar as quebras (0.0, 0.2, 0.4, 0.6, 0.8, 1.0) (frequência e proporção)
table(cut(exec_1_1$avaliacao, breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), include.lowest = TRUE))


#FIFA
exec_1_2 <- read.csv2("dataset/exec_1_2.csv")
exec_1_2

#3 Para o conjunto de dados FIFA2017. Fazer em R
#a.	Tabela para a perna que o jogador usa (proporção)
prop.table(table(exec_1_2$perna_preferencia))
#b. Tabela para classificacao_skill
table(exec_1_2$classificacao_skill)
#c. Considerando as classificações para idade { [17,20), [20,30) , [30,40) , [40,50)}. Faça uma tabela cruzada com a perna do jogador.
table(cut(exec_1_2$idade, breaks = c(17, 20, 30, 40, 50), include.lowest = TRUE),exec_1_2$perna_preferencia)
#d. Usando Sturges, faça uma tabela para drible.
table(exec_1_2$drible, breaks=nclass.Sturges(exec_1_2$drible))


#AULA
aula1 <- read.csv2("dataset/aula1.csv")
colnames(aula1) = c("idade","sexo","altura","peso","areconh","natsp","satiCel","timefut","satban","fuma")
aula1

# Os gráficos gerados copiar o comando (se for em R) 
#e além disso,fazer uma breve interpretação.
#Para o conjunto de dados Questionário. Fazer em Excel.

#a.	Um gráfico para Sexo 
pie(table(aula1$sexo))

#b.	Gráfico de dispersão para Peso x Altura
plot(aula1$altura~aula1$peso)


#EMPRESA RH
exec_1_1 <- read.csv2("dataset/exec_1_1.csv")
View(exec_1_1)

#Para o conjunto de dados RH_empresa. 
#a.	Gráfico para Numero do projeto
hist(exec_1_1$num_projeto,breaks = c(0,1,2,3,4,5,6,7))

#b.	Barras múltiplas de acordo com quem saiu do trabalho para os níveis de salários. (3 barras para quem saiu e 3 barras para quem ainda está trabalhando).
barplot(table(exec_1_1$nivel_salario,exec_1_1$tempo_trabalho),  col=c("lightblue1", "lightblue2", "lightblue3"),
        main="Salário por Tempo Trabalhado", xlab="Anos Trabalhados", ylab="Qtd. Pessoas" )
legend("topright", c("High", "Median", "Low"), fill=c("lightblue1", "lightblue2", "lightblue3"))
#c.	Histograma para media_horas_mensais
hist(exec_1_1$media_horas_mensais, breaks = 5)


#FIFA
exec_1_2 <- read.csv2("dataset/exec_1_2.csv")
exec_1_2

#Para o conjunto de dados FIFA2017.

#a. Gráfico de barras horizontais para nacionalidade
barplot(table(exec_1_2$nacionalidade),horiz=TRUE)
#b. Dispersão para Peso x Altura
plot(exec_1_2$altura~exec_1_2$peso)
#c. Histograma para idade
hist(exec_1_2$idade)
#d. Barra para classificacao_skill
barplot(table(exec_1_2$classificacao_skill))
#e. Gráfico de setor para perna que usa pra jogar.
pie(table(exec_1_2$perna_preferencia))

