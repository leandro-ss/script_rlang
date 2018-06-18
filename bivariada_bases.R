## carregamento de pacotes
if(!require(dplyr)){install.packages("dplyr")}         # filter, select, %>%
if(!require(tidyr)){install.packages("tidyr")}         # gather, spread
if(!require(ggplot2)){install.packages("ggplot2")}     # graficos
if(!require(car)){install.packages('car')}             # leveneTest
if(!require(DescTools)){install.packages('DescTools')} # CohenD
if(!require(corrplot)){install.packages("corrplot")}   # corrplot

getwd() # setwd("C:/Users/User/Documents/Disciplinas/P?s estatistica/04 - Bivariada")

####### Introducao###############################################

## Construir figuras:
set.seed(42)
dados<-data.frame(valor = c(rnorm(100, 2, 5), 
                            rnorm(100, 5, 2)),
                  popul = c(rep('A',100), 
                            rep('B',100)))


ggplot(dados, aes(x=valor, group=popul, fill=popul))+
  stat_density(position = position_dodge(width = 0), alpha=.5)+
  theme_classic()+xlim(c(-15, 20))


binominal
negativo-binomial
poisson
normal

####### Teste t de Student popula??es indenpendentes #############

## Teste 1
# Variancias iguais
teste_1<-data.frame(valor = c(rnorm(n=100, mean=2, sd=1), 
                              rnorm(n=100, 5, 1)),
                    popul = c(rep('A',100), 
                              rep('B',100)))

shapiro.test(teste_1$valor[teste_1$popul=='A'])
shapiro.test(teste_1$valor[teste_1$popul=='B'])

qqnorm(exp(teste_1$valor[teste_1$popul=='A']))
qqline(exp(teste_1$valor[teste_1$popul=='A']))

qqnorm(log(teste_1$valor[teste_1$popul=='A']))
qqline(log(teste_1$valor[teste_1$popul=='A']))

##write.csv(teste_1, "dataset/teste.csv")

boxplot(valor ~ popul, teste_1)
with(teste_1, rbind(Medias = tapply(valor, popul, mean),
                    Variancias = tapply(valor, popul, var)))


med_A <- mean(teste_1$valor[teste_1$popul=="A"])
med_B <- mean(teste_1$valor[teste_1$popul=="B"])
dp_A  <- sd(teste_1$valor[teste_1$popul=="A"])
dp_B  <- sd(teste_1$valor[teste_1$popul=="B"])
n_A <- length(teste_1$valor[teste_1$popul=="A"])
n_B <- length(teste_1$valor[teste_1$popul=="B"])

resulta_t <- (med_A-med_B)/sqrt( ((dp_A^2)/n_A)+((dp_B^2)/n_B))
resulta_t 
pt(resulta_t, df=99)
t.test(valor ~ popul, teste_1, var.equal=FALSE)
t.test(valor ~ popul, teste_1, var.equal=TRUE)
shapiro.test(teste_1$valor[teste_1$popul=='A'])
shapiro.test(teste_1$valor[teste_1$popul=='B'])
par(mfrow=c(1,2))
qqnorm(teste_1$valor[teste_1$popul=='A']);qqline(teste_1$valor[teste_1$popul=='A'])
qqnorm(teste_1$valor[teste_1$popul=='B']);qqline(teste_1$valor[teste_1$popul=='B'])

## Teste 2
teste_2 <- data.frame(valor = c(rnorm(n=100, mean=2, sd=5), 
                              rnorm(n=100, mean=5, sd=2)),
                    popul = c(rep('A', 100), 
                              rep('B', 100)))
write.csv(teste_2, "teste_2.csv")

boxplot(valor ~ popul, teste_2)
with(teste_2, rbind(Medias = tapply(valor, popul, mean),
                    Variancias = tapply(valor, popul, var)))

med_A <- mean(teste_2$valor[teste_2$popul=="A"])
med_B <- mean(teste_2$valor[teste_2$popul=="B"])
dp_A  <- sd(teste_2$valor[teste_2$popul=="A"])
dp_B  <- sd(teste_2$valor[teste_2$popul=="B"])
n_A <- length(teste_2$valor[teste_2$popul=="A"])
n_B <- length(teste_2$valor[teste_2$popul=="B"])

resulta_t <- (med_A-med_B)/sqrt( ((dp_A^2)/n_A)+((dp_B^2)/n_B))
resulta_t
pt(resulta_t, df=19)


t.test(valor ~ popul, teste_2, var.equal=FALSE)
t.test(valor ~ popul, teste_2, var.equal=TRUE)
t.test(valor ~ popul, teste_2)


leveneTest(valor ~ popul, teste_2)
CohenD(teste_2$valor[teste_2$popul=='A'], teste_2$valor[teste_2$popul=='B'])

## Cohen tosco
Cohen<-function(n, m1, m2, sd1, sd2){
  s <- ( ((n-1)*sd1^2) + ((n-1)*sd2^2) )/((2*n)-2)
  d<-(m1-m2)/s
  return(d)
}
d_Cohen<-c()
for(n in 2:100){
  d_Cohen[n]<-Cohen(n=n, 
        m1=mean(rnorm(n, 2, 5)),
        m2=mean(rnorm(n, 5, 2)),
        sd1=sd(rnorm(n, 2, 5)),
        sd2=sd(rnorm(n, 5, 2)))
}
plot(d_Cohen, xlab = 'n?mero amostral')

###########################################################3


####### Teste t de Student para amostras dependentes

## Teste 4
set.seed(42)
t1<-rnorm(30,20,2)
teste_4_wide <- data.frame(t1=t1,
                      t2=t1+rnorm(30,1,0.3),
                      n=1:30)
teste_4_long<-teste_4_wide %>% gather(key=tempo, value=valores, -n) 



boxplot(valores ~ tempo, teste_4_long,
        main="Massa dos carneiros nos tempos t1 e t2", 
        ylab="Massa (*10 kg)")

write.csv(teste_4_long, "teste_4_long.csv")

ggplot(teste_4_long, 
       aes(x=as.factor(tempo),y=valores, group=n))+
  geom_point()+geom_smooth(method='lm', se=F)+theme_classic()

ggplot(teste_4_long, 
       aes(x=as.factor(tempo),y=valores))+
  geom_point(aes(group=n))+
  geom_smooth(aes(group=n),method='lm', se=F)+
  geom_boxplot(aes(fill=tempo),alpha=0.2)+
  theme_bw()


t.test(valores ~ tempo, teste_4_long)
t.test(valores ~ tempo, teste_4_long, paired=T)

### Teste Z de Proporcoes

binom.test(5, 6, p = 0.5, alternative = "greater")
prop.test(x=5, n=6,p=.5)

prop.test(x = c(490,400), 
          n = c(500, 500))
## Teste de independencia

file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)
tarefas<-housetasks
names(tarefas)<-c("esposa", "alterna", "marido", 'juntos')
row.names(tarefas)<-c("lavanderia", "refeicao", "jantar", "cafe_manha", "roupa",
                      "loucas", "compras", "impostos", "dirigir", "financas", 
                      "seguros", "reparos", "feriados")

# write.csv(tarefas, "tarefas.csv")
tarefas<-read.csv('tarefas.csv')

tarefas_long<-gather(tarefas, key=quem, value=vezes)
tarefas_long$tarefas<-row.names(tarefas)

ggplot(tarefas_long,aes(x=tarefas, y=vezes, fill=quem))+
  geom_col(position = 'dodge')+theme_classic()+
  scale_fill_manual(values=c('yellow', 'pink', 'red', 'blue'))



chisq<-chisq.test(tarefas)
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)
# Contribuicao
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

