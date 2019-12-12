# Inspiracoes em https://rpubs.com/haj3/conjoint e https://www.linkedin.com/learning/the-data-science-of-marketing/conjoint-analysis-with-r

##### Analise de Conjuntos "Tradicional"
if(!require(conjoint)){install.packages("conjoint")}
# carregando pacote
data(tea)
tprof # sao os estimulos que foram elaborados (3 Fatores c/ 3 Niveis + 1 Fator c/ 2 niveis)
str(tprof)
tlevn # sao os rotulos dos niveis
str(tlevn)
tprefm # sao as respostas dos entrevistados ranqueados (L: entrevistado, C: )
str(tprefm)

## Calculando o modelo do primeiro respondente
# caModel(y = vetor do perfil das preferencias, x = matriz dos perfis)
caModel(y = tprefm[1,], x=tprof)

## Retornando um vetor de utilidades de todos os 12 atributos (11 niveis + intercepto)
caUtilities(y=tprefm[1,], x=tprof, z=tlevn)

## Calculando as utilidades parciais para os primeiros 6 respondentes
head(caPartUtilities(y=tpref, x=tprof, z=tlevn))

## Estimando os parametros de toda a amostra (assumindo que eh homogenea)
# z = matriz de nomes dos niveis
# essa funcao nos retorna tambem a importancia media dos fatores 
Conjoint(y=tpref, x=tprof, z=tlevn)

## Estimativa de Participacao
ShowAllSimulations(sym=tsimp, y=tpref, x=tprof)

# Retorna tres vetores de porcentagem de participacao
# usando Utilidade Maxima, BTL(Bradley-Terry-Luce) e Modelos logit 

## Segmentacao
if(!require(cluster)){install.packages("cluster")}
segmentos <- caSegmentation(y=tpref, 
                            x=tprof, 
                            c = 2)
segmentos

###### Analise de Conjuntos Adaptativa
# Fonte: https://blog.sicara.com/market-research-survey-conjoint-analysis-r-code-3d4f6190c2aa
## Declaracao das caracteristicas e niveis
nome_fatores <- c("LENGTH","ILLUSTRATION","CLAPS") # feature_names
niveis_fatores <- list() # features_values
niveis_fatores[[1]] <- c("2min", "7min", "20min")
niveis_fatores[[2]] <- c("several images","one image", "no image")
niveis_fatores[[3]] <- c("+500 claps", "less than 500 claps")

## Geracao de todos os estimulos
articles <- expand.grid(
  LENGTH=niveis_fatores[[1]],
  ILLUSTRATION=niveis_fatores[[2]],
  CLAPS=niveis_fatores[[3]]
)

maxNumberOfArticles = 8
## Selecao de conceitos relevantes
selectedArticles <- caFactorialDesign(
  data=articles,
  type='fractional',
  cards=maxNumberOfArticles
)

## Checando se conceitos selecionados sao relevantes para o estudo
corrSelectedArticles <- caEncodedDesign(selectedArticles)
# Matriz de correlacao entre os niveis das caracteristicas dos estimulos selecionados
# Para estimar a perda de informacao devido a selecao:
print(cor(corrSelectedArticles))
# Nos obtemos que ha grande correlacao entre as caracteristicas CLAPS e LENGTH features. 
# Na pratica, devemos tentar outras selecoes.
# O objetivo eh encontrar a melhor relacao entre numero de estimulos e correlacao entre fatores
# Gracas ao experimento fatorial, podemos usar somente 2 a 8 estimulos, ao inves de 18

## Usando notas:
ranking = c(7/7,
            3/7,
            3/7,
            5/7,
            2/7,
            6/7,
            3/7,
            0/7)
## Analise conjunta do ranking hipotetico:
Conjoint(y=ranking, 
         x=selectedArticles, 
         z=unlist(niveis_fatores))

######## Choice- Based Conjoint Analysis
## Fonte: http://www.rpubs.com/angelayy/185881
## Livro:(Chapman & Feit) R for marketing research and analytics

cbc.df <- read.csv("http://goo.gl/5xQObB", 
                   colClasses = c(seat = "factor", 
                                  price = "factor" ))
## Checando os dados brutos
summary(cbc.df)

## Ajuste do modelo
if(!require(mlogit)){install.packages("mlogit")}
# Tem que usar um formato especial, usando mlogit.data()
# choice, varying & id.var parameters indicam
# quais colunas contem os dados das respostas, atributos e id dos respondentes (respectivamente).
cbc.mlogit <- mlogit.data(data=cbc.df, 
                          choice = "choice", # atributo choice, coluna chama "choice"
                          shape = "long", # ou "wide"
                          varying = 3:6, # 
                          alt.levels = paste0("pos", 1:3), 
                          id.var = "resp.id") # identificacado do responde, nome da coluna "resp.id"
m1 <- mlogit(choice ~ 0 + # intercepto = 0
               seat + cargo + eng + price, 
             data = cbc.mlogit)
# igual ao conjoint::caModel()
summary(m1)

# Os coeficientes calculados listam a media dos valores de cada nivel em relacao ao nivel basico
# Por ex seat1, comparado a seat2, eh mais popular (positivo),similarmente cargo1 eh menos popular do que
# cargo0 (2ft),pois eh negativo
# Os parametros estimados estao em escala logit,que varia normalmente entre -2 e 2
# valores maiores sugerem forte probabilidade de gostar (positivo) ou naogostar (negativo)
# o 0 no modelo fala que nao foi incluido o intercepto
# Se removermos 0 entao incluiremos o intercepto, 
# Dois paramentros adicionais indicam a preferencia para diferentes posicoes (left, middle, ou  right)

# Normalmente, nao sabemos se os consumidores preferem uma posicao,
# mas vemos o intercepto, se for significativo, pode sugerir que alguns
# simplesmente escolhe pela posicao ("chuta a letra c")

# Prever escolhas (Share Prediction)
predict_mnl <- function(model, products) {  
  # model: objeto mlogit criado pela funcao mlogit()  
  # data: data frame contendo os designs q voce quer prever
  # data deve ter mesmo formato dos dados usados para estimar o modelo mlogit
  data.model <- model.matrix(update(model$formula, 0 ~ .), 
                             data = products)[,-1]  
  utility <- data.model %*% model$coefficients 
  share <- exp(utility)/sum(exp(utility))  
  cbind(share, products)
}

## Para prever um novo produto
# m1 : seat + cargo + eng + price
attrib <- list(seat = c("6", "7", "8"),
               cargo = c("2ft", "3ft"),
               eng = c("gas", "hyb", "elec"),
               price = c("30", "35", "40"))
(new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ])
predict_mnl(m1, new.data)
# 7 lugares, 2ft, hibrido e 30 mil dolares eh o favorito

## Interpretando o modelo
# Ao inves de apresentar os coeficientes, os analistas fazem predicoes de escolhas ou 
# computam o quanto o consumidor deseja pagar (willingnes-to-pay) por cada atributo

m2 <- mlogit(choice ~ 0 + seat + cargo + eng + 
                    as.numeric(as.character(price)), data = cbc.mlogit)
summary(m2)

# Esse modelo pode ser converteu price para uma variavel numerica
# E coeficientes negativos sugerem que pessoas preferem pagar menores precos 
# do que maiores precos

# Nos estimagos um unico parametro para price
# Nos podemos calcular a media do "willingness-to-pay" para um nivel particular de atributo
# Para isso, dividimos o coeficiente do n?vel a ser analisado pelo coeficiente de price
# Por exemplo: Quanto as pessoas desejam pagar por
coef(m2)["cargo1"]/(-coef(m2)["as.numeric(as.character(price))"]/1000)
# Esse resultado mostra que em media, quando a diferenca do preco estah ateh 1375.3, 
# as pessoas sao indiferentes entre as duas opcoes de carreta (cargo).

## Comparando modelos
lrtest(m1, m2)

####################################################################################
#### Dados chocolate # DataCamp
# carregar o pacote  mlogit 
library(mlogit) # setwd(choose.dir())
sportscar_long <- read.csv("sportscar_choice_long.csv")
sportscar_long$seat<-as.factor(sportscar_long$seat)
sportscar <- mlogit.data(sportscar_long,
                       shape = "long", 
                       alt.var = "alt",
                       choice = "choice")
head(sportscar)

# ajustar modelo usando mlogit() 
model <- mlogit(choice ~ 0+seat+trans+convert+price, data=sportscar)
summary(model)

# Prevendo os shares do segmento de carros
# Your task is to modify my code to predict shares for the "racer" segment
# modify the code below so that the segement is set to "racer" for both alternatives
products <- data.frame(seat = factor(c(2, 2), levels=c(2,4,5)), 
                       trans= factor(c("manual", "auto"), 
                                     levels=c("auto", "manual")),
                       convert=factor(c("no", "no"), 
                                      levels=c("no", "yes")), 
                       price = c(35, 30), 
                       segment=factor(c("racer", "racer"), 
                                      levels=c("basic", "fun", "racer")))


# Prever escolhas (Share Prediction)
predict_mnl <- function(model, products) {  
  # model: objeto mlogit criado pela funcao mlogit()  
  # data: data frame contendo os designs q voce quer prever
  # data deve ter mesmo formato dos dados usados para estimar o modelo mlogit
  data.model <- model.matrix(update(model$formula, 0 ~ .), 
                             data = products)[,-1]  
  utility <- data.model %*% model$coefficients 
  share <- exp(utility)/sum(exp(utility))  
  cbind(share, products)
}

# predict shares for the "racer" segment
predict_mnl(model, products)


### Wide to long
chocolate_wide <- read.csv("chocolate_choice_wide.csv")
# use reshape() to change the data from wide to long 
chocolate_longo <- reshape(data= chocolate_wide , direction="long",
                     varying = list(Brand=3:5, Price=6:8, Type=9:11),
                     v.names=c("Brand", "Price", "Type"), timevar="Alt")

# Criar  `new_order` para o data.frame
new_order <- order(chocolate_longo$Subject, chocolate_longo$Trial, chocolate_longo$Alt)
# Reordernar data.frame com new_order
chocolate_longo <- chocolate_longo[new_order,]
# Criar novo nome das linhas
row.names(chocolate_longo)<-1:nrow(chocolate_longo)
# Transformar `Selection` em logico (alternativa `Alt` selecionada `Selection`)
chocolate_longo$Selection <- chocolate_longo$Alt==chocolate_longo$Selection

chocolate_long <- read.csv("dataset/chocolate_choice_long.csv") # esse é o dado bruto
# ver se sao iguais
head(chocolate_long)
head(chocolate_longo)

## Designing Conjoint Analysis
# use mlogit.data() para converter chocolate_long para mlogit.data
chocolate <- mlogit.data(chocolate_longo, # arquivo selecionado
                         shape = "long", # formato (pode ser wide ou long)
                         choice = "Selection", # variavel foi escolhida ou nao
                         alt.var = "Alt", # variavel de alternativa
                         varying = 6:8)   # variaveis explicativas                       
# use str() para confirmar que `chocolate` eh um objeto `mlogit.data`
str(chocolate)

# summary do modelo `choc_m1` com os interceptos
choc_m1 <- mlogit(Selection ~  Brand + Type + Price, data = chocolate)
summary(choc_m1)

# modificar para q mlogit fique sem os interceptos
choc_m2 <- mlogit(Selection ~ 0 + Brand + Type + Price, data = chocolate)
# summary de choc_m2 model
summary(choc_m2)
# Usa-se intercepto = 0 pois senao ele vai calcular um intercepto diferente para 
# cada vez que calcular um coeficiente diferente. 
# O intercepto não é identificado.

#############
## Willingness to Pay (WTP)
# eh o valor (em moeda) que faz um produto ser igualmente atrativo em relacao a outro produto

summary(choc_m2)

## Divida os coeficientes pelo
# valor negativo (pois as pessoas desejam pagar MENOS) da variavel `price` 
# (escala de $) = q eh o coeficiente 9
coef_price <- -1*coef(choc_m2)[9]
coef(choc_m2)/coef_price
# O valor de Dove (modelo nulo) sobre Ghirardelli eh $ 1.56.
# (razao do coeficiente `BrandGhirardelli` sobre o negativo do coeficiente de preço - coef_price).
# Ou seja, se Ghirardeli pode custar ate 1.56 a mais que Dove q as pessoas preferem essa marca
# Veja `TypeWhite` e interprete

##########
## Ver a interacao `Trial` e `Price`:

choc_m3 <- mlogit(Selection ~ 0 + Brand + Type + Price + Price:Trial, data = chocolate)
summary(choc_m3)

# O coeficiente para Price:Trial eh -0.0080398. 
# Isso significa que a medida q os respondentes respondiam os testes
# eles ficavam menos sensiveis ao preco
# ficando menos propensos a escolher o chocolate mais barato 
# ao longo do teste
# Mas nao foi significativo. Entao ok...
