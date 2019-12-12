if(!require(conjoint)){install.packages("conjoint")}
if(!require(cluster)){install.packages("cluster")}
if(!require(mlogit)){install.packages("mlogit")}

### Wide to long
chocolate_wide <- read.csv("dataset/chocolate_choice_wide.csv")
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
choc_m1 <- mlogit(Selection ~ 0+ Brand + Type + Price, data = chocolate)
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
