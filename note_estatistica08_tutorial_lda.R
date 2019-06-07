# Baixe/instale na biblioteca o pacote MASS
if(!require(MASS)){install.packages('MASS')}

# Baixe/instale na biblioteca os dados Boston e veja o cabeçalho dos dados 
data("Boston")
head(Boston)

# Centralize e padronize (standardize), usando a função scale as variáveis contidas na tabela Boston
boston_scaled <- scale(Boston)

# Verifique a classe do objeto boston_scaled 
class(boston_scaled)

# Mude o objeto para a classe data.frame
boston_scaled <- as.data.frame(boston_scaled)

# Veja o resumo dos dados escalonados com a função summary
summary(boston_scaled)

# Crie um vetor dos quantiis da coluna crim da tabela Boston, e a visualize
bins <- quantile(boston_scaled$crim);bins

# Crie uma variável categórica 'crime‘ da coluna crim a partir dos quantis (bins), dando os rótulos (labels) de ‘baixo’, ‘med_baixo’, ‘med_alto’ e ‘alto’.
crime <- cut(boston_scaled$crim,
             labels=c('baixo', 'medio_baixo', 'medio_alto', 'alto'), 
                   breaks = bins,  include.lowest = TRUE);
summary(crime)

# Baixe/instale na biblioteca o pacote MASS
if(!require(dplyr)){install.packages('dplyr')}
# Inclua o novo veto crime aos dados escalonados (já está feito)
boston_scaled <- data.frame(boston_scaled, crime)
boston_scaled <- dplyr::select(boston_scaled,-crim) 

# CUIDADO ERRAR!  # formas Opcionais
# boston_scaled$crim <- Null 
# boston_scaled <- boston_scaled[,-1]

set.seed(42)

ind = sample( nrow(boston_scaled) , size = nrow(boston_scaled) * 0.8)
treino  =  boston_scaled[ind,]
teste  =  boston_scaled[-ind,]

# salve as classes corretas de crime dos dados teste
classes_correstas = teste$crime

pairs(treino[,1:7])
pairs(treino[,8:13])

# avaliar a distribuicao normal da variavel 3, no grupo
shapiro.test(treino [treino$crime== "alto" , 3 ])

# Avaliar se Matrizes de dispersão são iguais.
# Se p<0.05 Matrizes de Var-Covar nao sao iguais
# if(!require(biotools)){install.packages('biotools')};require (biotools)
# biotools::boxM(treino[, 3], treino$crime)

if(!require(GGally)){install.packages('GGally')};require (GGally)
ggpairs(treino, aes (color = crime))

### Centroides
lda_boston <- lda(crime~.,data=treino);
# 1º: obter valores dos LDs para cada observação
LD_treino <- predict(lda_boston, treino)$x
LD_treino <- as.data.frame(LD_treino)
# 2º. Criar coluna com os nomes das classes a que pertencem
LD_treino$crime<-treino$crime


# funcoes descriminantes
head(LD_treino)

# 3º. Construir gráfico com os grupos em relação as FDs

if(!require(ggplot2)){install.packages("ggplot2")}; require (ggplot2)

ggplot( LD_treino,# FDs 1 e 2
        aes(x=LD1, y=LD2, shape=crime, color=crime))+
        geom_point()+
        stat_ellipse(aes(x=LD1,y=LD2,color=crime),type='norm')

centroides = data.frame (
LD1 = tapply(LD_treino$LD1, LD_treino$crime,mean),
LD2 = tapply(LD_treino$LD2, LD_treino$crime,mean),
LD3 = tapply(LD_treino$LD3, LD_treino$crime,mean)
)

# centroide do grupo "alto", coordenada LD
mean(LD_treino$LD1[LD_treino$crime=="alto"])
mean(LD_treino$LD2[LD_treino$crime=="alto"])
mean(LD_treino$LD3[LD_treino$crime=="alto"])

# Gráfico em 3D interativo
if(!require(plotly)) {install.packages("plotly")}; require(plotly)
plotly::plot_ly(LD_treino, x=~LD1, y=~LD2,z=~LD3,color=~crime)

#### Passo 5. Validação
predicao <- predict(lda_boston, teste)      # testar modelo
predicao$posterior                          # probabilidades de pertencer a cada grupo
reais = teste$crime
previsto = predicao$class                   # valores previstos conforme modelo
confusao = table(previsto,reais); confusao  # Tabela confusao

N = length(reais)
n = sum (reais == previsto)
K = length(levels(reais))

# Q-Quad - frequencia esperada vs frequencia observada
# --- Calculo falta acurassia na conclusao,
# --- para esse caso observamos um erro do tipo ou seja um falso positivo
qpress = ((N-(n*K))^2/(N*(K-1))); 1-pchisq (qpress, 3)

# Modelo melhor associado
mean(diag(confusao)/ table(reais))

#A ser considerado em Análises Discriminantes:
#  Considerações sobre distribuição normal multivariada é relativo às variáveis resposta. Isso significa que:
#  Cada variável dependente (y) apresenta distribuição normal dentro de cada grupo;
#Qualquer combinação linear das variáveis dependentes apresenta distribuição normal;
#Todos os subconjuntos das variáveis devem ser normais
#Cada grupo deve ter um número suficientemente grande de casos;
#Diferentes métodos de classificação depende se as matrizes de variância-covariância são
#iguais (ou similares) entre os grupos.  LDA ou QDA (próxima disciplina)
#Análises não-paramétricos para discriminação de grupos também podem ser realizados  KNN

ggp <- GGally::ggpairs(treino, aes(color=class))
print(ggp, progress = F)  
