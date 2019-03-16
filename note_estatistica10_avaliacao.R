#Avalidacao – Estatistica Multivariada IV Analise Discriminante Multipla

# Nome: Leandro Sampaio

#Considere o conjunto Auto data do pacote ISLR do software R para desenvolver um 
#modelo de predicao para prever se um carro tem alta ou baixa quilometragem.

  if(!require(ISLR)){install.packages('ISLR')};
  if(!require(foreign)){install.packages('foreign')};
  if(!require(MASS)){install.packages('MASS')};
  if(!require(dplyr)){install.packages('dplyr')};
  if(!require(heplots)){install.packages('heplots')};
  if(!require(ggplot2)){install.packages('ggplot2')};
  if(!require(DiscriMiner)){install.packages('DiscriMiner')};
  if(!require(mvnormtest)){install.packages('mvnormtest')};
  if(!require(rpart)){install.packages('rpart')};
  
  set.seed(0);head(Auto);str(Auto);dataset <- Auto;
  
  dataset <-subset(dataset, select=-c(name))
  dataset$mpgclass <- as.factor(dataset$mpgclass)
  table(dataset$mpgclass)
  str (dataset)

#(a) Crie uma variavel binaria,classmpg, que seja igual a 1 se o mpg for maior  doque a mediana e 0,
# caso contrario. Voce pode calcular a mediana de mpg no R usando a função median()
  dataset$mpgclass = NA;
  dataset[median(dataset$mpg) >= dataset$mpg,]$mpgclass = 1 
  dataset[median(dataset$mpg) <  dataset$mpg,]$mpgclass = 0 
  
  cor(dataset[,2:7])
  
#(b) Explore os dados graficamente para investigar a associacao entre o classmpg e as demais variaveis.
#Quais variaveis  parecem ser uteis para prever o classmpg? Pode usar boxplots para responder a essa questão.

  str(dataset); cor( subset(dataset, select=-c(mpgclass)))
  mshapiro.test(t( subset(dataset, select=-c(mpgclass)) ))
  discPower(
    variables = subset(dataset, select=-c(mpgclass)), 
    group = dataset$mpgclass
  )
  boxplot(mpg ~ mpgclass ,data=dataset,main="mpg") 
  boxplot(horsepower ~ mpgclass ,data=dataset,main="horsepower") 
  boxplot(displacement ~ mpgclass ,data=dataset,main="displacement") 
  boxplot(cylinders ~ mpgclass ,data=dataset,main="cylinders") 

#(c) Divida os dados em duas amostras, uma de treino (75%) e outra de teste (25%).

  treino <- dataset %>% sample_frac(.75); nrow(treino)
  teste  <- dataset %>% setdiff(treino); nrow(teste)

#(d) Obtenha e interprete as funcoes discriminantes para esse estudo.
#Verifique tambem as suposicoes da analise Voce utilizaria a analise
#discriminante linear ou quadratica para classificacao?
  
  data_subset_treino <- subset(treino, select=-c(mpgclass))
  data_subset_teste  <- subset(teste, select=-c(mpgclass))
  
  # LINEAR
  fit_linDA <- linDA(
    variables = data_subset_treino, 
    group = treino$mpgclass
  )
  classif    <- classify( fit_linDA, newdata = data_subset_teste )$pred_class
  tab_fuzzy  <- table( classif, teste$mpgclass);
  sucess_hit <- (tab_fuzzy[1,1] + tab_fuzzy[2,2])/sum(tab_fuzzy)
  
  paste("O modelo discriminante linear apresenta uma taxa de erro:",
        1 - round(sucess_hit,2),
        ',tendo o melhor resultado entre todos os modelos analisados')
  
  # QUADRATICA
  fit_quaDA <- quaDA(
    variables = data_subset_treino, 
    group = treino$mpgclass
  )
  classif    <- classify( fit_quaDA, newdata = data_subset_teste )$pred_class
  tab_fuzzy  <- table( classif, teste$mpgclass);
  sucess_hit <- (tab_fuzzy[1,1] + tab_fuzzy[2,2])/sum(tab_fuzzy)
  
  paste("O modelo discriminante quadratico apresenta uma taxa de erro:",
        1 - round(sucess_hit,2),
        ',sendo infeior ao modelo discriminante linear')
  

  ################################################################################################    
  #R: No caso a taxa de acerto do modelo de testes foi maior considerando o modelo linear
  ################################################################################################
  
#(e) Compare a LDA e a QDA com relacao a taxa de erro.

  fit_linDA$error_rate
  fit_quaDA$error_rate

  ################################################################################################    
  #R: O percentual de erro do modelo quadratico se mostra um pouco maior
  ################################################################################################

#(f) Faca uma regressao logistica a e avalie sua taxa de erro de acordo com alguma regra de classificacao.
  ajuste_reg_log <- glm( 
    mpgclass ~  horsepower + displacement + cylinders,
    family = binomial(link = 'logit'),
    data = dataset
  )
  p <- mean(as.numeric (dataset$mpgclass))
  log_chances <- predict.glm(
    ajuste_reg_log, 
    newdata = data_subset_teste
  )
  prob_posteriori <- exp(log_chances)/(1+exp(log_chances))
  classificacoes <- ifelse(prob_posteriori > p, 1, 0) 
  tab_confusao <- table(classificacoes, teste$mpgclass)
  taxa_acerto <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)

  paste("O modelo logistico apresenta uma taxa de erro:",
        1 - round(taxa_acerto,2),
        ',sendo infeior ao modelo discriminante linear')
  
#(g) Faça agora uma árvore de decisão e avalie sua taxa de erro.
  fit_tree <- rpart(
    mpgclass ~  horsepower + displacement + cylinders,
    data = treino,
    method="class"
  )
  melhorCp <- fit_tree$cptable[which.min(fit_tree$cptable[,"xerror"]),"CP"]
  pfit <- prune(fit_tree, cp = melhorCp)
  classificacoes <- predict(pfit, data_subset_teste, type = 'class')
  tab_confusao <- table(classificacoes, teste$mpgclass)
  taxa_acerto <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
   
  paste("O modelo de arvore de decisao apresenta uma taxa de erro:",
        1 - round(taxa_acerto,2),
        ',sendo infeior ao modelo discriminante linear')
  
#(h) Utilize o metodo dos vizinhos mais proximos com k = 30 e avalie sua taxa de erro.
  ajuste <- knn(
    train = data_subset_treino,
    test = data_subset_teste, 
    cl = treino$mpgclass,
    k = 30)
  tab_confusao <- table(ajuste, teste$mpgclass)
  taxa_acerto <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
  
  paste("O modelo de vizinhos mais proximos apresenta uma taxa de erro:",
        1 - round(taxa_acerto,2),
        ',sendo infeior ao modelo discriminante linear')
  