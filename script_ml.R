###################################################################
################# MONITORADO ######################################
###################################################################

library(readr)
wdbc <- read_csv("dataset/wdbc.csv",
                 col_names = FALSE, col_types = cols(X1 = col_skip()))

# Definindo variável dependente como fator
wdbc$X2 = factor(wdbc$X2, levels = c('B','M'), labels = c(0,1))

# Importando dataset
wdbc = wdbc[-1]


# Preparando dados de treinamento e teste

library(caTools)
set.seed(1)

#split = sample.split(wdbc$X2, SplitRatio = 0.75)
training_set = subset(wdbc, split == TRUE)
test_set = subset(wdbc, split == FALSE)

# Ajustando a escala das variáveis dependente
training_set[-1] = scale(training_set[-1])
test_set[-1] = scale(test_set[-1])

# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X2 ~ ., family = binomial, data = training_set)

# Visualizando modelo
summary(classifier)

##########################
#### INCONGRUENTE ########
##########################

library(readr)

bcw <- read_csv("C:/dev/workspace_r/br.com.ml/R/bcw.csv",
                col_names = FALSE, col_types = cols(X1 = col_skip()))

bcw[is.na(bcw)] <- as.integer (0)

bcw$X11 = factor(bcw$X11, levels = c('2','4'), labels = c(0,1))


# Preparando dados de treinamento e teste
# install.packages('caTools')
library(caTools)
set.seed(123)

split = sample.split(bcw$X11, SplitRatio = 0.75)
training_set = subset(bcw, split == TRUE)
test_set = subset(bcw, split == FALSE)

# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X11 ~ ., family = binomial, data = training_set)

# Visualizando modelo
summary(classifier)


# Removendo variaveis fracas
print(training_set)
training_set = training_set[-2]
print(training_set)
# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X11 ~ ., family = binomial, data = training_set)
# Visualizando modelo
summary(classifier)



# Removendo variaveis fracas
print(training_set)
training_set = training_set[-4]
print(training_set)
# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X11 ~ ., family = binomial, data = training_set)
# Visualizando modelo
summary(classifier)


# Removendo variaveis fracas
print(training_set)
training_set = training_set[-7]
print(training_set)
# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X11 ~ ., family = binomial, data = training_set)
# Visualizando modelo
summary(classifier)


print(training_set)
training_set = training_set[-3]
print(training_set)
# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = X11 ~ ., family = binomial, data = training_set)

# Visualizando modelo
summary(classifier)



test_set = test_set[-2]
test_set = test_set[-4]
test_set = test_set[-7]
test_set = test_set[-3]


library(naivebayes)
classifier = naive_bayes(x = training_set[-6], y = training_set$X11)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, type = 'response', newdata = test_set[-6])
prob_pred = predict(classifier, newdata = test_set[-6])

# Criando a Matriz de Confusão
y_pred = ifelse(prob_pred > 0.3, 1, 0)
cm = table(test_set$X11, y_pred)
cm = table(test_set[,6], prob_pred)
cm




# Verificando modelo (Correto e Falso Positivo)
#install.packages('ROCR')

# library(ROCR)
# pred = predict(classifier, type="response", newdata = test_set[-10])
# predObj = prediction(pred, test_set$X11)

# rocObj = performance(predObj, measure="tpr", x.measure="fpr")
# aucObj = performance(predObj, measure="auc")
# plot(rocObj, main = paste("Area under the curve:",
# round(aucObj@y.values[[1]] ,4)))

# # Extraindo os Thresholds para Correto e Falso Positivo
# alpha <- round(as.numeric(unlist(rocObj@alpha.values)),4)
# fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
# tpr <- round(as.numeric(unlist(rocObj@y.values)),4)
# par(mar = c(5,5,2,5))
# plot(alpha,tpr, xlab="Threshold", xlim=c(0,1),
# ylab="True positive rate", type="l")

# par(new="True")
# plot(alpha,fpr, xlab="", ylab="", axes=F, xlim=c(0,1), type="l" )
# axis(side=4)
# mtext(side=4, line=3, "False positive rate")
# text(0.18,0.18,"FPR")
# text(0.58,0.58,"TPR")


# Visualisando os dados do classificador
#install.packages('ElemStatLearn')
#library(ElemStatLearn)
# set = test_set

# X2 = seq(min(set$X2) - 1, max(set$X2) + 1, by = 0.01)
# X4 = seq(min(set$X4) - 1, max(set$X4) + 1, by = 0.01)
# X5 = seq(min(set$X5) - 1, max(set$X5) + 1, by = 0.01)
# X7 = seq(min(set$X7) - 1, max(set$X7) + 1, by = 0.01)
# X8 = seq(min(set$X8) - 1, max(set$X8) + 1, by = 0.01)
# X9 = seq(min(set$X9) - 1, max(set$X9) + 1, by = 0.01)

# #install.packages('data.table')
# #library(data.table)

# grid_set = expand.grid(X2)

# print (grid_set)

# prob_set = predict(classifier, type = 'response', newdata = grid_set)


##########################################################################################
##########################################################################################
##########################################################################################



# Regress?o e Classifica??o Log?stica

# Importando dataser
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Definindo vari?vel dependente como fator
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Preparando dados de treinamento e teste
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Ajustando a escala das vari?veis dependente
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Criando o modelo baseado nos dados de treinamento
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Visualizando modelo
summary(classifier)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
prob_pred

# Verificando modelo (Correto e Falso Positivo)
#install.packages('ROCR')
library(ROCR)
pred = predict(classifier, type="response")
predObj = prediction(pred, training_set$Purchased)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:",
                          round(aucObj@y.values[[1]] ,4)))

# Extraindo os Thresholds para Correto e Falso Positivo
alpha <- round(as.numeric(unlist(rocObj@alpha.values)),4)
fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
tpr <- round(as.numeric(unlist(rocObj@y.values)),4)
par(mar = c(5,5,2,5))
plot(alpha,tpr, xlab="Threshold", xlim=c(0,1),
     ylab="True positive rate", type="l")
par(new="True")
plot(alpha,fpr, xlab="", ylab="", axes=F, xlim=c(0,1), type="l" )
axis(side=4)
mtext(side=4, line=3, "False positive rate")
text(0.18,0.18,"FPR")
text(0.58,0.58,"TPR")

# Criando a Matriz de Confus?o
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[, 3], y_pred)
cm

# Visualisando os dados do classificador
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3], main = 'Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
