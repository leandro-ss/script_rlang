###################################################################
############# MONITORADO ##########################################
###################################################################

# Importando dataser
dataset = read.csv('dataset/car_data.csv', header = FALSE)

# Definindo variável dependente como fator
dataset$V1 = factor(dataset$V1)

# Preparando dados de treinamento e teste
library(caTools)
set.seed(123)
split = sample.split(dataset$V1, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Classificador Naive Bayes
library(e1071)
classifier = naiveBayes(x = training_set[-1], y = training_set$V1)

# Classificador SVM
classifier <- svm(V1~., data=training_set, method='C-classification', kernal='radial', gamma=0.1, cost=10)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, newdata = test_set[-1])

# Criando a Matriz de Confusão
library(caret)
cm = table(test_set[, 1], prob_pred)
confusionMatrix(cm)

###############################################################
########### EXERCICIO #########################################
###############################################################

library(readr)
dataset <- read_delim("dataset/waveform.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Definindo variável dependente como fator
dataset$X22 = factor(dataset$X22)

# Preparando dados de treinamento e teste
library(caTools)
set.seed(123)
split = sample.split(dataset$X22, SplitRatio = 0.5)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


library(e1071)

# Classificador SVM
classifier <- svm(X22~., data=training_set, method='C-classification', kernal='radial', gamma=0.1, cost=10)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, newdata = test_set[-22])

# Criando a Matriz de Confusão
library(caret)
cm = table(test_set$X22, prob_pred)
confusionMatrix(cm)
