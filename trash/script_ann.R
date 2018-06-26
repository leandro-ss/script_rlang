###################################################################
############# MONITORADO ##########################################
###################################################################

# Carregando dados e preparando set de treinamento e teste

data(iris)
n <- nrow(iris)
ntrain <- round(n*0.6)
set.seed(333)
tindex <- sample(n, ntrain)
train_iris <- iris[tindex,]
test_iris <- iris[-tindex,]

# Preparando dados para ANN

nn1_iristrain <- train_iris
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == 'setosa')
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == 'versicolor')
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == 'virginica')
names(nn1_iristrain)[6] <- 'setosa'
names(nn1_iristrain)[7] <- 'versicolor'
names(nn1_iristrain)[8] <- 'virginica'
head(nn1_iristrain[,5:8])

# Instalando pacote e rodando ANN
library(neuralnet)

nn1 <- neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                 data=nn1_iristrain,
                 hidden=c(4))

print(nn1)

plot(nn1)

# Construindo a matrix de confusao
prediction <- compute(nn1, test_iris[-5])
prediction <- prediction$net.result
pred_idx <- function(x) {return(which(x==max(x)))}
idx <- apply(prediction, c(1), pred_idx)
prediction_nn <- c('setosa', 'versicolor', 'virginica')[idx]
xtab <- table(prediction_nn, test_iris$Species)

library(caret)
confusionMatrix(xtab)

###############################################################
########### EXERCICIO #########################################
###############################################################

# Carregando dados e preparando set de treinamento e teste
library(readr)
dataset <- read_delim("dataset/waveform.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

n <- nrow(dataset)
ntrain <- round(n*0.6)
set.seed(123)
tindex <- sample(n, ntrain)
train_dataset <- dataset[tindex,]
test_dataset <- dataset[-tindex,]

# Preparando dados para ANN

nn1_datasettrain <- train_dataset
nn1_datasettrain <- cbind(nn1_datasettrain, train_dataset$X22 == 0)
nn1_datasettrain <- cbind(nn1_datasettrain, train_dataset$X22 == 1)
nn1_datasettrain <- cbind(nn1_datasettrain, train_dataset$X22 == 2)
names(nn1_datasettrain)[23] <- 'A'
names(nn1_datasettrain)[24] <- 'B'
names(nn1_datasettrain)[25] <- 'C'

# Instalando pacote e rodando ANN

#install.packages('neuralnet')
library(neuralnet)

nn1 <- neuralnet(A+B+C~   X1
                         +X2
                         +X3
                         +X4
                         +X5
                         +X6
                         +X7
                         +X8
                         +X9
                         +X10
                         +X11
                         +X12
                         +X13
                         +X14
                         +X15
                         +X16
                         +X17
                         +X18
                         +X19
                         +X20
                         +X21,
                 data=nn1_datasettrain,
                 hidden=c(4))

# Construindo a matrix de confusao
prediction <- compute(nn1, test_dataset[-22])
prediction <- prediction$net.result
pred_idx <- function(x) {return(which(x==max(x)))}
idx <- apply(prediction, c(1), pred_idx)
prediction_nn <- c('0', '1', '2')[idx]
xtab <- table(prediction_nn, test_dataset$X22)

library(caret)
confusionMatrix(xtab)
