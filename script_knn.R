###################################################################
############# MONITORADO ##########################################
###################################################################

set.seed(123)

data(iris)

n <- nrow(iris)
ntrain <- round(n*0.6)
tindex <- sample(n, ntrain)
train_iris <- iris[tindex,]
test_iris <- iris[-tindex,]

plot(train_iris$Petal.Length, train_iris$Petal.Width, pch=c(train_iris$Species))
legend('topleft', legend=c("setosa", "versicolor", "verginica"), pch=c(1,2,3), bty="o")

library(class)

train_x <- train_iris[,-5]
train_y <- train_iris[,5]
test_x <- test_iris[,-5]
test_y <- test_iris[,5]
prediction <- knn(train_x, test_x, train_y, k=5)

cm = table(prediction, test_iris$Species)

library(caret); confusionMatrix(cm)



###########################################################################
#### EXERCICIO_1 ##########################################################
###########################################################################

library(readr)
dataset <- read_delim("dataset/waveform.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

dataset[is.na(dataset)] <- as.integer (0)

dataset$X22 = factor(dataset$X22)

n <- nrow(dataset)
ntrain <- round(n*0.6)
set.seed(123)
tindex <- sample(n, ntrain)
training_set <- dataset[tindex,]
test_set <- dataset[-tindex,]

# newcol <- data.frame(isVersicolor=(train_iris$Species=='versicolor'))
# train_iris <- cbind(train_iris, newcol)
# plot(train_iris$Petal.Length, train_iris$Petal.Width, pch=c(train_iris$Species))
# legend('topleft', legend=c("setosa", "versicolor", "verginica"), pch=c(1,2,3), bty="o")

library(class)

prediction <- knn(training_set[,-22], test_set[,-22], training_set$X22, k=5)

cm = table(prediction, test_set$X22)

library(caret); confusionMatrix(cm)


###########################################################################
#### EXERCICIO_2 ##########################################################
###########################################################################

library(readr)

bcw <- read_csv("dataset/bcw.csv",
                col_names = FALSE, col_types = cols(X1 = col_skip()))

bcw[is.na(bcw)] <- as.integer (0)

bcw$X11 = factor(bcw$X11, levels = c('2','4'), labels = c(0,1))


# Preparando dados de treinamento e teste
library(caTools)
set.seed(1)

split = sample.split(bcw$X11, SplitRatio = 0.75)
training_set = subset(bcw, split == TRUE)
test_set = subset(bcw, split == FALSE)

library(class)

classifier = knn(training_set[-10], test_set[-10], training_set$X11, k=5)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, newdata = test_set[-10])

# Criando a Matriz de Confusão
cm = table(test_set$X11, prob_pred)

library(caret); confusionMatrix(cm)