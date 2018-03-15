###################################################################
############# SAMPLE ##############################################
###################################################################

# Importando dataser
dataset = read.csv('dataset/car_data.csv', header = FALSE)

# Definindo variável dependente como fator
dataset$V7 = factor(dataset$V7)

# Preparando dados de treinamento e teste
library(caTools)
set.seed(123)
split = sample.split(dataset$V7, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

library(randomForest)
classifier <- randomForest(V7~., data=training_set, ntree=500, mtry=4, importance=TRUE)
# Verificando as probabilidades nos dados de teste
text (classifier);
plot(classifier);

prob_pred = predict(classifier, newdata = test_set[-7], type = "class")


# Criando a Matriz de Confusão
library(caret)
cm = table(test_set$V7, prob_pred)
confusionMatrix(cm)

