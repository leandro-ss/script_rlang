###################################################################
################# SAMPLE ##########################################
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

library(tree)
classifier_tree <-tree(V7 ~ ., data=training_set)

library(rpart)
classifier_rpart <-rpart(V7 ~ ., data=training_set)

plot (training_set)

#print(classifier);
text (classifier_tree);
text (classifier_rpart);
plot(classifier_tree);
plot(classifier_rpart);

#https://stackoverflow.com/questions/23085096/type-parameter-of-the-predict-function
prob_pred = predict(classifier, newdata = test_set[-7], type = "class")

library(caret)
cm = table(test_set$V7, prob_pred)
confusionMatrix(cm)


###################################################################
################# SAMPLE ##########################################
###################################################################
