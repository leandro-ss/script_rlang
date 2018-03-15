###################################################################
############# MONITORADO ##########################################
###################################################################

library(readr)

bcw <- read_csv("dataset/bcw.csv", col_names = FALSE)

bcw[is.na(bcw)] <- as.integer (0)

bcw$X11 = factor(bcw$X11, levels = c('2','4'), labels = c(0,1))

# Preparando dados de treinamento e teste
library(caTools)
set.seed(1)

split = sample.split(bcw$X11, SplitRatio = 0.75)
training_set = subset(bcw, split == TRUE)
test_set = subset(bcw, split == FALSE)

library(e1071)

classifier = naiveBayes(x = training_set[-11], y = training_set$X11)

# Verificando as probabilidades nos dados de teste
prob_pred = predict(classifier, newdata = test_set[-11])

# Criando a Matriz de Confusão
cm = table(test_set$X11, prob_pred)

library(caret); confusionMatrix(cm)
