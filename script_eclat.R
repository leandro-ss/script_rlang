###################################################################
############# NAO MONITORADO ######################################
###################################################################

# Data Preprocessing

library(arules)
dataset = read.csv('dataset/market_basket_optimisation.csv')
dataset = read.transactions('dataset/market_basket_optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Eclat on the data
rules = eclat(data = dataset, parameter = list(support = 0.003, minlen = 2))
# Visualising the results
inspect(sort(rules, by = 'support')[1:10])

# Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))
# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
