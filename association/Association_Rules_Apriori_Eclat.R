# Apriori
library(arules)
dataset = read.csv('C:/Desktop/Market.csv', header=FALSE)
dataset = read.transactions('Market.csv', sep= ',', rm.duplicates=TRUE)
summary (dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset - Support and confidence
rulesap = apriori(data = dataset, parameter = list (support = 0.003, confidence = 0.2))

# Visualising the results
inspect (sort(rulesap, by = 'lift')[1:10])

# Eclat

# Apriori simplified - only support - sets not rules
ruleseclat = eclat(data = dataset, parameter = list (support = 0.003, minlen=2))

# Visualising the results - we have sets not rules
inspect (sort(ruleseclat, by = 'support')[1:10])