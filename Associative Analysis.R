# 1. Load the data

library(arules)
Df = read.transactions('http://bit.ly/SupermarketDatasetII', sep = ",")
Df


# Check the class of the object

class(Df)

# Preview the first 5 transactions

inspect(Df[1:5])

# Check a summary of the dataset. 

summary(Df)

# Exploring the frequency of some articles.
# View transactions ranging from 10 to 12 and performing 
# some operation in percentage terms of the total transactions 

itemFrequency(Df[, 10:12],type = "absolute")
round(itemFrequency(Df[, 10:12],type = "relative")*100,2)

# plot the frequency of items
itemFrequencyPlot(Df, topN = 10,col="darkgreen")
itemFrequencyPlot(Df, support = 0.1,col="darkred")

# Building a model based on association rules using the apriori function

# We use Min Support as 0.001 and confidence as 0.8

rules <- apriori (Df, parameter = list(supp = 0.001, conf = 0.8))
rules

# Building a apriori model with Min Support as 0.002 and confidence as 0.8

rules2 <- apriori (Df, parameter = list(supp = 0.002, conf = 0.8)) 
rules2

# Building apriori model with Min Support as 0.002 and confidence as 0.6

rules3 <- apriori (Df, parameter = list(supp = 0.001, conf = 0.6)) 
rules3

summary(rules)

# Observing rules built in our model i.e. first 5 model rules

inspect(rules[1:5])


# Order these rules by a criteria such as the level of confidence
# then looking at the first five rules.

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

# 1. Determine items that customers who previously bought eggs might buy 

eggs <- subset(rules, subset = lhs %pin% "eggs")

# Then order by confidence
eggs <-sort(eggs, by="confidence", decreasing=TRUE)
inspect(eggs[1:5])


# 2. Determine items that customers who previously bought turkey might buy 

turkey <- subset(rules, subset = lhs %pin% "turkey")

# Then order by confidence
turkey <-sort(turkey, by="confidence", decreasing=TRUE)
inspect(turkey[1:3])


# 3. Determine which items customers bought before purchasing chocolate

chocolate <- subset(rules, subset = rhs %pin% "chocolate")

# Then order by confidence
chocolate <-sort(chocolate, by="confidence", decreasing=TRUE)
inspect(chocolate[1:2])



