install.packages("arules")

library(arules)
data(Adult)

rules <- apriori(Adult, parameter = list(supp = 0.5, conf = .75, target = "rules"))
summary(rules)
inspect(head(rules))
