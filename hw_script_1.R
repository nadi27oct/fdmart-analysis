#Installing required packages
install.packages("arules")

#Loading Libraries
library(arules)
library(readr)

transaction_list <- read_csv("C:/Users/nadyaw/Downloads/TransactionList.csv", col_names = FALSE, rm.duplicates=TRUE)
View(transaction_list)

#add header names to clean data
colnames(transaction_list) <- c("transaction_id","item")

transaction_list

#converting current transaction list dataframe to a transaction matrix
grocery_list <- as(split(transaction_list$item, transaction_list[,"transaction_id"]), "transactions")
grocery_list
inspect(head(grocery_list,5))

summary(grocery_list)

#items with a 10% support
itemFrequencyPlot(grocery_list, support=0.1, cex.names=0.8)

#item frequency plot for top 20 items
itemFrequencyPlot(grocery_list,topN=10,type="absolute")

rules_1 <- apriori(grocery_list, parameter = list(support = 0.001, conf = 0.8))

options(digits=2)
inspect(rules_1[1:5])
summary(rules_1)

rules <- apriori(Groceries, parameter = list(support=0.001, confidence=0.8))
inspect(rules[1:5])
summary(rules)

rules_2 <- apriori(grocery_list, parameter = list(support = 0.01, conf = 0.8))

options(digits=2)
inspect(rules_2[1:5])
summary(rules_2)
rules_2<-sort(rules_2, by="confidence", decreasing=TRUE)
inspect(rules_2[1:5])
data("Groceries")

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
summary(rules)
inspect(rules[1:5])

rules_2 <- apriori(grocery_list, parameter = list(support = 0.01, conf = 0.8))
winerules <- subset(rules_2, subset = rhs %pin% "Wine")
summary(winerules) #produced 0 rules

rules_3 <- apriori(grocery_list, parameter = list(support = 0.01, conf = 0.15))
rules_3<-sort(rules_3, by="lift")
inspect(rules_3[1:10])
winerules <- subset(rules_3, subset = rhs %pin% "Wine")
summary(winerules) #
winerules<-sort(winerules, decreasing=TRUE,by="confidence")
inspect(winerules[1:5])

crossTable(grocery_list, measure='lift',sort=T)[1:10,1:10]
crossTable(grocery_list, measure='chi')['Fresh Vegetables', 'Soup']
crossTable(grocery_list, measure='chi')['Fresh Vegetables', 'Canned Vegetables']
crossTable(grocery_list, measure='chi')['Beer', 'Wine']

crossTable(grocery_list)['Fresh Vegetables','Canned Vegetables']
crossTable(grocery_list)['Beer','Wine']
rules <- apriori(grocery_list, parameter = list(support=.001,conf = .01,minlen=2,
maxlen=2,target='rules'))
quality(rules)$chi  <- interestMeasure(rules, measure='chi', significance=T, grocery_list)
beer_wine <- subset(rules, lhs %pin% 'Beer' & rhs %pin% 'Wine')
summary(beer_wine)
inspect(beer_wine)

rules <- apriori(grocery_list, parameter = list(support=.001,conf = .08))
quality(rules)$chi  <- interestMeasure(rules, measure='chi', significance=T, grocery_list)
fresh_can <- subset(rules, lhs %pin% 'Fresh' & rhs %pin% 'Canned')
summary(fresh_can)
inspect(fresh_can)

is.redundant(rules_3, measure = "confidence")
inspect(rules_3[is.redundant(rules_3)])

