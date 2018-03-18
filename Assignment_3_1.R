getwd()
setwd("E:/R")

transData<-read.csv("Marketbasket.csv")
class(transData)

myTransactions<-as(transData,"transactions") #convert into Transaction object
class(myTransactions)

transItemSets<-apriori(myTransactions,
                       parameter = 
                         list(minlen = 1,maxlen =1,
                              support=0.02,target="frequent itemsets"))

inspect(head(sort(transItemSets,by="support"),10))


myTransRules<-apriori(myTransactions,parameter = 
                        list(minlen = 1,maxlen =2,support=0.001,
                             confidence=0.01,target="rules")) # For two products

inspect(head(sort(myTransRules,by="lift"),10))

myTransRules<-apriori(myTransactions,parameter = 
                        list(minlen = 1,maxlen =3,support=0.001,
                             confidence=0.02,target="rules")) # For Three products

inspect(head(sort(myTransRules,by="lift"),10))
