#Let's try to break Sam's Zooper function. 

source("Zoop synthesizer function.R")

EMPtest = Zooper(Sources = "EMP", Data = "Community")
table(EMPtest$Year)

#So that works fine, but when I ask for the same thing in the Shiny app I get:
EMPtest2 <- read.csv("C:/Users/rhartman/Desktop/data-2019-07-16 EMP.csv")
table(EMPtest2$Year)

EMP1985 = filter(EMPtest, Year == 1985)
#I don't know why shiny doesn't like the 1980s. It doesn't have all the data for other years too

EMP1999a = filter(EMPtest, Year == 1999)
EMP1999b = filter(EMPtest2, Year == 1999)

#####################################################################################
#Some random thoughts

#1. Is there a reason we don't set the default parameter values to -Inf, Inf (or something like that)?