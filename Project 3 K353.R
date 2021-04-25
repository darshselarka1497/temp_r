## Project 3 ##

# Problem 1 #

best_home <- read.csv('BestHomePart3.csv', header = T, as.is = T)

#Part 1: Data Understanding#

train_size <- 0.7 * nrow(best_home)
set.seed(500)
train_index <- sample(x = 1:nrow(best_home), size = train_size, replace = F)
train_set <- best_home[train_index, ]
valid_set <- best_home[-train_index, ]

best_home$Class <- as.factor(best_home$Class)
best_home$Assembly_status <- as.factor(best_home$Assembly_status)
best_home$Material_type <- as.factor(best_home$Material_type)
#In order to use these variables the right way in our regressions,
#they must be converted into categorical variables.

summary(best_home)


#lets first start with quantitative variables vs. demand

#weekly discount
plot(best_home$WeeklyDiscount,best_home$WeeklyDemand)
cor(best_home$WeeklyDiscount,best_home$WeeklyDemand)
hist(best_home$WeeklyDiscount)

#selling price
plot(best_home$Selling_price,best_home$WeeklyDemand)
cor(best_home$Selling_price,best_home$WeeklyDemand)

#purchasing cost
plot(best_home$Purchasing_cost,best_home$WeeklyDemand)
cor(best_home$Purchasing_cost,best_home$WeeklyDemand)

cor(best_home$Purchasing_cost,best_home$Selling_price)
#you would not want variables that are that highly correlated,
#so we will use purchasing cost and selling price in one of each.

#required capacity
plot(best_home$Required_capacity,best_home$WeeklyDemand)
cor(best_home$Required_capacity,best_home$WeeklyDemand)

#now lets move to the categorical variables vs. demand

#class
table(best_home$Class)
barplot(table(best_home$Class))
#there is more of a even distribution, so we will include class.
barchart(best_home$Class)
#assembly status
table(best_home$Assembly_status)

#material type
table(best_home$Material_type)



#now we can look at the measures for weekly demand
mean(best_home$WeeklyDemand)

indicies_Decor <- which(best_home$Class == "Decor")
hist(best_home$WeeklyDemand[indicies_Decor],
     main = "Histogram of Decor Class Demand",
     xlab = "Weekly Demand")

indicies_bb <- which(best_home$Class == "Decor")
hist(best_home$WeeklyDemand[indicies_bb],
     main = "Histogram of Bedding and Bath Class Demand",
     xlab = "Weekly Demand")

#Part 2: Modeling#

#Linear Regression#

#First linear regression#
lin_reg1 <- lm(formula = WeeklyDemand ~ Selling_price + WeeklyDiscount + Required_capacity + Class , data = train_set)
summary(lin_reg1)

#formula turned out to be

# Weekly Demand = 43.47 + 

#Second linear regression#
lin_reg2 <- lm(formula = WeeklyDemand ~ Material_type + WeeklyDiscount + Selling_price , data = train_set)
summary(lin_reg2)



#Comparing regression in terms of goodness of fit

pred_dem1 <- predict(object = lin_reg1, newdata = valid_set)
accuracy(pred_dem1, valid_set$WeeklyDemand)

pred_dem1_train <- predict(object = lin_reg1, newdata = train_set)
accuracy(pred_dem1_train, train_set$WeeklyDemand)


pred_dem2 <- predict(object = lin_reg2, newdata = valid_set)
accuracy(pred_dem2, valid_set$WeeklyDemand)

pred_dem2_train <- predict(object = lin_reg2, newdata = train_set)
accuracy(pred_dem2_train, train_set$WeeklyDemand)

#First Decision Tree#
library(rpart)
library(rpart.plot)
reg_tree <- rpart(formula = WeeklyDemand ~  WeeklyDiscount + Selling_price + Material_type , train_set, method="anova")
prp(reg_tree)


#Second Decision Tree#
library(rpart)
library(rpart.plot)
reg_tree2 <- rpart(formula = WeeklyDemand ~ Class + Assembly_status + Material_type +
                     Required_capacity + WeeklyDiscount + Purchasing_cost , train_set, method="anova", minsplit = 150)
prp(reg_tree2)

#Prediction performances of Decision Trees

library("forecast")

#performance of tree 1
pred_regtree <- predict(reg_tree,train_set)
accuracy(pred_regtree,train_set$WeeklyDemand)

pred_regtree_v <- predict(reg_tree,valid_set)
accuracy(pred_regtree_v,valid_set$WeeklyDemand)


#performance of tree 2
pred_regtree2 <- predict(reg_tree2,train_set)
accuracy(pred_regtree2,train_set$WeeklyDemand)

pred_regtree2_v <- predict(reg_tree2,valid_set)
accuracy(pred_regtree2_v,valid_set$WeeklyDemand)


#Part 3: Choosing the Best Model#

best_home[170,]







# Problem 2 #

quantile <- quantile(best_home$WeeklyDemand)[4]
best_home$DemandLevel <- ifelse(best_home$WeeklyDemand>=quantile,1,0)
best_home$DemandLevel <- as.factor(best_home$DemandLevel)
class(best_home$DemandLevel)

#Part 1: Data Understanding#
train_size <- 0.7 * nrow(best_home)
set.seed(500)
train_index <- sample(x = 1:nrow(best_home), size = train_size, replace = F)
train_set <- best_home[train_index, ]
valid_set <- best_home[-train_index, ]

#lets first start with quantitative variables vs. demand level

#weekly discount
boxplot(best_home$WeeklyDiscount ~ best_home$DemandLevel)

#selling price
boxplot(best_home$Selling_price ~ best_home$DemandLevel)

#purchasing cost
boxplot(best_home$Purchasing_cost ~ best_home$DemandLevel)

#required capacity
boxplot(best_home$Required_capacity ~ best_home$DemandLevel)


#now lets move to the categorical variables vs. demand level

#class
table(best_home$DemandLevel, best_home$Class)
#Appliances = 0/750 = 0
#Bedding and bath = 0.92
#Decor = 0
#Furniture = 0
#Rugs = 0.012

#assembly status
table(best_home$DemandLevel, best_home$Assembly_status)

#material type
table(best_home$DemandLevel, best_home$Material_type)


#Part 2: Modeling#

#Logistic regression 1
logit_reg1 <- glm(formula = DemandLevel ~ Selling_price + WeeklyDiscount
    + Required_capacity , data = train_set, family = "binomial")
summary(logit_reg1)


#Logistic regression 2
logit_reg2 <- glm(formula = DemandLevel ~ Material_type + WeeklyDiscount
    + Selling_price , data = train_set, family = "binomial")
summary(logit_reg2)


#First Decision Tree#
reg_tree <- rpart(formula = DemandLevel ~  WeeklyDiscount + Selling_price 
    + Material_type , train_set, method="anova")
prp(reg_tree)

#Second Decision Tree#
reg_tree2 <- rpart(formula = DemandLevel ~ Class + Assembly_status + Material_type +
                     Required_capacity + WeeklyDiscount + Purchasing_cost , train_set, method="anova", minsplit = 150)
prp(reg_tree2)

#Logistic regression 1 prediction performance
pred_logit_reg1 <- predict(object = logit_reg1, newdata = valid_set, 
                           type = "response")

#Logistic regression 2 prediction performance
pred_logit_reg2 <- predict(object = logit_reg2, newdata = valid_set, 
                           type = "response")

#Part 3: Choosing the Best Model#

