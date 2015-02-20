# Import training and testing datasets
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Inspect training set
str(train)

# What is the survival rate among passengers?
prop.table(table(train$Survived))
# Roughly 38% of passengers survived

# As 62% of the passengers died, assume everyone in the test set died as well
test$Survived = rep(0, nrow(test))

# Extract PassengerId and Survived for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# ----

# Now train a model based on gender and age ("women and children first")
summary(train$Sex)

# What proportion of males and females survived?
prop.table(table(train$Sex, train$Survived),1)
# 74% of females survived, compared to 19% of males

# So now assume only females survived
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1

# Extract PassengerId and Survived for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "malesallperish.csv", row.names = FALSE)

# ----

# Leverage the Age variable as well
summary(train$Age)
hist(train$Age)

# Create new variable to indicate whether the passenger is below 18
train$Child <- 0
train$Child[train$Age < 18] <- 1

# Find the number of survivors for the different subsets
aggregate(Survived ~ Child + Sex, data=train, FUN = function(x) { sum(x)/length(x) })
# Most females survive, whether they are children or not, and most males died as
# well. So predictions are unlikely to change.

# Let’s take a look at a couple of other potentially interesting variables: the
# class that they were riding in, and what they paid for their ticket.

# First, bin the fares into 4 categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
# And inspect the data
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) { sum(x)/length(x) })
# While the majority of males, regardless of class or fare still don’t do so
# well, we notice that most of the class 3 women who paid more than $20 for
# their ticket actually also miss out on a lifeboat.

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Extract PassengerId and Survived for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender-class.csv", row.names = FALSE)

# ----

# Decision Trees
library(rpart)

# Train and inspect a ‘Recursive Partitioning and Regression Trees’ model that
# uses the CART decision tree algorithm
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data = train, method = "class")
plot(fit)
text(fit)

# Plot a nicer representation with fancyRpartPlot
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

# Submit decision tree predictions
pred <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submit, file = "decisiontree.csv", row.names = FALSE)
