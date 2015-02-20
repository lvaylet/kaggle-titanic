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
