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
