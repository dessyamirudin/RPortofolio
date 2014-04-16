#embarked
summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'