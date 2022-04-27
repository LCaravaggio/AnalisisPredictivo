# Load Libraries
library(randomForest)
library(datasets)
library(caret)

# Data
data<-iris
str(data)
data$Species <- as.factor(data$Species)
table(data$Species)

# Partición en Train y Test
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
rf <- randomForest(Species~., data=train, proximity=TRUE)
print(rf)

# Train
p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)


# Test
p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)

# error rate
plot(rf)
