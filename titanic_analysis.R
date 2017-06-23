setwd("~/Desktop/Titanic")

trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors=FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors=FALSE)
testData$Survived <- 0

combineData <- rbind(trainData, testData)

#convert Sex, Cabin and Embark into Factor
combineData$Sex <- factor(combineData$Sex) 
combineData$Cabin <- factor(combineData$Cabin)
combineData$Embarked <- factor(combineData$Embarked) 

# Extract the title from the names. The title is useful to determine age and survival possibility
#X <- strsplit(combineData$Name[1], split='[,.]')
#X <- strsplit(combineData$Name[1], split='[,.]')[[1]]
#X <- strsplit(combineData$Name[1], split='[,.]')[[1]][2]
X <- sapply(combineData$Name, FUN = function(x) {strsplit (x, split='[,.]')[[1]][2]})

#remove the white spaces
X <- gsub(" ", "", X, fixed = TRUE)

# Assign to new column in combined data set
combineData$Title <- X


#Standardise the Title to Mr, Mrs and Miss
combineData$Title[combineData$Title %in% c('Lady', 'the Countess', 'Mlle', 'Mee', 'Ms')] <- 'Miss'
combineData$Title[combineData$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
combineData$Title[combineData$Title %in% c('Dona')] <- 'Mrs'
combineData$Title[combineData$PassengerId == 797] <- 'Mrs' # female doctor

combineData$Title <- factor(combineData$Title)

# Passenger on row 62 and 830 do not have an embarkment status
# Infer from Cabin Class segmented by Embarkation - C, S and Q
# X <- subset(combineData , Pclass==1 & Embarked == 'C') 
# median(x$Fare)   #52
# Y <- subset(combineData , Pclass==1 & Embarked == 'S')
# median(Y$Fare)   #76.7
# Z <- subset(combineData , Pclass==1 & Embarked == 'Q')
# median(Z$Fare)   #90

# Turns out that passenger embarked from 'C' in class 1 has median fare closest to 80
combineData$Embarked[c(62,830)] = "C"

# Passenger #1044 does not have fare registered -->combineData[is.na(combineData$Fare),]
# Passenger is in 3rd Class Cabin and embarked from 'S'
X <- subset(combineData, Pclass == 3 & Embarked == 'S')
# Set the fare to the median value at 8.05
combineData$Fare[1044] <- median(X$Fare, na.rm = TRUE)

# 263 passengers do not have age registered
# summary(combineData$Age)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                       data = combineData[!is.na(combineData$Age),], method="anova")

combineData$Age[is.na(combineData$Age)] <- predict(predicted_age, combineData[is.na(combineData$Age),])
combineData$Age <- round(combineData$Age, digits = 0)

### CREATING MODEL ###
new_trainData <- combineData[1:891,]
new_testData <- combineData[892:1309,]
new_testData$Survived <- NULL

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
             data=new_trainData, method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, new_testData, type = "class")
submit <- data.frame(PassengerId = new_testData$PassengerId, Survived = Prediction)
write.csv(submit, file = "my_solution.csv", row.names = FALSE)
