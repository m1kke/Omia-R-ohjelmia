# Titanic-haaste

install.packages("treemap")

# Luodaan pari muuttujaa
x <- 5
y <- "asdf"


# Set work directory
setwd("C:/Users/mikkok/Desktop/titanic/data/") # asdfdsafsaf


# Ladataan datasetit
trainData <- read.csv("train.csv", header = T, stringsAsFactors = F)
testData <- read.csv("test.csv", header = T, stringsAsFactors = F)


# Perus EDA
head(trainData)
plot(density(trainData$Age, na.rm = T))
plot(density(trainData$Fare, na.rm = T))

counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "Survived and deceased between male and female")

Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People", main = "survived and deceased between male and female", legend = rownames(Pclass_survival), beside = T)


hist(trainData$Age, col="red", main = "Histogrammi iästä", xlab = "Ikä", ylab = "Frekvenssi", breaks = 20)

round(aggregate(Survived ~ Pclass, data = trainData, FUN = function(x) {sum(x)/length(x)}), digits = 2)

trainData$Uusicolumn <- 1


# Save results
setwd("C:/Users/mikkok/Desktop/titanic/results")
