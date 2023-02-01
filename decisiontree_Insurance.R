library(caret)
library(rpart)

#wczytywanie danych
data = read.csv("C://Users//janut//Desktop//SI//insurance.csv", header = TRUE, sep = ",")

#konwersja kolumny "smoker" na faktory
data$smoker = as.factor(data$smoker)

#powtorzenie procesu trzykrotnie
accuracies = c()
for(i in 1:3){
  #podział danych na zbiór treningowy i testowy
  trainIndex = sample(1:nrow(data), 0.7 * nrow(data))
  trainData = data[trainIndex, ]
  testData = data[-trainIndex, ]
  
  #wytrenowanie modelu drzewa decyzyjnego
  library(rpart)
  fit = rpart(smoker ~., data = trainData, method = "class")
  
  #ocena jakości modelu
  pred = predict(fit, newdata = testData, type = "class")
  confMatrix = confusionMatrix(pred,testData$smoker)
  
  #zapisanie dokładności
  accuracies[i] = confMatrix$overall[1]
}

#obliczanie odchylenia standardowego i średniej dokładności
meanAccuracy = mean(accuracies)
sdAccuracy = sd(accuracies)

print(paste("Średnia dokładność", meanAccuracy))
print(paste("Odchylenie standardowe", sdAccuracy))
