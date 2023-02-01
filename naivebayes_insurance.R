library(naivebayes)

dane = read.csv("C://Users//janut//Desktop//SI//insurance.csv", header = TRUE, sep = ",")



Dokladnosc = function(){
  
  idx=sample(2,nrow(dane),replace=T,prob=c(0.8,0.2))
  train = dane[idx==1,]
  test = dane[idx==2,]
  
  model=naive_bayes(as.character(smoker) ~ ., data=train, usekernel=T)
  plot(model)
  
  #PREDYKCJA
  p = predict(model, test)
  cm = table(p, test$smoker)

  #print(cm)
  pcf = cm/sum(cm)
  
  #DOKŁADNOŚĆ
  return (sum(diag(cm))/sum(cm))
}
results = replicate(100, Dokladnosc())

#Policzenie odchylenia standardowej
sr_dokladnosc = mean(results)
odchylenie = sd(results)

#Wyświetl wynik
cat("Jakosc klasyfikatora:", sr_dokladnosc, "\n")
cat("Odchylenie standardowe:", odchylenie, "\n")

