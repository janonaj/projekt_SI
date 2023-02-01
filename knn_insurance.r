  # Wczytanie biblioteki knn
  library(class)
  
  # Wczytywanie danych z pliku
  dane = read.csv("C://Users//janut//Desktop//SI//insurance.csv", header = TRUE, sep = ",")
  dane=dane[,-6] #usuwa region
  
  
  # Stwórz mapowanie wartości na liczby
  value_mapping = c("male" = 1, "female" = 2)
  
  # Zastąp dane charakteru numerycznymi danymi
  dane$sex = value_mapping[as.character(dane$sex)]
  
  
  dane$smoker = as.factor(dane$smoker)
  
  # Funkcja do wyliczenia jakości klasyfikacji
  jakosc_klasyfikacji = function(training, test, k) {
    print(training[, -5])
    model = knn(training[, -5], test[, -5], training[, 5], k = k)
    confusion_matrix = table(test[, 5], model)
    jakosc = sum(diag(confusion_matrix)) / sum(confusion_matrix)
    print(confusion_matrix)
    #print(jakosc)
    return(jakosc)
  }
  
  # Powtórzenie obliczeń kilkukrotnie
  jakosc_klasyfikacji_xn <- replicate(100, {
    idx=sample(2,nrow(dane),replace=T,prob=c(0.8,0.2))
    training_data=dane[idx==1,]
    test_data=dane[idx==2,]
    jakosc_klasyfikacji(training_data, test_data, k = 8)
  })
  
  
  # Policzenie odchylenia standardowej
  sr_dokladnosc = mean(jakosc_klasyfikacji_xn)
  odchylenie = sd(jakosc_klasyfikacji_xn)
  
  # Wyświetl wynik
  cat("Jakosc klasyfikatora:", sr_dokladnosc, "\n")
  cat("Odchylenie standardowe:", odchylenie, "\n")
  
