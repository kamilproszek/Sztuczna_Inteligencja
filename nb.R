library(naivebayes)
dane <- wine
dane$V1 = factor(dane$V1)
q <- c()

for(x in 1:10)
{
  idx = sample(2,178,replace = T,c(0.8,0.2))
  train = dane[idx == 1,]
  test = dane[idx == 2,]
  model = naive_bayes(V1~., data = train, usekernel = T)
  p = predict(model, test)
  cm = table(p, test$V1)
  print(cm)
  z = sum(diag(cm))/sum(cm)
  q <- append(q, z)
}

sd(q) #odchylenie standardowe
mean(q) #jakosc klasyfikatora

