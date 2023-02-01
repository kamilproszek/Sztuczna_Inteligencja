library(party)
library(randomForest)
data = wine
data$V1 = factor(data$V1)
q = c()
for (x in 1:10)
{
  idx = sample(1:nrow(data), 0.4*nrow(data))
  test = data[idx,]
  train = data[-idx,]
  model = randomForest(V1 ~ ., data = train)
  p=predict(model, test)
  cm = table(p, test$V1)
  quality = sum(diag(cm))/sum(cm)
  q = append(q, quality)
}
cm
mean(q) #jakość klasyfikatora
sd(q) #odchylenie standardowe