library(class)
df <- wine
df$V1 = factor(df$V1)
result = c()
for (x in 1:10)
  
{
  idx = sample(1:nrow(df), as.integer(0.4*nrow(df)))
  train = df[idx,]
  test = df[-idx,]
  trainDf = train[,2:14] 
  testDf = test[,2:14]
  traincl = train[,1]
  testcl = test[,1]
  
  nn3 <- knn(trainDf, testDf, cl=traincl, k=5)
  
  cm3 = table(nn3, testcl)
  ef = sum(diag(cm3))/sum(cm3)
  result <- append(result, sum(diag(cm3))/sum(cm3))
}
cm3
sd(result) #odchylenie standardowe
mean(result) #jakość klasyfikatora
