library(tidyverse)

N = 100
Sparsity = 10

set.seed(195)
X = runif(N, 0, Sparsity* pi)
Y_point = sin(X) + rnorm(N, 0,0.5)
Y_class = (Y_point > sin(X)) * 1


df = data.frame(X = X, Y_point = Y_point, Y_class=as.factor(Y_class))

fun.1 = function(x) sin(x)

p = ggplot(df, aes(x=X, y=Y_point, color = Y_class)) + 
  geom_point() + 
  stat_function(fun = fun.1, aes(x=seq(0,Sparsity*pi, length.out=N)), color="black") + 
  xlim(0,Sparsity*pi)
p  



# Train-Test Split 
set.seed(123)
dat.d <- sample(1:nrow(df),size=nrow(df)*0.8,replace = FALSE) #random selection of 80% data.


df_train <- df[dat.d,]$X
Y_train <- df[dat.d,]$Y_class
df_test <- df[-dat.d,]$X
Y_test <- df[-dat.d,]$Y_class

library(class)       
test_pred_1nn <- knn(train = data.frame(X=df_train), 
                     test = data.frame(X=df_test),
                     cl = Y_train, 
                     k=1)
test_pred_2nn <- knn(train = data.frame(X=df_train), 
                     test = data.frame(X=df_test),
                     cl = Y_train, 
                     k=2)


ClassificationError1 = mean((as.numeric(Y_test) == as.numeric(test_pred_1nn)))
ClassificationError2 = mean((as.numeric(Y_test) == as.numeric(test_pred_2nn)))

paste("1-NN Classification error:", ClassificationError1 )
paste("2-NN Classification error:", ClassificationError2 )


