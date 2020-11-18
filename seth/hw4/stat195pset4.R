
fun = function(x) { 
  return(x)
}
simulator = function(fun) { 

  # Function
  # set.seed(195)
  # Training Constants
  N_train = 10
  epsilon_train = rnorm(10)
  X_train = rnorm(10)
  Y_train = fun(X_train) + epsilon_train
  
  # Draw Test data
  N_test = 10
  X_test = rnorm(10, mean=2, sd=1)
  epsilon_test = rnorm(10)
  Y_test = fun(X_test) + epsilon_test
  
  
  # OLS 
  lm(Y_train~X_train - 1)
  beta_OLS = (t(X_train) %*% Y_train) / (t(X_train)%*%X_train)
  OLS_pred = X_test%*%beta_OLS
  
  # WLS
  W = diag(N_train)
  for(i in 1:N_train){
    W[i,i] <- exp(2*X_train[i] - 2)
  }
  beta_WLS = (t(X_train) %*%W %*% Y_train) / (t(X_train) %*%W %*% X_train)
  WLS_pred = X_test %*% beta_WLS
  
  # print("OLS_pred: ")
  ols_mse = mean((OLS_pred - Y_test)^2)
  
  # print("WLS_pred: ")
  wls_mse = mean((WLS_pred - Y_test)^2)
  
  return(list(ols =ols_mse, wls= wls_mse))
}

Nsims = 1000
ols = c()
wls = c()
for (sim in 1:Nsims){
  results = simulator(fun)
  ols = c(ols, results[[1]])
  wls = c(wls, results[[2]])
}


mean(ols) + 2* sd(ols)
mean(wls) - 2* sd(ols)
