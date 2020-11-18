# Problem 3

library(Rfast)
M = 100
N = c(10,100, 1000, 10000)
N_test = 0.2

simulator = function(M, N){
  cvs = c()
  for (j in 1:M){ 
    
    normals <- rnorm(N, 0, 1)
    
    mses = c()
    for (i in 1:N) {
      index = which.max(abs(normals[-i]))
      thing = max(0, normals[-i][index])
      mse = mean((thing - normals[i])^2)
      mses = c(mses, mse)
    }
    #hist(mses)
    meanmse = mean(mses)
    
    cvs = c(cvs, meanmse)
  }
  return(cvs)
}

mses = simulator(10000, 2000)
mean(mses)
hist(mses, freq = F, breaks = "fd", main='3a')

sd(mses) 


mses = simulator(100, 5000)
mean(mses)
hist(mses, freq = F, breaks = "fd", main='3a')
sd(mses)
min(mses)
N = 10000

10000 

predictions = max(normals)



mean(mses > 3)
# 4 
m = 10 
N = 100 
N_test = 10 
p_norm = 0.3 
mu = 10
sigma = 1

norms = rnorm(N + N_test, 10, 1)

cauchys = rcauchy(N + N_test)
Xtrain = 





