EMV_theta = function(ech){
  
  L_log= function(theta){
    return(-sum(log(dexp(ech,rate=theta))))
  }
  res=optim(par = 0.1, fn = L_log, lower = 1e-8, method = "L-BFGS-B")$par
  return(res)  
}


X=rexp(1e3, rate=2)
EMV_theta(X)
1/mean(X)

N=1000
n=100
lambda.chap=rep(0,N)

for (i in 1:N){
  X=rexp(n, rate=2)
  lambda.chap[i]=EMV_theta(X)
}

mean(lambda.chap)
var(lambda.chap)
hist(scale(lambda.chap),prob=T)
curve(dnorm(x),col='red',lwd=2,add=TRUE)







#Exo 2
require(EnvStats)


#1

theta_1=function(ech){
  return(sum(ech)/(sum(ech)-1))
}

theta_2=function(ech){
  M_n=median(ech)
  return(log(2)/log(M_n))
}

theta_3=theta_2=function(ech){
  return(length(ech)/sum(log(ech)))
}


# Paramètres de la loi de Pareto
alpha = 2

# Générer un échantillon de 1000 valeurs suivant une loi de Pareto
theta_1(ech)
theta_2(ech)
theta_3(ech)

N=1000
n=100
lambda.chap=rep(0,N)

for (i in 1:N){
  X=rpareto(n,1,alpha)
  lambda.chap[i]=theta_1(X)
}
print(lambda.chap)
mean(lambda.chap)
var(lambda.chap)
hist(scale(lambda.chap),prob=T)
curve(dnorm(x),col='red',lwd=2,add=TRUE)















