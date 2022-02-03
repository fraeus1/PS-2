library("MASS")
library("ivreg")


#Construct a function to generate correlated variables:

Independent_var_function<-function(n, a, b, c, varnames){
  set.seed(7)
  Data = as.data.frame(mvrnorm(n, mu = c(0, 0, 0), Sigma = matrix(c(1, a, b, a, 1, c, b, c, 1), nrow = 3)))
  colnames(Data) = varnames
  return(Data)
}

#Create X, Z and E
Variables<-Independent_var_function(10000, 0.5, 0.4, 0, c("X", "Z", "E"))

#Create Y
Variables$Y<-Variables$X+Variables$E

#Create plot for cor(x,z)=.5

Beta_function<-function(N, data){
  beta<-as.data.frame(matrix(data = NA, nrow = 10000, ncol = 2))
  colnames(beta)<-c("OLS", "IV")
  for (i in 1:10000) {
    Subset_data<-data[sample(1:10000, N, replace=T),]
    RegOLS<-lm(Y~X, data = Subset_data)
    RegIV<-ivreg(Y~X|Z, data = Subset_data)
    beta[i,1] = RegOLS$coefficients[2]
    beta[i,2] = RegIV$coefficients[2]
  }
  return(beta)
}

beta1<-Beta_function(50, Variables)
hist(beta1$IV, freq = F, breaks = 20, col="blue", main = "N=50, cor(X,Z)=0.5", xlab = "Estimate distributions", ylim=c(0,4), xlim=c(0,2))
hist(beta1$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topright', c('IV', 'OLS'), fill=c('blue', 'red'))


beta2<-Beta_function(100, Variables)
hist(beta2$IV, freq = F, breaks = 10, col="blue", main = "N=100, cor(X,Z)=0.5", xlab = "Estimate distributions", ylim=c(0,5), xlim=c(0.3,1.8))
hist(beta2$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topright', c('IV', 'OLS'), fill=c('blue', 'red'))

beta3<-Beta_function(250, Variables)
hist(beta3$IV, freq = F, breaks = 10, col="blue", main = "N=250, cor(X,Z)=0.5", xlab = "Estimate distributions", ylim=c(0,6), xlim=c(0.5,1.8))
hist(beta3$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topleft', c('IV', 'OLS'), fill=c('blue', 'red'))

beta4<-Beta_function(1000, Variables)
hist(beta4$IV, freq = F, breaks = 10, col="blue", main = "N=1000, cor(X,Z)=0.5", xlab = "Estimate distributions", ylim=c(0,15), xlim=c(0.7,1.8))
hist(beta4$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topright', c('IV', 'OLS'), fill=c('blue', 'red'))

#cor(x,z)=.15

Variables1 = Independent_var_function(10000, 0.15, 0.4, 0, c("X", "Z", "E"))
Variables1$Y = Variables1$X+Variables1$E

beta1a<-Beta_function(50, Variables1)
hist(beta1a$IV, freq = F, breaks = 20, col="blue", main = "N=50, cor(X,Z)=0.15", xlab = "Estimate distributions", ylim=c(0,1))
hist(beta1a$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('top', c('IV', 'OLS'), fill=c('blue', 'red'))

beta2a<-Beta_function(100, Variables1)
hist(beta2a$IV, freq = F, breaks = 10, col="blue", main = "N=100, cor(X,Z)=0.15", xlab = "Estimate distributions", ylim=c(0,1))
hist(beta2a$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topright', c('IV', 'OLS'), fill=c('blue', 'red'))

beta3a<-Beta_function(250, Variables1)
hist(beta3a$IV, freq = F, breaks = 10, col="blue", main = "N=250, cor(X,Z)=0.15", xlab = "Estimate distributions", ylim=c(0,7))
hist(beta3a$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('top', c('IV', 'OLS'), fill=c('blue', 'red'))

beta4a<-Beta_function(1000, Variables1)
hist(beta4a$IV, freq = F, breaks = 10, col="blue", main = "N=1000, cor(X,Z)=0.15", xlab = "Estimate distributions", ylim=c(0,13), xlim=c(0.5,1.8))
hist(beta4a$OLS, freq = F, breaks = 10, col="red", add=TRUE)
legend('topleft', c('IV', 'OLS'), fill=c('blue', 'red'))
