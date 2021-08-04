library("estimatr")
library("modelsummary")
library("tidyverse")
library("MASS")
library("quantreg")
library("stargazer")
library("censReg") 
library("sampleSelection") 

N=1000

df = tibble(id =1:N,
            u=rnorm(N),
            alpha=rnorm(N),
            nu1=rnorm(N),
            nu2=rnorm(N),
            x1=alpha+nu1,
            x2=alpha+nu2,
            y=1+x1+x2+u)
df

qt = rq(y~1+x1+x2,tau = 0.5, data=df) 
stargazer(qt, type="text")

#c
vec<-c(0.1,0.25,0.5, 0.75, 0.9)
res3d <- matrix(0,ncol=length(vec),nrow = 3)
for(i in 1:length(vec)){
    res3d[,i] = coef(rq(y~1+x1+x2,tau = vec[i], data=df))
}
res3d

# 4 likelhiood estimation
data_generation4 <- function(N,sigma){
  df4 = tibble(id =1:N,
               u=rnorm(N, 0, sigma),
               alpha=rnorm(N),
               nu1=rnorm(N),
               nu2=rnorm(N),
               x1=alpha+nu1,
               x2=alpha+nu2,
               y=if_else(1+x1+x2+u>=0, 1, 0))
  return(df4)
}

df4 <- data_generation4(N, 1)
df4

probit = glm(y~1+x1+x2, family=binomial("probit"), data=df4)
stargazer(probit, type="text")

df4 <- data_generation4(N, 2)
probit = glm(y~1+x1+x2, family=binomial("probit"), data=df4)
stargazer(probit, type="text")


# 4 likelhiood estimation
data_generation5 <- function(N,sigma){
  df5 = tibble(id =1:N,
               u=rnorm(N, 0, sigma),
               alpha=rnorm(N),
               nu1=rnorm(N),
               nu2=rnorm(N),
               x1=alpha+nu1,
               x2=alpha+nu2,
               ystar=1+x1+x2+u,
               y=if_else(ystar>=0, ystar, 0))
  return(df5)
}
df5 =data_generation5(N, 1)
df5

cens = censReg(y~1+x1+x2, left=0, data=df5)
stargazer(cens, type="text")

# sigma2
df5 =data_generation5(N, 2)

cens = censReg(y~1+x1+x2, left=0, data=df5)
stargazer(cens, type="text")


# 6 sample selection
data_generation6 <- function(N,mv){
  df6 = tibble(id =1:N,
               u=mv[,1],
               v=mv[,2],
               alpha=rnorm(N),
               nu1=rnorm(N),
               nu2=rnorm(N),
               z=rnorm(N),
               x1=alpha+nu1,
               x2=alpha+nu2,
               ystar=1+x1+x2+u,
               sel = if_else(1+x1+z+v>=0,1,0),
               y=if_else(1+x1+z+v>=0, ystar, 0))
  return(df6)
}
sigma=matrix(c(1, 0.5,0.5,1), ncol=2, nrow=2)
mv = mvrnorm(N, c(0,0), sigma)
df6 =data_generation6(N,mv)
df6

sample_selection <- selection(selection = sel ~ 1 + x1 + z,
                    outcome = y ~ 1 + x1 + x2, data = df6)
stargazer(sample_selection, type="text")

sigmau=2
comvat=matrix(c(sigmau^2, 0.5*sigmau,0.5*sigmau,1), ncol=2, nrow=2)
mv = mvrnorm(N, c(0,0), comvat)
df6 =data_generation6(N,mv)
sample2 <- selection(selection=sel~1+x1+z,outcome=y~1+x1+x2, data=df6)
stargazer(sample2, type="text")

# 10 Heckit
data_generation10 <- function(N,mv){
  df10 = tibble(id =1:N,
               u=mv[,1],
               v=mv[,2],
               z=rnorm(N),
               x=rnorm(N),
               ystar=1+x+u,
               first=1+z+v,
               d=if_else(first>=0, 1, 0),
               y=if_else(first>=0, ystar, 0))
  return(df10)
}
sigma=matrix(c(0.25, 0.5,0.5,4), ncol=2, nrow=2)
mv = mvrnorm(N, c(0,0), sigma)
df10 =data_generation10(N,mv)
df10
first = glm(d~1+z, family="probit", data=df10)
