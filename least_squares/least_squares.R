library("estimatr")
library("modelsummary")
library("stargazer")
library("tidyverse")
library("MASS")
library("gmm")

N=1000
df <- tibble(x_star=rnorm(N),
             z=rnorm(N),
             u=rnorm(N),
             v1=rnorm(N),
             v2=rnorm(N),
             y=1+x_star+z+u,
             x1=x_star+v1,
             x2=x_star+v2)

df

model <- lm(y~1+x1+z, data=df) 
stargazer(model, type="text")

model <- lm_robust(y~1+x1+z, data=df)
msummary(model, 'markdown')

#(c)
rho <- 0.8
mu  <- c(0,0)
sigma <- matrix(c(1, rho, rho , 1),nrow=2 ,ncol=2)

multi <- mvrnorm(N, mu, sigma)
cor(multi[,1],multi[,2])

df <- df %>%
  mutate(x_star_p=multi[,1],
         x1_p = x_star_p + v1,
         x2_p = x_star_p + v2,
         z_p=multi[,2],
         y_p = 1+x_star_p+z_p+u)
df

model2 = lm_robust(y_p~1+x1_p+z_p+u, df)
summary(model2, 'markdown')

#(f) iv
iv <- iv_robust(y~1+x1+z | 1+x2+z, data=df)
msummary(iv, 'markdown')


# gmm
iv_matrix= as.matrix(cbind(rep(1,N),df$x2,df$z))
gmm <- gmm(y~1+x1+z, x=iv_matrix, data=df)
msummary(gmm, 'markdown')

# (f)
iv <- as.matrix(cbind(rep(1,N), df$z, df$x2))
iv
res <- gmm(y~1+x1+z, x=iv, data=df)
msummary(res, 'markdown')

estimate_IV <- function(n){
  df <- tibble(id=1:n,
               xstar=rnorm(n),
               z=rnorm(n),
               v1=rnorm(n),
               v2=rnorm(n),
               u=rnorm(n),
               x1=xstar+v1,
               x2=xstar+v2,
               y=1+xstar+z+u)
  iv <- iv_robust(y~1+x1+z|1+x2+z, data=df)
  est <- summary(iv)$coefficients[,1]
  return(est)
}
x1est_vector <- rep(0, (1000/5)-1)
zest_vector <- rep(0, (1000/5)-1)
for (i in seq(5, 1000, 5)) {
  x1est_vector[i/5] <- estimate_IV(i)[2]
  zest_vector[i/5] <- estimate_IV(i)[3]
}

est <- tibble(n=seq(5,1000, 5), x1est=x1est_vector, zest=zest_vector)

# x1
g <- ggplot(data=est, aes(x=n, y=x1est))+geom_point()
g <- g + geom_hline(yintercept=1, color='red')
print(g)

# z
g <- ggplot(data=est, aes(x=n, y=zest))+geom_point()
g <- g + geom_hline(yintercept=1, color='red')
print(g)

# asymtotic normality
trial <- c(20, 50,200, 1000)
sim <- 1000
est <- list()
for(k in 1:length(trial)) {
  x1est_vactor <- rep(0, sim)
  zest_vectr <- rep(0, sim)
  for (i in 1:sim){
    x1est_vector[i] <- estimate_IV(trial[k])[2]
    zest_vector[i] <- estimate_IV(trial[k])[3]
  }
  temp = tibble(x1est=x1est_vector, zest=zest_vector)
  est[[k]] <- temp
}


# x1 with n=20
g <- ggplot(data=est[[1]], aes(x=x1est))+geom_histogram()
print(g)
# x1 with n=50
g <- ggplot(data=est[[2]], aes(x=x1est))+geom_histogram()
print(g)
# x1 with n=200
g <- ggplot(data=est[[3]], aes(x=x1est))+geom_histogram()
print(g)
# x1 with n=1000
g <- ggplot(data=est[[4]], aes(x=x1est))+geom_histogram()
print(g)

# (h) 2SLS
first = lm_robust(x1~1+x2+z, data=df)
x1_pred = first$fitted.values
df<- df %>%
  mutate(x1pred=x1_pred)
second = lm_robust(y~1+x1pred+z, data=df)
res_list = list()
res_list[['2SLS']] <- second
res_list[['GMM']] <- gmm
msummary(res_list)
         
