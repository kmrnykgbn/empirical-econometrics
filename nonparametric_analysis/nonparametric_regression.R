library("estimatr")
library("modelsummary")
library("tidyverse")
library("np")
library("lmtest")
library("sandwich")
library("ggplot2")


N<- 1000

df <- tibble(X=runif(N,0,1),
             eps=rnorm(N,0,1),
             Y=1+X+exp(-X)+eps)
df

createKernel <- function(X,x,h){
  s <- (X-x)/h
  bin <- if_else(abs(s)<=1, 1, 0)
  return(bin)
}

df <- df %>%
  mutate(ker=createKernel(X, 0.5, 0.1))

# kernel_regression_estimator
kerreg<-lm(Y~1, weights = ker, data=df)
tidy(kerreg)
coeftest(kerreg, vcov = vcovHC(kerreg, type = "HC0"))

# npreg
npregres <- npreg(regtype="lc", txdat = df$X, tydat=df$Y,
                  exdat=0.5)
fit <- fitted(npregres)
se(npregres)


# local linerar regression
npregresl <- npreg(regtype="ll", txdat = df$X, tydat=df$Y,
                  exdat=0.5)
fit <- fitted(npregresl)
se(npregresl)

repnpreg <- function(data,bws){
  res <- npreg(txdat=df$X,tydat=df$Y,bws=bws,reg_type="lc")
  return(res)
}
res_mt <- matrix(c(seq(0,1,length=1000),
                 repnpreg(df, 0.1)$mean,
                 repnpreg(df, 0.5)$mean,
                 repnpreg(df, 1)$mean, df$Y), ncol = 5)
res_mt

#visualization
graph<-ggplot(as.data.frame(res_mt), aes(x=df$X)) + 
  geom_line(aes(y = V2, colour = "0.1")) +
  geom_line(aes(y = V3, colour = "0.5")) +
  geom_line(aes(y = V4, colour = "1")) +
  geom_point(alpha=1/8,aes(y = V5, colour = "actual"))
graph<-graph+
  scale_colour_manual("h",values=c("0.1"="red","0.5"="blue",
                                   "1"="green","actual"="black"))
graph<-graph + stat_function(fun=function(X) df$X+exp(-df$X)+1, colour = "black")
graph<-graph+
  scale_colour_manual("h",values=c("0.1"="red","0.5"="blue",
                                   "1"="green","actual"="black"))
graph<-graph+labs(x="X",y="Y")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))
graph

# Expanded figure
graph2=graph+ylim(1.9,2.4)
graph2

