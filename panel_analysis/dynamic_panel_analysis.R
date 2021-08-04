library("estimatr")
library("modelsummary")
library("tidyverse")
library("MASS")
library("gmm")
library("plm")

t=20
N=1000
id=1:N
Time=1:t

data_generation1 <- function(n,t,coef,problem){
  id <- 1:n
  time <- 0:t      # t = (0), 1,,...,20
  df <- expand_grid(id,time) %>% 
    mutate(u1 = rnorm(n*(t+1)),
           u2 = rnorm(n*(t+1)),
           nu = rnorm(n*(t+1)),
           epsilon = rnorm(n*(t+1))) %>%
    group_by(id) %>%
    mutate(alpha = rnorm(1),
           z = rnorm(1),
           y = if_else(time == 0, rnorm(1), 0)) %>%
    ungroup %>%
    mutate(x1 = alpha + u1,
           x2 = alpha + u2) 
  
  # create variable y
  # If you know more efficient way, please let me know.
  df <- df %>%
    group_by(id)
  for(k in 1:t){
    df <- df %>%
      mutate(y = if_else(time == k,
                         1 + coef*dplyr::lag(y, n = 1) + x1 + x2 + z + alpha + nu,
                         y)) %>%
      ungroup
  }

  df <- df %>%
    mutate(ylag = dplyr::lag(y, n = 1)) %>%
    ungroup
  
  if (problem=="i") {
    for(k in 1:t){
      df <- df %>%
        group_by(id)%>%
        mutate(nu = if_else(time == k,
                0.5*dplyr::lag(nu, n = 1) + epsilon,nu))
    }
  }
  if (problem =="e" || problem =="h" || problem =="i") {
    df <- df %>%
      group_by(id) %>%
      mutate(x1diff=x1-dplyr::lag(x1, n=1),
             x2diff=x2-dplyr::lag(x2, n=1),
             x1diff2=dplyr::lag(x1, n=1)-dplyr::lag(x1, n=2),
             x2diff2=dplyr::lag(x1, n=1)-dplyr::lag(x2, n=2),
             ylag2=dplyr::lag(y,n=2),
             ydiff=y-ylag,
             ydiff2=ylag-dplyr::lag(y,n=2))%>%
      ungroup
  }
  
  return(df)
}

df1 <- data_generation1(N, 2, 0.6, "a")

temp <- df1 %>%
  filter(time == 1 | time == 2) # data for 2 period
temp <- pdata.frame(temp,index = c("id","time"))
FE2period <- plm(y ~ ylag + x1 + x2, data = temp, 
                 model = "within", effect = "individual")
msummary(FE2period, 'markdown')

#(e)
df1 <- df1 %>%
  group_by(id) %>%
  mutate(x1diff=x1-dplyr::lag(x1, n=1),
         x2diff=x2-dplyr::lag(x2, n=1),
         x1diff2=dplyr::lag(x1, n=1)-dplyr::lag(x1, n=2),
         x2diff2=dplyr::lag(x1, n=1)-dplyr::lag(x2, n=2),
         ylag2=dplyr::lag(y,n=2),
         ydiff=y-ylag,
         ydiff2=ylag-dplyr::lag(y,n=2))%>%
  ungroup %>%
  filter(time == 1 | time == 2)
df1

list_fg = list()
list_fg[["iv1"]] = iv_robust(ydiff~-1+ydiff2+x1diff+x2diff | -1+ylag2+x1diff+x2diff,
                data=df1)
msummary(list_fg, 'markdown')

#g
list_fg[["iv2"]] = iv_robust(ydiff~-1+ydiff2+x1diff+x2diff |
                               -1+x1diff2+x2diff2+x1diff+x2diff,data=df1)
msummary(list_fg, 'markdown')

#h
df_h <- data_generation1(N, 2, 0.9, "h")
df_h <- df_h %>%
  group_by(id) %>%
  mutate(x1diff=x1-dplyr::lag(x1, n=1),
         x2diff=x2-dplyr::lag(x2, n=1),
         x1diff2=dplyr::lag(x1, n=1)-dplyr::lag(x1, n=2),
         x2diff2=dplyr::lag(x1, n=1)-dplyr::lag(x2, n=2),
         ylag2=dplyr::lag(y,n=2),
         ydiff=y-ylag,
         ydiff2=ylag-dplyr::lag(y,n=2))%>%
  ungroup %>%
  filter(time == 1 | time == 2)
list_fg[["ivh"]] <- iv_robust(ydiff~-1+ydiff2+x1diff+x2diff | -1+ylag2+x1diff+x2diff,
                             data=df_h)
list_fg[["ivht"]] <- iv_robust(ydiff~-1+ydiff2+x1diff+x2diff |
                                 -1+x1diff2+x2diff2+x1diff+x2diff,data=df_h)
msummary(list_fg, 'markdown')

#i
df_i <- data_generation1(N, 2, 0.6, "i")

list_i =list()
list_i[["iv1"]] = list_fg[["iv1"]]
list_i[["iv2"]] = list_fg[["iv2"]]
list_i[["ivi1"]] <- iv_robust(ydiff~-1+ydiff2+x1diff+x2diff | -1+ylag2+x1diff+x2diff,
                              data=df_i)
list_i[["ivi2"]] <- iv_robust(ydiff~-1+ydiff2+x1diff+x2diff |
                                 -1+x1diff2+x2diff2+x1diff+x2diff,data=df_i)
msummary(list_fg, 'markdown')
