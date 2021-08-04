library("estimatr")
library("modelsummary")
library("stargazer")
library("tidyverse")
library("MASS")
library("gmm")
library("plm")

t<-2
N<-1000
id <- 1:N
Time<-1:t

df <- expand_grid(id, Time)
df <- df %>%
  mutate(v=rnorm(N*T),
            u1=rnorm(N*T),
            u2=rnorm(N*T)) %>% 
  group_by(id) %>% 
  mutate(alpha=rnorm(1),
             z=rnorm(1)) %>%
  ungroup %>%
  mutate(x1=alpha+u1, 
         x2=alpha+u2, 
         y=1+x1+x2+z+alpha+v)
df

# OLS
df1 = df[1==df$Time,]

ols = lm_robust(y~1+x1+x2+z, data=df1)
msummary(ols, 'markdown')

# panel_data
df_panel <- pdata.frame(df, index=c("id", "Time"))
df_panel

FEc <- plm(y~1+x1+x2+z, data=df_panel, model="within", effect="individual")
msummary(FEc, 'markdown')

# estimate 1 and z
coef = FEc$coefficients
df_e <- df %>%
  mutate(y_e=y-coef[1]*x1-coef[2]*x2)
olse = lm(y_e~1+z, data=df_e)
msummary(olse, 'markdown')

df <- df %>%
  mutate(x2=alpha+0.1*u2,
         y=1+x1+x2+z+alpha+v)
df_panel2 <- pdata.frame(df, index=c("id","Time"))
FEc2= plm(y~x1+x2+z, data=df_panel2, model = "within"
          , effect="individual")
res_l=list()
res_l[['FEc']]=FEc
res_l[['FEc2']]=FEc2
msummary(res_l, 'markdown')      

# fixed effect est by ols
df <- expand_grid(id, Time)
df <- df %>%
  mutate(v=rnorm(N*T),
         u1=rnorm(N*T),
         u2=rnorm(N*T)) %>% 
  group_by(id) %>% 
  mutate(alpha=rnorm(1),
         z=rnorm(1)) %>%
  ungroup %>%
  mutate(x1=alpha+u1, 
         x2=alpha+u2, 
         y=1+x1+x2+z+alpha+v)

df_panel$diff_y <- df_panel$y-plm::lag(df_panel$y)
df_panel$diff_x1 <- df_panel$x1-plm::lag(df_panel$x1)
df_panel$diff_x2 <- df_panel$x2-plm::lag(df_panel$x2)

res_la = list()
res_la[['OLS']] <- lm_robust(diff_y ~ diff_x1+diff_x2, data=df_panel)
res_la[['FE']] <- plm(y~x1+x2, data=df_panel, model="within", effect = "individual")

df_pa_se<- df_panel
df_pa_se$x2 <- df_pa_se$alpha+df_pa_se$u2 + 
  plm::lag(df_pa_se$v)
df_pa_se <- df_pa_se %>%
  mutate(x2=if_else(is.na(x2),alpha+u2+rnorm(N), x2),
         y=1+x1+x2+z+alpha+v)
df_pa_se$diff_y <- df_pa_se$y-plm::lag(df_pa_se$y)
df_pa_se$diff_x1 <- df_pa_se$x1-plm::lag(df_pa_se$x1)
df_pa_se$diff_x2 <- df_pa_se$x2-plm::lag(df_pa_se$x2)
df_pa_se

res_lc = list()
res_lc[['OLS']] <- lm_robust(diff_y ~ diff_x1+diff_x2, data=df_pa_se)
res_lc[['FE']] <- plm(y~x1+x2, data=df_pa_se, model="within", effect = "individual")

msummary(res_lc, 'markdown')

t<-20
id <- 1:N
Time<-1:t

df_e = expand_grid(id=id, Time=Time)
df_e <- df_e %>%
  mutate(nu=rnorm(N*t),
         u1=rnorm(N*t),
         u2=rnorm(N*t)) %>%
  group_by(id) %>%
  mutate(alpha=rnorm(1),
         z=rnorm(1))%>%
  ungroup %>%
  mutate(x1=alpha+u1,
         x2=alpha+u2+plm::lag(nu),
         x2=if_else(is.na(x2), alpha+u2+rnorm(N*T),x2),
         y = 1+x1+x2+z+alpha+nu)
df_e

dfpa_e = pdata.frame(df_e, index=c("id","Time"))
dfpa_e$diff_y <- dfpa_e$y-plm::lag(dfpa_e$y)
dfpa_e$diff_x1 <- dfpa_e$x1-plm::lag(dfpa_e$x1)
dfpa_e$diff_x2 <- dfpa_e$x2-plm::lag(dfpa_e$x2)

res_lf =list()
res_lf[["fed"]] =res_lc[['FE']]
res_lf[["fef"]] = plm(y~x1+x2, data=dfpa_e, model="within", effect="individual")
msummary(res_lf, 'markdown')
