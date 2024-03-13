# Created by Renata Rojas Guerra (renata.r.guerra@ufsm.br)
# March 2024

library(tidyverse)
library(gamlss)
library(readr)
#data for random effects model
data1 <- read_csv("data_elections_lat_long.csv") %>% 
  select(-NM_Municipio,
         -State,
         -latitude,-longitude,
         -Nordeste,-Norte,-Sul,-Sudeste,-Centro_Oeste,
         -PG_2014_Centro,-PG_2018_Centro
  )
#data for fixed effects model with region
data2 <- read_csv("data_elections_lat_long.csv") %>% 
  select(-NM_Municipio,
         -State,
         -latitude,-longitude,
         -Region,
         -Centro_Oeste,
         -PG_2014_Centro,-PG_2018_Centro
  )
#data for fixed effects with latitude and longitude 
data3 <- read_csv("data_elections_lat_long.csv") %>% 
  select(-NM_Municipio,
         -State,
         -Region,
         -Nordeste,-Norte,-Sul,-Sudeste,
         -Centro_Oeste,
         -PG_2014_Centro,-PG_2018_Centro
  )

########################### 
# fitting beta regression #
###########################
# complete models
beta_RE0<-gamlss(Prop_PSL~EP+PE+LR+MHIC+DD+
                   PG_2014_Esquerda+PG_2014_Direita+
                   PG_2018_Esquerda+PG_2018_Direita+
                   Cap_BR+
                   random(as.factor(Region)),
                 sigma.formula =~EP+PE+LR+MHIC+DD+
                   PG_2014_Esquerda+PG_2014_Direita+
                   PG_2018_Esquerda+PG_2018_Direita+
                   Cap_BR+
                   random(as.factor(Region)),
                 family = BE(mu.link = "logit", sigma.link = "logit"), 
                 method = CG(),
                 control=gamlss.control(c.crit = 0.001, n.cyc = 700, 
                                        mu.step = .1, sigma.step = .1,
                                        trace= F),
                 data=data1
)

beta_FE0<-gamlss(Prop_PSL~.,
                 sigma.formula =~.,
                 family = BE(mu.link = "logit", sigma.link = "logit"),
                 method = CG(),
                 sigma.start = .9,
                 control=gamlss.control(c.crit = 0.001, n.cyc = 700,
                                        mu.step = .1, sigma.step = .1,
                                        trace= F),
                 data=data2
)

beta_lat_long0<-gamlss(Prop_PSL~.,
                       sigma.formula =~.,
                       family = BE(),
                       method = CG(),
                       control=gamlss.control(c.crit = 0.001, n.cyc = 700, 
                                              mu.step = .1, sigma.step = .1,
                                              trace= F),
                       data=data3
)
########################### 
# fitting simplex regression #
###########################
# complete models
simplex_RE0<-gamlss(Prop_PSL~EP+PE+LR+MHIC+DD+
                      PG_2014_Esquerda+PG_2014_Direita+
                      PG_2018_Esquerda+PG_2018_Direita+
                      Cap_BR+
                      random(as.factor(Region)),
                    sigma.formula =~EP+PE+LR+MHIC+DD+
                      PG_2014_Esquerda+PG_2014_Direita+
                      PG_2018_Esquerda+PG_2018_Direita+
                      Cap_BR+
                      random(as.factor(Region)),
                    family = SIMPLEX(mu.link = "logit", sigma.link = "log"), 
                    method=CG(),
                    control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                           mu.step = .1, sigma.step = .1,
                                           trace= F),
                    data=data1
)

simplex_FE0<-gamlss(Prop_PSL~.,
                    sigma.formula =~.,
                    family = SIMPLEX(mu.link = "logit", sigma.link = "log"), 
                    method = CG(),
                    control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                           mu.step = .1, sigma.step = .1,
                                           trace= F),
                    data=data2
)

simplex_lat_long0<-gamlss(Prop_PSL~.,
                          sigma.formula =~.,
                          family = SIMPLEX(mu.link = "logit", sigma.link = "log"),
                          method = CG(),
                          control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                                 mu.step = .1, sigma.step = .1,
                                                 trace= F),
                          data=data3
)
######################### 
# fitting UG regression #
#########################
source("UG_reg.R") # from https://github.com/renata-rojasg/UnitDistForGAMLSS/tree/main

# complete models
UG_RE0<-gamlss(Prop_PSL~EP+PE+LR+MHIC+DD+
                 PG_2014_Esquerda+PG_2014_Direita+
                 PG_2018_Esquerda+PG_2018_Direita+
                 Cap_BR+
                 random(as.factor(Region)),
               sigma.formula =~EP+PE+LR+MHIC+DD+
                 PG_2014_Esquerda+PG_2014_Direita+
                 PG_2018_Esquerda+PG_2018_Direita+
                 Cap_BR+
                 random(as.factor(Region)),
               family = UG(mu.link = "logit", sigma.link = "log"), 
               method=CG(),
               sigma.start = .2,
               control=gamlss.control(c.crit = 0.001, n.cyc = 300,
                                      mu.step = .1, sigma.step = .1,
                                      trace= F),
               data=data1
)

UG_FE0<-gamlss(Prop_PSL~.,
               sigma.formula =~.,
               family = UG(mu.link = "logit", sigma.link = "log"), 
               method = CG(),
               sigma.start = .2,
               control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                      mu.step = .1, sigma.step = .1,
                                      trace= F),
               data=data2
)


UG_lat_long0<-gamlss(Prop_PSL~.,
                     sigma.formula =~.,
                     family = UG(mu.link = "logit", sigma.link = "log"),
                     method = CG(),
                     # sigma.start = 0.2,
                     control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                            mu.step = .1, sigma.step = .1,
                                            trace= F),
                     data=data3
)
############################### 
# fitting ULindley regression #
###############################
source("ULindley_reg.R") # from https://github.com/renata-rojasg/UnitDistForGAMLSS/tree/main

# complete models
UL_RE0<-gamlss(Prop_PSL~EP+PE+LR+MHIC+DD+
                 PG_2014_Esquerda+PG_2014_Direita+
                 PG_2018_Esquerda+PG_2018_Direita+
                 Cap_BR+
                 random(as.factor(Region)),
               family = ULindley(mu.link = "logit"),
               control=gamlss.control(c.crit = 0.001, n.cyc = 300, 
                                      mu.step = .1, sigma.step = .1,
                                      trace= F),
               data=data1
)

UL_FE0<-gamlss(Prop_PSL~.,
               family = ULindley(mu.link = "logit"), 
               control=gamlss.control(n.cyc = 300, trace=F),
               data=data2
)


UL_lat_long0<-gamlss(Prop_PSL~.,
                     family = ULindley(mu.link = "logit"),
                     trace= F,
                     data=data3
)
################## 
# results matrix #
##################

## Likelihood
bll_FE0<-gen.likelihood(beta_FE0)
bll_RE0<-gen.likelihood(beta_RE0)
bll_lat_long0<-gen.likelihood(beta_lat_long0)
sll_FE0<-gen.likelihood(simplex_FE0)
sll_RE0<-gen.likelihood(simplex_RE0)
sll_lat_long0<-gen.likelihood(simplex_lat_long0)
ugll_FE0<-gen.likelihood(UG_FE0)
ugll_RE0<-gen.likelihood(UG_RE0)
ugll_lat_long0<-gen.likelihood(UG_lat_long0)
ulindley_FE0<-gen.likelihood(UL_FE0)
ulindley_RE0<-gen.likelihood(UL_RE0)
ulindley_lat_long0<-gen.likelihood(UL_lat_long0)

## Shapiro-Wilk test
bsw_FE0<-shapiro.test(beta_FE0$residuals)
bsw_RE0<-shapiro.test(beta_RE0$residuals)
bsw_lat_long0<-shapiro.test(beta_lat_long0$residuals)
ssw_FE0<-shapiro.test(simplex_FE0$residuals)
ssw_RE0<-shapiro.test(simplex_RE0$residuals)
ssw_lat_long0<-shapiro.test(simplex_lat_long0$residuals)
ugsw_FE0<-shapiro.test(UG_FE0$residuals)
ugsw_RE0<-shapiro.test(UG_RE0$residuals)
ugsw_lat_long0<-shapiro.test(UG_lat_long0$residuals)
ulindleysw_FE0<-shapiro.test(UL_FE0$residuals)
ulindleysw_RE0<-shapiro.test(UL_RE0$residuals)
ulindleysw_lat_long0<-shapiro.test(UL_lat_long0$residuals)

## pseudo-R2
brsq_FE0<-Rsq(beta_FE0)
brsq_RE0<-Rsq(beta_RE0)
brsq_lat_long0<-Rsq(beta_lat_long0)
srsq_FE0<-Rsq(simplex_FE0)
srsq_RE0<-Rsq(simplex_RE0)
srsq_lat_long0<-Rsq(simplex_lat_long0)
ugrsq_FE0<-Rsq(UG_FE0)
ugrsq_RE0<-Rsq(UG_RE0)
ugrsq_lat_long0<-Rsq(UG_lat_long0)
ulindleyrsq_FE0<-Rsq(UL_FE0)
ulindleyrsq_RE0<-Rsq(UL_RE0)
ulindleyrsq_lat_long0<-Rsq(UL_lat_long0)


result<-matrix(c(bll_RE0(),beta_RE0$df.fit,beta_RE0$aic,
                 brsq_RE0,bsw_RE0$p.value,
                 bll_FE0(),beta_FE0$df.fit,beta_FE0$aic,
                 brsq_FE0,bsw_FE0$p.value,
                 bll_lat_long0(),beta_lat_long0$df.fit,beta_lat_long0$aic,
                 brsq_lat_long0,bsw_lat_long0$p.value,
                 sll_RE0(),simplex_RE0$df.fit,simplex_RE0$aic,
                 srsq_RE0,ssw_RE0$p.value,
                 sll_FE0(),simplex_FE0$df.fit,simplex_FE0$aic,
                 srsq_FE0,ssw_FE0$p.value,
                 sll_lat_long0(),simplex_lat_long0$df.fit,simplex_lat_long0$aic,
                 srsq_lat_long0,ssw_lat_long0$p.value,
                 ugll_RE0(),UG_RE0$df.fit,UG_RE0$aic,
                 ugrsq_RE0,ugsw_RE0$p.value,
                 ugll_FE0(),UG_FE0$df.fit,UG_FE0$aic,
                 ugrsq_FE0,ugsw_FE0$p.value,
                 ugll_lat_long0(),UG_lat_long0$df.fit,UG_lat_long0$aic,
                 ugrsq_lat_long0,ugsw_lat_long0$p.value,
                 ulindley_RE0(),UL_RE0$df.fit,UL_RE0$aic,
                 ulindleyrsq_RE0,ulindleysw_RE0$p.value,
                 ulindley_FE0(),UL_FE0$df.fit,UL_FE0$aic,
                 ulindleyrsq_FE0,ulindleysw_FE0$p.value,
                 ulindley_lat_long0(),UL_lat_long0$df.fit,UL_lat_long0$aic,
                 ulindleyrsq_lat_long0,ulindleysw_lat_long0$p.value
), ncol=5, byrow = T
)
result<-cbind(rep(c("Fit 1","Fit 2","Fit 3"),4),round(result,2))

rownames(result)<-c(rep("Beta",3),rep("Simplex",3),rep("UG",3),rep("UL",3))

colnames(result)<-c("Model","Log-likelihood",  "Degrees of Freedom", "AIC",
                    "R_G2","SW(p-value)")

# xtable::xtable(result, digits = 3)

stargazer::stargazer(result, digits = 4)

which(result[,2]==max(as.numeric(result[,2])))
which(result[,4]==min(as.numeric(result[,4])))
which(result[,5]==max(as.numeric(result[,5])))


################################## 
# step-GAIC with the final model #
##################################
beta_FE1<-stepGAIC(beta_FE0)
# checking significant effects
summary(beta_FE1)
beta_FE2<-gamlss(formula = Prop_PSL ~ PE + MHIC + Nordeste + Sul + 
                   PG_2014_Esquerda +
                   PG_2018_Esquerda + PG_2018_Direita + 
                   Cap_BR, 
                 sigma.formula = ~ EP+PE+LR+Sul+Sudeste+
                   PG_2014_Esquerda +Cap_BR, 
                 family = BE(mu.link = "logit", sigma.link = "logit"), 
                 data = data2, method = CG(), sigma.start = 0.9, 
                 control = gamlss.control(c.crit = 0.001, n.cyc = 700, mu.step = 0.1, 
                                          sigma.step = 0.1, trace = F), trace = FALSE)
final<-summary(beta_FE2)
xtable::xtable(final[,-4])
LR.test(beta_FE1,beta_FE0) # simpler model is prefered
LR.test(beta_FE2,beta_FE0) # simpler model is prefered
LR.test(beta_FE2,beta_FE1) # simpler model is prefered



Rsq(beta_FE2)
