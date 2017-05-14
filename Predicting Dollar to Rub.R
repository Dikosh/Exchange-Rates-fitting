library(Quandl)
library(zoo)
library(dplyr)
oil.ts <-Quandl("EIA/PET_RBRTE_D", trim_start="2005-04-03", trim_end="2017-03-10", type="zoo", collapse="weekly")
usd.ts <-Quandl("BOE/XUDLBK69", trim_start="2005-04-03", trim_end="2017-03-10", type="zoo", collapse="weekly")
tr.s <- "2015-03-01"; tr.e <- "2016-12-31"
train <- data.frame(oil=as.vector(window(oil.ts, start=tr.s, end=tr.e)),
                    rub=as.vector(window(rub.ts, start=tr.s, end=tr.e)),
                    date=index(window(oil.ts, start=tr.s, end=tr.e))) %>% 
  mutate(month=factor(format(date, "%m")), 
         date=NULL)
te.s <- "2017-01-01"; te.e <- "2017-03-15"
test <- data.frame(oil=as.vector(window(oil.ts, start=te.s, end=te.e)),
                   rub=as.vector(window(rub.ts, start=te.s, end=te.e)),
                   date=index(window(oil.ts, start=te.s, end=te.e))) %>% 
  mutate(month=factor(format(date, "%m")), 
         date=NULL)
fit1 <- lm(rub ~ oil, data=train)
summary(fit1)
which.min(sapply(1:5, function(i) AIC(lm(rub ~ poly(oil, i), data=train))))
library(forecast)
fit2 <- auto.arima(train$rub, xreg=train$oil)
summary(fit2)
library(nlme)
fit3 <- gls(rub ~ oil, data=train, correlation=corAR1(value=0.7, form=~1|month))
summary(fit3)
which.min(sapply(1:5, function(i) {
  d <- data.frame(poly(train$oil, i), month=train$month, rub=train$rub)
  AIC(gls(rub ~ . - month, data=d, correlation=corAR1(value=0.7, form=~1|month)))
}))
library(R2jags)
model1 <- "model {
  for (i in 1:n) {
    y[i] ~ dnorm(a + inprod(X[i,], b[]), tau)
  }
  for (j in 1:p) {
    ind[j] ~ dbern(pind)
    bT[j] ~ dnorm(0, taub)
    b[j] <- ind[j] * bT[j]
  }
  a ~ dnorm(0, 1e-04)
  tau ~ dgamma(1, 1e-03)
  taub ~ dgamma(1, 1e-03)
  pind ~ dbeta(2, 9)
}"
p <- 5
m.jags1 <- jags(data=list(y=train$rub,
                          X=poly(train$oil, p),
                          n=nrow(train),
                          p=p),
                parameters.to.save=c("a", "b", "ind"),
                model.file=textConnection(model1),
                n.chains=1, n.iter=5000)
m.jags1
model2 <- "model {
  a ~ dt(0, 5, 1)
  b ~ dt(0, 5, 1)
  phi ~ dunif(-1, 0.999)
  tau0 ~ dgamma(1, 1e-03)
  tau[1] <- tau0
  y[1] ~ dnorm(a + b * x[1], tau[1])
  for (i in 2:n){
    tau[i] <- tau0 + phi * tau[i-1]
    y[i] ~ dnorm(a + b * x[i], tau[i])

  }
}"
set.seed(123)
n <- nrow(test)
m.jags2 <- jags(data=list(y=c(train$rub, rep(NA, n)),
                          x=c(train$oil, test$oil),
                          n=nrow(train)+n),
                parameters.to.save=c("a", "b", "phi", "y"),
                model.file=textConnection(model2),
                n.chains=1, n.iter=5000)
