# library("fitdistrplus")
# 
mLess30 <- c(7.71,12.05,18.1)
m3039 <- c(10.67,15.37,25.52)
m4049 <- c(11.07,16.67,31.18)
m5059 <- c(11.11,17.18,33.91)
m60plus <- c(11.05,20.18,45.42)
# 
# res_g <- fitdist(mLess30,"gamma")
# res_ln <- fitdist(mLess30,"lnorm")
# res_w <- fitdist(mLess30,"weibull")
# 
# cdfcomp(list(res_g,res_ln,res_w), xlogscale = TRUE, ylogscale = TRUE, legendtext = c("gamma", "lognormal", "weibull"))
# 
# gofstat(list(res_g,res_ln,res_w),fitnames=c("gamma","lnorm","weibull"))

library(nleqslv)
library(VGAM)

quant <- c(0.1,0.5,0.9)

for (emp in c(mLess30,m3039,m4049,m5059,m60plus)) {
  ofln <- function(x)sum(abs(emp-qlnorm(quant,x[1],x[2]))^2)
  osol_ln <- optim(c(1,1),ofln)
  print(osol_ln$par)
  plot(x <- seq(5,30,by=0.5), dlnorm(x,osol_ln$par[1],osol_ln$par[2]), type="l",col=1)
}


lines(x <- seq(5,30,by=0.5), dsinmad(x,osol_sm$par[1],osol_sm$par[2],osol_sm$par[3]), type="l",col=2)
#plot(emp,quant)
