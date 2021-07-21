# library("fitdistrplus")
# 
mLess30 <- c(7.71,12.05,18.1)
m3039 <- c(10.67,15.37,25.52)
m4049 <- c(11.07,16.67,31.18)
m5059 <- c(11.11,17.18,33.91)
m60plus <- c(11.05,20.18,45.42)
fLess30 <- c(8.65,11.78,17.02)
f3039 <- c(9.85,14.38,22.47)
f4049 <- c(9.93,14.63,24.68)
f5059 <- c(9.9,14.83,25.85)
f60plus <- c(9.95,16,30.62)

m <- c(mLess30,m3039,m4049,m5059,m60plus)
f <- c(fLess30,f3039,f4049,f5059,f60plus)

g=c("M","F")
a=c("<30","30-39","40-49","50-59",">60")
mod=c("Q1","Median","Q9")

ndim <- array(0,dim=c(2,5,3),dimnames=list(g,a,mod))
for (gender in g) {
  gender_array <- if(gender=="M") m else f
  print(gender_array)
  for (age in a) {
    age_idx <- match(age,a)
    for (i in 1:3) { ndim[match(gender,g),age_idx,i] <- gender_array[(age_idx-1)*3+i]}
  }
}

print(ndim)
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

# Method taken from
# https://stats.stackexchange.com/questions/6022/estimating-a-distribution-based-on-three-percentiles

cl_m <- rainbow(5*3,start=0.0,end=0.49)
cl_f <- rainbow(5*3,start=0.5,end=0.99)
midx <- 1
fidx <- 1

res <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(res) <- c("Gender","Age","Distribution","Param1","Param2","logliklihood") 
print(res)

plot(0,0,xlim = c(0,40),ylim = c(0,0.15), main="Estimated lnorm function for earnings", sub="accross age and gender",
     xlab="Earning per hour", ylab="Probability Density function", type = "n")
legend(27, 0.15, legend=c("Younger men", "Older men", "Younger women", "Older women"), 
       col=c(cl_m[1], cl_m[5], cl_f[1], cl_f[5]), lwd=1, cex=0.8)
for (gender in g) {for (age in a) {
  gi <- match(gender,g)
  ai <- match(age,a)
  emp <- c(ndim[gi,ai,1],ndim[gi,ai,2],ndim[gi,ai,3])
  ofln <- function(x)sum(abs(emp-qlnorm(c(0.1,0.5,0.9),x[1],x[2]))^2)
  osol_ln <- optim(c(1,1),ofln)
  if (dim(res)[1] == 0) { res[1, ] <- c(gender,age,"lognormal",osol_ln$par[1],osol_ln$par[2],osol_ln$value[1])} 
  else {res <- rbind(res,c(gender,age,"lognormal",osol_ln$par[1],osol_ln$par[2],osol_ln$value[1]))}
  #print(summary(osol_ln))
  #print(paste(gender,age,osol_ln$par,sep=" "))
  lines(1:40, dlnorm(1:40,osol_ln$par[1],osol_ln$par[2]), type="l",col=if(gender=="M") cl_m[midx] else cl_f[fidx])
  if (gender=="M") {midx <- midx + 1} else {fidx <- fidx + 1}
}}
print(res)

write.csv(res,"Development/S-at-work/scripts/fitdist/fit_earning_eurostats.csv", row.names = FALSE)
