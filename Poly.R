##subset selection
##forward selection 
##written on 07/oct/14
##data1 refers to the k means dataset 
library(stats)
set <- glm(data1$avg_trans_per_merchant~., data = data1)
out <- step(set, direction = "forward")
## subset selection done using the function step from the package stats
summary(out)
##backward selection 
out1 <- step(set, direction = "backward")
summary(out1)
##spline
##written on 07/oct/14
library(stats)
splineout <- smooth.spline(data1$num_distinct_merchants, y = NULL, w = NULL )
##fits a cubic smoothing spline to tha dataset 
splineout1 <- spline(data1$num_distinct_merchants, y = NULL,n = 3*length(x), method = "fmm",xmin = min(x), xmax = max(x), ties = mean)
summary(splineout)
## the spline function from the package stats, performs the spline interpolatoin of the given data points 

##gam (generalized additive models)
##written on 07/oct/14
library(splines)
library(gam)
outg <- gam(data1$avg_trans_per_merchant~., family = gaussian, data = data1)
##the function gam from the package gam fits the dataset into the additive model
summary(outg)


##polynomial
##written on 07/oct/14
library(stats)
var1 <- data1[1:100,2]
c <- poly(var1, 5)
c
summary(c)
##the function poly from the package stats evaluates polynomials of degree over the specified set of points
data1<-read.csv("kmeans.csv")

############# LASSO ###############################
## written on 07th October 2014
install.packages("lars", dep = T)
library(lars)
par(mfrow=c(2,2))

attach(data1)

a <- matrix(c(num_distinct_merchants,avg_trans_per_merchant), ncol=2)

object <- lars(a,data1$cl_tid)# this is done using lars() function in package lars
plot(object)

############# Ridge Regression ####################
install.packages("ridge", dep = T)
library(ridge)
mod <- linearRidge(data1$num_distinct_merchants ~ a, data = data1)
## this is done using linearRidge() function in package ridge
summary(mod)

############# Principal Component Regression ######
############# Partial Least Squeares ##############
install.packages("pls", dep = T)
library(pls)
data1.pcr <- pcr(data1$cl_tid ~ ., data = data1, method = pls.options()$pcralg)
# this is done using pcr() function in package pls
data1.pls <- plsr(data1$cl_tid ~ ., data = data1, method = pls.options()$plsralg)
# this is done using plsr() function in package pls
summary(data1.pcr)
summary(data1.pls)
