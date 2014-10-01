##factor analysis 
# entering raw data and extracting 3 factors, 
# with varimax rotation 
# Written on 22/09/2014
data1<-read.csv("kmeans.csv")
data.fac <- data.frame(data1)
fit <- factanal(data.fac[,2:8], 3, rotation="varimax") ##factanal function calculates this.



##principal component analysis 
## written on  22/09/2014
data.pc<- data.frame(data1$avg_trans_per_merchant, data1$avg_trans_amt)
(pc.cl  <- princomp(data.pc)) # function that does the calculation is princomp

##outlier detection 
## written on  22/09/2014
install.packages("outliers")
library(outliers)
chisq.out.test(data1$num_trans_2012) ## outliers found by the chisq.out.test function 


##cosine similarity 
## written on  23/09/2014
library("SnowballC")
library(lsa)
vec1 = data1$pcnt_trans_q1 
vec2 = data1$pcnt_trans_q2
cosine(vec1,vec2) # cosine function calculates this

##Singular value decomposition 
## written on  23/09/2014
svd.data1<-data1[1:96,4]
svd.data2<-data1[1:96,5]
matrix <- matrix(svd.data1, svd.data2)
svd.output<-svd(matrix) ##svd function computes the single value decomposition 
svd.output

##multinomial logist regression
## written on  23/09/2014
library(nnet)
vec1 = data1$pcnt_trans_q1 
vec2 = data1$pcnt_trans_q2
vec3 = data1$pcnt_trans_q3
m <- multinom(vec1 ~ vec2 + vec3) ## multinom function enables this 


##LDA(LINEAR DISCRIMINANT ANALYSIS)
## written on  23/09/2014 
library (MASS)
lin1<-data1$pcnt_trans_clothing
lin2<-data1$pcnt_trans_food
lin3<-data1$pcn_trans_stores
lin.out<-lda(lin1 ~ lin2 + lin3) ##lda functionj enables this 
lin.out

##vector quantization
## written on  24/09/2014
install.packages("class")
library("class")
lv2<- data1$num_trans_2013
lvd <- data.frame(lv1)
lev<- factor(c(rep("low",40), rep("med", 40), rep("high",40)))
lv3 <- lvqinit(lvd,lev, 10) ##vector quantizaton done by lvqinit function 



## k-nearest neighbour
## written on  24/09/2014
library(class)
k1 <- data1$pcnt_trans_q1
k2 <- data1$pcnt_trans_q2
k3 <- data1$pcnt_trans_q3
k4 <- data1$pcnt_trans_q4

kd <- data.frame(k1,k2,k3,k4)

n1<- kd[1:50,1]
n2<- kd[1:50,2]
n3<- kd[1:50,3]
n4<- kd[1:50,4]

nd <- data.frame(n1,n2,n3,n4)

o1 <- k1[51:100]
o2 <- k2[51:100]
o3 <- k3[51:100]
o4 <- k4[51:100]

od <- data.frame(o1,o2,o3,o4)
## k = 3 neighbours considered 
level <- factor(c(rep("a",10),rep("b",10),rep("c",10),rep("d",10),rep("e",10)))
test <- knn(nd,od,level,k=3,prob = TRUE) ## nearest neighbours computed by knn function 
test
summary(test)


##DBSCAN
## written on  24/09/2014

library(fpc)
k1 <- data1$pcnt_trans_q1
k2 <- data1$pcnt_trans_q2
k3 <- data1$pcnt_trans_q3
k4 <- data1$pcnt_trans_q4

kd <- data.frame(k1,k2,k3,k4)
out <- dbscan(kd,0.02) ## dbscan function performs the DBSCAN of data 
out

##C50 algo
## written on  24/09/2014
library(C50)
k5 <- data1$onlin_enrolled
c5 <- data.frame(k1,k2,k3,k4)
y<-as.factor(k5)
c5out <- C5.0(x = c5, y ) ## function C5.0 performs the decision tree building 
al.out <- summary(c5out)
al.out





##anova
## written on  22/09/2014
## analysis of variance
ano.out <- aov( data1$pcnt_trans_q1 ~ data1$pcnt_trans_q2 + data1$pcnt_trans_q3 , data = data1 )
## analysis of variance done by anova 


##backpropogation
## written on  25/09/2014
library(neuralnet)
library(grid)
library(MASS)

nn<-neuralnet(data1$avg_trans_per_merchant ~ data1$num_trans_2012 + data1$num_trans_2013 + data1$total_num_trans + data1$total_num_trans + data1$avg_trans_amt,data=data, err.fct="sse",hidden=c(3),linear.output=FALSE )

## neuralnet function enables the backpropogation of the data
summary(nn)

##apriori
## written on  26/09/2014
## association rules
c1 <- as.factor(data1[,2])
c2 <- as.factor(data1[,3])
c3 <- as.factor(data1[,4])
c4 <- as.factor(data1[,5])
c5 <- as.factor(data1[,6])
c6 <- cbind(c1,c2,c3,c4,c5)
## Mine association rules.
rules <- apriori(c6, parameter = list(supp = 0.5, conf = 0.9,target = "rules")) ## frequent pattern obtained by the apriori function 
summary(rules)


##eclat 
## written on  26/09/2014
##Mining associaton rules
eout <- eclat(c6, parameter = list(supp = 0.1, maxlen = 15)) ## frequent pattern found by eclat function 
summary(eout)


##ALS (Alternating least square)
## written on  26/09/2014
library(nnls)
library(Iso)
library(ALS)

a1 <- as.factor(data1[1:20,2])
a2 <- as.factor(data1[1:20,3])
cmat <- cbind(a1,a2)

a3 <- as.factor(data1[1:20,4])
a4 <- as.factor(data1[1:20,5])

pmat <- cbind(a3,a4)
vec1 <- c(1,0)
vec2 <- c(0,1)
smat <- cbind(vec1,vec2)
##test0 <- als(CList=list(cmat),S=matrix(1,nrow=2,ncol=2),PsiList=list(pmat),  uniC=TRUE, normS=0)
a1 <- as.factor(data1[1:20,2])
a2 <- as.factor(data1[1:20,3])
c1 <- cbind(a1,a2)
a3 <- as.factor(data1[1:20,4])
a4 <- as.factor(data1[1:20,5])
c2 <- cbind(a3,a4)

a5 <- as.factor(data1[1:20,6])
a6 <- as.factor(data1[1:20,7])

a7 <- as.factor(data1[1:20,8])
a8 <- as.factor(data1[1:20,9])
p1 <- cbind(a5,a6,a7,a8)

a9 <- as.factor(data1[1:20,10])
a10 <- as.factor(data1[1:20,11])

a11 <- as.factor(data1[1:20,12])
a12 <- as.factor(data1[1:20,13])
p2 <- cbind(a9,a10,a11,a12)

test1 <- als(CList=list(c1,c2),S=matrix(1,nrow=4,ncol=2),PsiList=list(p1,p2),  uniC=TRUE, normS=0)
## the als function performs the alternating least square
