# MVA Labs and Projects

# 1 - Hierarchical Clustering

library(pdfCluster)
data(oliveoil)
head(oliveoil)

acids = oliveoil[,3:10]

StDev = apply(acids, 2, sd)
apply(acids, 2, mean)
stdacids = sweep(acids, 2, StDev, "/")

cl.average = hclust(dist(stdacids, method="euclidean"), method="average")
plot(cl.average)

cl.single = hclust(dist(stdacids), method="single")
plot(cl.single)

cl.complete = hclust(dist(stdacids), method="complete")
plot(cl.complete)

hcl = cutree(cl.complete, k = 3)
hcl
table(hcl)
table(hcl, oliveoil[,1])

# 2 - K Means Clustering

data(faithful)

WGSS <- rep(0,10)

n <- nrow(faithful)

WGSS[1] <- (n-1) * sum(apply(faithful, 2, var))

for(k in 2:10)
{
WGSS[k] <- sum(kmeans(faithful, centers = k)$withinss)
}

plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")

K <- 2
cl <- kmeans(faithful, center=K)
table(cl$cluster)

plot(faithful, col= cl$cluster)
points(cl$centers, col=1:K, pch=8)


# Silhouette Plot of this 

library(cluster) 
library(HSAUR) 
data(pottery) 
km <- kmeans(stPottery,3) 
dissE <- daisy(stPottery) 
dE2<- dissE^2
sk2 <- silhouette(km$cl, dE2) 
plot(sk2) 

# Cluster Plot allows examination of individual observations 

clusplot(stPottery, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


hcl <- cutree(hclust(dist(faithful)), 2)
pcl <- kmeans(faithful, centers=2)
tab <- table(hcl, pcl$cluster)
tab

library(e1071)

classAgreement(tab)

# 3 - LDA / QDA

# Initial Analysis
library(plyr)
library(MASS)
Pima.tr
Pima.te

Pima=data.frame(Pima)
head(Pima)
summary(Pima)
count(Pima, "type")

# model 1 - no validation

qda.fit1=qda(type~.,Pima)
qda.fit1$counts

qda.class1 <- predict(qda.fit1, Pima)$class
table(qda.class1)
table(Pima$type)
table(qda.class1, Pima$type) 

#This suggests that QDA predictions are accurate ~ 77% of the time
mean(qda.class1 == Pima$type)

# model 2 - Training and test data

# Training and Test split
require(caTools)
set.seed(101) 
sample = sample.split(Pima$type, SplitRatio = .80)
train = subset(Pima, sample == TRUE)
test = subset(Pima, sample == FALSE)
count(train, "type")
count(test, "type")

# Training
qda.fit2=qda(type~.,train)
qda.fit2$counts

# Test
qda.class2 <- predict(qda.fit2, test)$class
table(qda.class2)
table(qda.class2, test$type) 

#This suggests that QDA predictions are accurate ~ 76% of the time
mean(qda.class2 == test$type)

# Applied to entire set
qda.class3 <- predict(qda.fit2, Pima)$class
table(qda.class3)
table(qda.class3, Pima$type) 

# The mean is slightly lower than the original (77.2% vs 77.6%)
mean(qda.class3 == Pima$type)

# Q3A - New Prediction - each model

qda.fit1
qda.fit2

predict(qda.fit1,newdata=data.frame(npreg=7,glu=187,bp=50,skin=33,bmi=33.9,ped=0.826,age=30))
predict(qda.fit2,newdata=data.frame(npreg=7,glu=187,bp=50,skin=33,bmi=33.9,ped=0.826,age=30))

# Q3B -  Applied to test set of five observations

head(PimaTest)
predict(qda.fit1, PimaTest) 
predict(qda.fit2, PimaTest) 

# 4 - Generating MV data and KNN

# 900 Group A Points
N <- 900
muA <- c(0,0)
SigmaA <- matrix(c(10,3,3,2),2,2)
x <- mvrnorm(n=N, muA, SigmaA)

plot(x)
kdfitx <- kde2d(x[,1], x[,2], n=10)
contour(kdfitx, add=TRUE, col="red", nlevels=6)

# 900 Group B Points
N <- 900
muB <- c(0,0)
SigmaB <- matrix(c(12,2,2,15),2,2)
y <- mvrnorm(n=N, muB, SigmaB)

plot(y)
kdfity <- kde2d(y[,1], y[,2], n=10)
contour(kdfity, add=TRUE, col="green", nlevels=6)

# Join the two samples together
z <- rbind(x, y)
dim(z)

# Then assign class membership to each group as 1 and 2
cls <- c(rep(1,N), rep(2,N))
z.dat <- data.frame(z, cls)
plot(z.dat)

# We can overlay the two contour plots
plot(z.dat[,1:2], type="n")
contour(kdfitx, add=TRUE, col="red", nlevels=6)
contour(kdfity, add=TRUE, col="blue", nlevels=6)

# Then KNN
library(class)

index <- c(1:600, 901:1500)
train <- z.dat[index, 1:2]
test <- z.dat[-index, 1:2]

# Apply with k=3
knn(train, test, cl = z.dat[index,3], k=3)

# Misclassification rate
result <- knn(train, test, cl=z.dat[index, 3], k=3)
(nrow(test) - sum(diag(table(result, z.dat[-index,3])))) / nrow(test)

# 5 - Model Based Clustering

library(mclust)
data(thyroid)
pairs(thyroid[,-1])

res = Mclust(thyroid[,-1], G=1:5)
summ = summary(res)
summ

# Plot BIC values
res$BIC
plot(res, what="BIC", legendArgs = list(x="topleft", cex=0.5,horiz=T), ylim=c(-7400, -4000))

plot(res, what="classification")

attributes(summ)
summ$pro
summ$mean
summ$variance

# Assess uncertainty associated with the model
plot(res, what="uncertainty")

# Cluster uncertainty at the individual patient level
plot(res$uncertainty, type="h")

# Rand Index comparison with full medical record patients
table(thyroid[,1], summ$classification)
adjustedRandIndex(thyroid[,1], summ$classification)

# 6 - PCA

# Unstandardised
data(iris)
fit = prcomp(iris[,1:4])
fit

names(fit)

fit$sdev

summary(fit)

round(fit$rotation, 2)

plot(fit)

newiris = predict(fit)

plot(newiris[,1], newiris[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris[,1], newiris[,2], labels=substr(iris[,5],1,2),
col=as.integer(iris[,5]))

# Standardise
apply(iris[,1:4], 2, var)

iris.dat = iris[,1:4]
sds = apply(iris.dat, 2, sd)
iris.std = sweep(iris.dat, 2, sds, "/")
fit.std = prcomp(iris.std)

fit.std <- prcomp(iris[,1:4], scale.=T)
plot(fit.std)

# 7 - Factor Analysis

# from Ewart Thomas's "Factor Analysis and Cluster Analysis"

# Stanford Data
d = read.table("http://www.stanford.edu/class/psych253/data/personality0.txt")
head(d)

library(corrplot)
corrplot(cor(d), order = "hclust", tl.col='black', tl.cex=.75) 

d_stan = as.data.frame(scale(d))

### Factor analysis with no rotation
res1b = factanal(d_stan, factors = 10, rotation = "none", na.action = na.omit)
res1b$loadings

# Compute eigenvalue of factor 1
loadings_fac1 = res1b$loadings[,1]
eigenv_fac1 = sum(loadings_fac1^2); eigenv_fac1

# Compute proportion variance
eigenv_fac1/32

res1b$uniquenesses

# Calculate uniqueness
loadings_distant = res1b$loadings[1,]
communality_distant = sum(loadings_distant^2); communality_distant

uniqueness_distant = 1-communality_distant; uniqueness_distant

### Plot loadings against one another
load = res1b$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # add variable names

### Factor analysis with rotation
res1a = factanal(d_stan, factors = 10, rotation = "varimax", na.action = na.omit)
res1a$loadings

### Plot loadings against one another
load = res1a$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # add variable names

k = 2
p = 5

yes = 'yes! lets extract k factors!'
no = 'no! we need more data or fewer factors!'

ifelse(((p-k)^2 > p+k), yes, no)

# Combining synonyms to get extended factors
shy = rowMeans(cbind(d_stan$distant, d_stan$shy, d_stan$withdrw, d_stan$quiet))
outgoing = rowMeans(cbind(d_stan$talkatv, d_stan$outgoin, d_stan$sociabl))
hardworking = rowMeans(cbind(d_stan$hardwrk, d_stan$persevr, d_stan$discipl))
friendly = rowMeans(cbind(d_stan$friendl, d_stan$kind, d_stan$coopera, d_stan$agreebl, d_stan$approvn, 
                          d_stan$sociabl))
anxious = rowMeans(cbind(d_stan$tense, d_stan$anxious, d_stan$worryin))
#etc, you guys choose what you want to combine
combined_data = cbind(shy,outgoing,hardworking,friendly,anxious)
combined_data = as.data.frame(combined_data)
res2 = factanal(combined_data, factors = 2, na.action=na.omit)
res2$loadings

### Plot loadings against one another
load = res2$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(combined_data),cex=.7) # add variable names

# 8 - Latent Class Analysis (poLCA)

data(carcinoma)
data(election)
data(cheating)

# One plan is to simulate data for this:

library(poLCA)
alpha = rep(0.25, 4) ## equal
p1 = 0.85
p2 = 0.1
p3 = 0.2
P = matrix(c(
  p1,p2,p2,p2,
  p1,p3,p3,p3,
  p2,p1,p2,p2,
  p3,p1,p3,p3,
  p2,p2,p1,p2,
  p3,p3,p1,p3,
  p2,p2,p2,p1,
  p3,p3,p3,p1), nrow=8, ncol=4, byrow = TRUE)

simLCA <- function(alpha, ProbMat, sampleSize, nsim) {
  ## Reformat the probability Matrix to a form required by poLCA.simdata
  probs = vector("list", nrow(ProbMat))
  for (i in 1:nrow(ProbMat)) {
    probs[[i]] = cbind(ProbMat[i,], 1 - ProbMat[i,])
  }
  X = poLCA.simdata(N = sampleSize*nsim, probs = probs, P = alpha)$dat
  split(X, rep(1:nsim, each=sampleSize))
}

set.seed(37421)
X = simLCA(alpha, P, sampleSize=50, nsim=1)
head(X)


#Q2A - Absent / present vote hierarchical clustering

library(cluster)
library(factoextra)
library(NbClust)
library(HSAUR)
library(ltm)
options(max.print = 10000)

# EDA
mode(votes)
head(votes)
descript(votes)
cor(votes)
names(votes)

# Implement - Distance based on euclidean / binary
dist.eucl<-dist(votes, method = "euclidean") 

# Then - hierarchical clustering of this based on hclust (average, single, complete)
cl.complete=hclust(dist.eucl, method="complete")
plot(cl.complete)

# How many clusters? - 5
clustercount<- NbClust(votes, distance = "euclidean", min.nc = 2,
                       max.nc = 10, method = "complete", index ="all")

# Implement - cut dendrogram into 5 clusters
hcl=cutree(cl.complete, k=5)
table(hcl)

# Visualize
clusplot(votes, hcl, color=TRUE, shade=TRUE, plotchar=TRUE, labels=0, lines=0)
plot(silhouette(hcl, dist.eucl))

# Cluster Membership
x<-cbind(votes,hcl)
x1<- subset(x, hcl==1)
x2<- subset(x, hcl==2)
x3<- subset(x, hcl==3)
x4<- subset(x, hcl==4)
x5<- subset(x, hcl==5)

# Tidying up
x["hcl"] <- hcl
x[order(x$hcl),]

##################################################################

#Q2B - poLCA applied to voting dataset

library(poLCA)
head(election)
# Need to create the formula to pass to the poLCA function
formula1 <- cbind(A,B,C,D,E,F,G) ~ 1
E<-election

# m0: Loglinear independence model
# m1: Two-class latent class model
# m2: Three-class latent class model
# m3: Four-class latent class model
# m4: Five-Class latent class model

# m2 is our preference in all of these
m0 <- poLCA(formula1,C,nclass=1) # cannot produce a graph for single latent var.  log-likelihood: 
m1 <- poLCA(formula1,C,nclass=2, graphs=TRUE) # log-likelihood: 
m2 <- poLCA(formula1,C,nclass=3,maxiter=8000, graphs=TRUE) # log-likelihood:
m3 <- poLCA(formula1,C,nclass=4,maxiter=8000, graphs=TRUE) # log-likelihood:
m4 <- poLCA(formula1,C,nclass=5,maxiter=8000, graphs=TRUE) # log-likelihood:

# observed and estimated frequencies for each of the response patterns
m0$predcell
m1$predcell
m2$predcell

# Predicted latent variable membership for m2
m2$predclass
table(m2$predclass)

x<-cbind(votes,m2$predclass)
x1<- subset(x, m2$predclass==1)
x2<- subset(x, m2$predclass==2)
x3<- subset(x, m2$predclass==3)

x["hcl"] <- hcl
x["M2"]<- m2$predclass
x[order(x$hcl),]

###################################################################

# Q2C - Results Comparison
table(hcl, m2$predclass)

####################################################################
#Q2D - Strategic absence / presence

library(plyr)

# refinement - nrep to avoid local maxima
m2 <- poLCA(formula1,votes,nclass=3,maxiter=8000, nrep=200,graphs=TRUE)

# Preliminary - Integrate Party Membership Data / Tidy up

x<-votes
members<-members.party
x["Party"]<-members$Party
x["Name"]<- members$TD

count(x, 'Party') # number of tds in each party
summary(x2)
# Class membership by party
x1<- subset(x, m2$predclass==1)
x1 <- x1[order(x1$Party), ]
count(x1, 'Party')
x2<- subset(x, m2$predclass==2)
x2 <- x2[order(x2$Party), ]
count(x2, 'Party')
x3<- subset(x, m2$predclass==3)
x3 <- x3[order(x3$Party), ]
count(x3, 'Party')

# Party membership by class - top 5 parties

xfg<-subset(x, x$Party=="People Before Profit Alliance")
xfg <- xfg[order(xfg$M2), ]

xla<-subset(x, x$Party=="Labour")
xla <- xla[order(xla$M2), ]

xff<-subset(x, x$Party=="Fianna Fail")
xff <- xff[order(xff$M2), ]

xin<-subset(x, x$Party=="Independent")
xin <- xin[order(xin$M2), ]

xsf<-subset(x, x$Party=="Sinn Fein")
xsf <- xsf[order(xsf$M2), ]






