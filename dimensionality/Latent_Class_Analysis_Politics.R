
#Q1A - Absent / present vote hierarchical clustering

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

#Q1B - poLCA applied to voting dataset

library(poLCA)

# Need to create the formula to pass to the poLCA function
formula1 <- cbind(ED1,ED2,Credit,Confidence1,Confidence2,Trade) ~ 1

# m0: Loglinear independence model
# m1: Two-class latent class model
# m2: Three-class latent class model
# m3: Four-class latent class model
# m4: Five-Class latent class model

# m2 is our preference in all of these
m0 <- poLCA(formula1,votes,nclass=1) # cannot produce a graph for single latent var.  log-likelihood: 
m1 <- poLCA(formula1,votes,nclass=2, graphs=TRUE) # log-likelihood: 
m2 <- poLCA(formula1,votes,nclass=3,maxiter=8000, graphs=TRUE) # log-likelihood:
m3 <- poLCA(formula1,votes,nclass=4,maxiter=8000, graphs=TRUE) # log-likelihood:
m4 <- poLCA(formula1,votes,nclass=5,maxiter=8000, graphs=TRUE) # log-likelihood:

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


