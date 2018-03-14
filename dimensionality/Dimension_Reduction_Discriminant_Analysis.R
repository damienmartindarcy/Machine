# MVA - W4 - Discriminant Analysis

# LDA 
library(MASS)

lda.res <- lda(class ~ glucose + insulin + sspg, data=diabetes)

attributes(lda.res)
lda.res$prior
lda.res$means

# We need to set up the common covariance matrix for diabetes
N <- nrow(diabetes)
G <- length(levels(diabetes$class))
diabetes.norm <- subset(diabetes, class == "normal")
diabetes.chem <- subset(diabetes, class == "chemical")
diabetes.over <- subset(diabetes, class == "overt")
cov_norm <- cov(diabetes.norm[,2:4])
cov_chem <- cov(diabetes.chem[,2:4])
cov_over <- cov(diabetes.over[,2:4])
cov_all<-((cov_norm*(nrow(diabetes.norm)-1)) + (cov_chem*(nrow(diabetes.chem)-1)) +
              + (cov_over*(nrow(diabetes.over)-1)))/(N - G)

# Then the function to run lda
ldf <- function(x, prior, mu, covar)
{
x <- matrix(as.numeric(x), ncol=1)
log(prior) - (0.5*t(mu)%*%solve(covar)%*%mu) + (t(x)%*%solve(covar)%*%mu)
}

# Then the lda for the first patient
id <- 5
dfs <- rep(0, G)
for(g in 1:G)
{
dfs[g] <- ldf(diabetes[id,2:4], lda.res$prior[g], lda.res$mean[g,], cov_all)
}
dfs
levels(diabetes$class)[dfs == max(dfs)]


# Apply cross validation to this to reduce the misclassification rate
lda.res.cv <- lda(class ~ glucose + insulin + sspg, CV=TRUE, data=diabetes)

# Then cross tab to compare predictions with known results
table(lda.res.cv$class, diabetes$class)

# Posterior probability of membership for each group for observation 1
round(exp(dfs)/sum(exp(dfs)), 4)
round(lda.res.cv$posterior[id,], 4)

# QDA Version of this
qda.res.cv <- qda(class ~ glucose + insulin + sspg, CV=TRUE, data=diabetes)

########################################################

# Trinity Data - LDA First

salmon <- read.table("https://www.scss.tcd.ie/~houldinb/Index/MVA_files/salmon.txt",header = TRUE)
salmon

# Simple Plot
plot(salmon[,-1],col=as.factor(salmon[,1]))

# Training and Test split
strain=salmon[c(1:40,51:90),]
stest=salmon[c(41:50,91:100),]

# Train the classifier on columns 2 and 3
lsol=lda(strain[,c(2,3)],grouping=strain[,1])
lsol

# Prior and means
# Prior probability of group membership - these are the class proportions of the training data
lsol$prior
# Estimated group means for each of the two groups
lsol$means

# Covariance matrix for salmon - Two groups
alaskasalmon=salmon[c(1:40),c(2,3)]
canadasalmon=salmon[c(51:90),c(2,3)]
singlecov=(39/78)*(cov(alaskasalmon)+cov(canadasalmon))

# Determine the classification for an observation with a freshwater recording of 120 
# and a marine recording of 380

predict(lsol,c(120,380))
# Predict based on the test data
predict(lsol,stest[,c(2,3)])

# Cross validation is an alternative to test / training split - so this is based on the whole dataset
lsolcv=lda(salmon[,c(2,3)],grouping=salmon[,1],CV=TRUE)

# Plot .. Plots the two numeric values with colouring determined by true classification and symbols
# determined by the resulting classification of leave one out LDA

plot(salmon[,c(2,3)],col=as.factor(salmon[,1]),pch=as.numeric(lsolcv$class))

# Then QDA - again test and training
qsol=qda(strain[,c(2,3)],grouping=strain[,1])

predict(qsol,stest[,c(2,3)])

# As this is QDA each group distribution has its own covariance matrix as opposed to a common one in lda
alaskasalmon=salmon[c(1:40),c(2,3)]
cov(alaskasalmon)
canadasalmon=salmon[c(51:90),c(2,3)]
cov(canadasalmon)

# Playing with Pima
summary(Pima)


plot(Pima[,-7],col=as.factor(Pima[,7]))



# Basic no cv
qda.pima <- qda(type ~ glu + npreg + bp + skin + bmi + ped + age, data=Pima)

# Basic with CV
qda.pima.cv <- qda(type ~ glu + npreg + bp + skin + bmi + ped + age, CV=TRUE, data=Pima)

head(pima)

# Training and test



predict(qda.pima,PimaTest)
predict(qda.pima.cv, PimaTest)

# Covariance matrix

Pima<-Pima[order(,(type)]

Yes=pima[c(1:40),c(2,3)]
cov(Yes)
canadasalmon=salmon[c(51:90),c(2,3)]
cov(No)

# Play with Discriminer
# load iris dataset
library(DiscriMiner)
data(iris)
head(iris)


# quadratic discriminant analysis with no validation
my_qua1 = quaDA(Pima[,1:7], Pima$type)
my_qua1$confusion
my_qua1$error_rate
my_qua1$scores
predict(my_qua1, PimaTest)


# quadratic discriminant analysis with cross-validation
my_qua2 = quaDA(Pima[,1:7], Pima$type, validation="crossval")
my_qua2$confusion
my_qua2$error_rate

# quadratic discriminant analysis with learn-test validation - v1
learning = Pima
testing = PimaTest
my_qua3 = quaDA(Pima[,1:7], Pima$type, validation="learntest",
                learn=Pima, test=PimaTest)
my_qua3$confusion
my_qua3$error_rate

# quadratic discriminant analysis with learn-test validation - v2
learning = c(1:40, 51:90, 101:140)
testing = c(41:50, 91:100, 141:150)
my_qua3 = quaDA(iris[,1:4], iris$Species, validation="learntest",
                learn=learning, test=testing)
my_qua3$confusion
my_qua3$error_rate

############################################################################

# Diabetes again - with added QDA

pima.res <- qda(type ~ glu + npreg + bp + skin + bmi + ped + age, data=Pima)

predict(pima.res)
attributes(pima.res)
pima.res$prior
pima.res$means

alaskasalmon=salmon[c(1:40),c(2,3)]
cov(alaskasalmon)
canadasalmon=salmon[c(51:90),c(2,3)]
cov(canadasalmon)

################################################################

# the need for more practice - College admission data

url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/admission.csv'
admit <- read.csv(url)
head(admit)

# Plot - for EDA
adm=data.frame(admit)
plot(adm$GPA,adm$GMAT,col=adm$De)

# Basic QDA
m2=qda(type~.,Pima)
m2

# Predict
predict(m2,newdata=data.frame(npreg=7,glu=1870,bp=50,skin=133,bmi=33.9,ped=0.826,age=50))

##################################################

# More Practice

credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")
head(credit,2)

cred1=credit[, c("Default","duration","amount","installment","age")]
head(cred1)

summary(cred1)

Pima=data.frame(Pima)
zqua=qda(type~.,Pima)
attributes(zqua)
zqua$prior
zqua$means

# Confusion Matrix:
table(predict(zqua)$type, Pima$type)

# New Prediction
predict(zqua,newdata=data.frame(npreg=2,glu=187,bp=50,skin=33,bmi=23.9,ped=0.826,age=20))

table(predict(zqua)$type,Pima$type)
predict(zqua, PimaTest) 

table(zqua$type, Pima$type)
zqua$confusion
