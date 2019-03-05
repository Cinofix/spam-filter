data <- read.csv("spambase.data", header = F)
nm <- scan(file = "names.txt", what="character")
names(data) <- nm
str(data)
features <- c(1:54)
nobs <- dim(data)[1]
col <- rep(nobs, x = "blue")
col[data$class == 0] <- "red"
# blue => spam
# red => ham

pairs(data[,1:4], col = col)
pairs(data[,5:12], col = col)


m <- 54
spam <- data[data$class == 1, 1:m]
ham <- data[data$class == 0, 1:m]


email.pc <- princomp(x = data[, features])
summary(email.pc)
plot(email.pc)
plot(email.pc$scores[,1:2], pch=20, col=col, main='PCA of Swiss')
pairs(email.pc$scores[,1:10], col = col)
install.packages("ggfortify")
library(ggfortify)
autoplot(prcomp(data[,features]), data = data, colour = col )
################################################
qqnorm(data$word_freq_make)
qqline(data$word_freq_make)

qqnorm(data$word_freq_make)
qqline(data$word_freq_make)

# http://www.dummies.com/programming/r/how-to-test-data-normality-in-a-formal-way-in-r/
install.packages("car")
library(car)
shapiro.test(data$word_freq_make) 

# if I assume the significance level at 0.05 than the p-value is larger 
# then alpha (0.6921 > 0.05) and I cannot reject the null hypothesis about the normal distribution, 
# but does it allow me to say that the sample has a normal distribution?

pshapiro.test <- function(x){
  pvalue = shapiro.test(x)$p.value
  pvalue
}
pvalues <- apply(data, MARGIN = 2, FUN = shapiro.test)
min(pvalues)
pvalues

# https://www.r-bloggers.com/normality-tests-for-continuous-data/
install.packages("nortest")
library(nortest)

pad.test <- function(x){
  pvalue = ad.test(x)$p.value
  pvalue
}
pvalues <- apply(data, MARGIN = 2, FUN = pad.test)
min(pvalues)
# Let us now look at the result from the second data set’s test. The p-value of the normality test done on this data set (y, which was not generated from a normal distribution), is very low, indicating that if the null hypothesis (that the data came from the normal distribution) were to be true, there would be a very small chance of seeing the same kind of sample from such a distribution. Therefore, the Anderson-Darling normality test is able to tell the difference between a sample of data from the normal distribution, and another sample, which is not from the normal distribution, based on the test-statistic.



setwd("~/Unive/Magistrale/AI/SpamFilter/spambase")
data <- read.csv("spambase_tfidf.csv", header =F)
m <- dim(data)[2]
spam <- data[data$V55 == 1, 1:(m-1)]
ham <- data[data$V55 == 0, 1:(m-1)]

names(spam)<- nm[1:(m-1)]
names(ham)<- nm[1:(m-1)]

summary(spam[,(m-6):(m-1)])
summary(ham[,(m-6):(m-1)])

install.packages("MVN")
library('MVN')
remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

spam_nz <- remove_zero_cols(spam)
ham_nz <- remove_zero_cols(ham)
spam_nz <- spam[,apply(spam,2,function(x) !all(x==0))] 
ham_nz <- ham[,apply(ham,2,function(x) !all(x==0))] 

resSpam <- mvn(data = spam_nz, mvnTest = "mardia")
resSpam$multivariateNormality

resHam <- mvn(data = spam_nz, mvnTest = "mardia")
resHam$multivariateNormality


resSpam <- mvn(data = spam_nz, mvnTest = "hz")
resSpam$multivariateNormality

resHam <- mvn(data = spam_nz, mvnTest = "hz")
resHam$multivariateNormality

library("car")
par(mfrow = c(2,2))
qqPlot(spam$V1)
qqPlot(spam$V6)
qqPlot(spam$V24)
qqPlot(spam$V52)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
qqPlot(ham$V2)
qqPlot(ham$V22)
qqPlot(ham$V45)
qqPlot(ham$V53)
par(mfrow = c(1,1))

#####################à Independence #################

spamS <- apply(scale(spam), MARGIN = 1, sum)
plot(density((spamS)), main = "spam sum density distribution")

hamS <- apply(scale(ham), MARGIN = 1, sum)
plot(density((hamS)), main = "ham sum density distribution")
mvn(spamS,mvnTest = "mardia")

chisq.test(spam$V16, spam$V17) #word_freq_free"             "word_freq_business"  
chisq.test(spam$V19, spam$V24) #word_freq_you"             "word_freq_money"  



chisq.test(ham$V16, ham$V17) #word_freq_free"             "word_freq_business"  
chisq.test(ham$V19, ham$V24) #word_freq_you"             "word_freq_money"  

