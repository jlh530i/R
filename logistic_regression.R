#Logisitic regression in R using GLM

#import data
dat <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)

#take a look
head(dat)
str(dat)
summary(dat)

#build model
fit <- glm( cbind(dat$using, dat$notUsing) ~ ., data=dat, family = binomial)

#backwards stepwise
back = step(fit)

#resulting dataset
dat2 <- cbind(fit$data, fit$fitted.values)

colnames(dat2) <- c('age','education','wantsMore','notUsing','using','p')

dat2 <- dat2[order(-dat2$p),]

