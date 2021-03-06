# Classification Tree
library(rpart)

#fit tree 
fit <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE,main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#extract probabilities from model
prob <- predict(fit, kyphosis, type ="prob")

#show probabilities with data
dat <- cbind(kyphosis, prob)