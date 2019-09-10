train <- read.csv("D:\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\mercedes-benz-greener-manufacturing\\train.csv")
test <- read.csv("D:\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\mercedes-benz-greener-manufacturing\\test.csv")

sapply(train, function(x) sum(is.na(x)))
sum(complete.cases(train))
sum(is.na(train))

## don't have a clue how to deal with it, let's try some tools to decrease dimentions
# Ridge regression
library(glmnet)
x <- model.matrix( ~ ., train[,-2])


ridge_mod <- glmnet(x,train$y, alpha=0,lambda=exp(seq(10,-5,-0.1)),standardize=F)
plot(ridge_mod,xvar="lambda",ylab="Coef",label=T)

cv.out_ridge <- cv.glmnet(x,train$y,alpha=0,nfolds=10,lambda=exp(seq(10,-5,-0.1)))
plot(cv.out_ridge)

# LASSO
cv.out_LASSO <- cv.glmnet(x,train$y,alpha=1,nfolds=10,
                          lambda=exp(seq(10,-5,-0.1)))
plot(cv.out_LASSO)
cv.out_LASSO$lambda.1se; log(cv.out_LASSO$lambda.1se)
cv.out_LASSO$lambda.min; log(cv.out_LASSO$lambda.min)
# so, prefer to choose lambda.1se for LASSO_mod model

LASSO_mod <- glmnet(x,train$y,alpha=1,lambda = exp(seq(10,-5,-0.1)))
plot(LASSO_mod,xvar="dev",ylab="Strandarised Coefficients",label=T)
plot(LASSO_mod,xvar="norm",ylab="Coef",label=T)





## now try the PCA
# before pca, trainsfer all columns into numeric
yoyo <- train[,-c(1,2)]
yoyo[] <- lapply(yoyo, function(x) as.numeric(x))
str(yoyo)

# pca - 1
pca_mod <- prcomp(yoyo, center = TRUE,scale. = FALSE)
library(factoextra)
fviz_eig(pca_mod, addlabels = TRUE)

# pca - 2
pca_mod2 <- princomp(yoyo)
pca_mod2$loadings[,1:4]

data(USJudgeRatings)
class(USJudgeRatings)
library(psych)
fa.parallel(USJudgeRatings,fa="pc",n.iter = 100,show.legend = T,main="Cattell碎石检验",ylabel="特征值")
abline(1,0)
  # well, this one works

  # install.packages("psych")
library(psych)
fa.parallel(yoyo,fa="pc",n.iter = 1000,show.legend = T,main="Cattell碎石检验",ylabel="特征值")
  # But this one fucking doesn't

PC1 <- principal(USJudgeRatings, nfactors = 3, scores = T)
PC1

PCyoyo <- principal(yoyo, nfactors = 1, scores = T)
PCyoyo


####################################
seq(3, 10)
library(tidyverse)
train0 <- train %>%
  mutate_at(vars(X0:X8), funs(factor))

train_f <- train %>%
  mutate_at(vars(starts_with("X")), funs(factor))
  # vars(): This helper is intended to provide equivalent semantics to select().



