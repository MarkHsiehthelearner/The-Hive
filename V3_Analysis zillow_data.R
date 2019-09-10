# install.packages("tidyverse", lib="C:/Program Files/Microsoft/MRO-3.3.1/library")
install.packages('tidyverse', dependencies=TRUE, type="source")
pacman::p_load(caret,tidyr,neuralnet,tidyverse)
install.packages("olsrr")


# load data
# data <- read.csv("C:\\Users\\13511935\\Desktop\\yoyoyohereyo\\dataset183.csv")
# data <- read.csv("C:\\Users\\13511935\\Desktop\\yoyoyo\\dataset183.csv")
data <- read.csv("D:\\Bigdatabase\\Datadeposit\\dataset183.csv")
head(data)

##################
# Data Wrangling #
##################

# don't need the first column
data1 <- data
data1 <- data1[,-1]

## a little bit data wrangling
sum(data1$basementsqft == 0)/nrow(data1) # the ratio that equals to 0, delete this column
data1 <- data1[,-4]
head(data1)

sum(is.na(data1$bathroomcnt))/nrow(data1) # checkout bathroomcnt
 # all good

sum(is.na(data1$bedroomcnt)) # checkout bedroomcnt
data1$bedroomcnt[is.na(data1$bedroomcnt)] <- mean(data1$bedroomcnt, na.rm = TRUE)
 # all good

sum(is.na(data1$buildingqualitytypeid)) # checkout the NAs in the column
summary(data1$buildingqualitytypeid)
data1$buildingqualitytypeid[is.na(data1$buildingqualitytypeid)] <- median(data1$buildingqualitytypeid, na.rm = TRUE)
 # using median of buildingqualitytypeid replace NAs;
 # all good

sum(is.na(data1$calculatedfinishedsquarefeet))/nrow(data1) # checkout the NAs in the calculatedfinishedsquarefeet
summary(data1$calculatedfinishedsquarefeet)
data1$calculatedfinishedsquarefeet[is.na(data1$calculatedfinishedsquarefeet)] <- median(data1$calculatedfinishedsquarefeet, na.rm = TRUE)
# hist(data1$calculatedfinishedsquarefeet)
  # using median of buildingqualitytypeid replace NAs;
 # all good

sum(is.na(data1$fips)) # considering to delete all the NA rows
# str(as.factor(data1$fips))
# splityo <- split(data1,f = data1$fips)
# split6037 <- splityo[1]
# split6059 <- splityo[2]
# split6111 <- splityo[3]
# 
# sum(is.na(split6037$fips))
# sum(is.na(split6059$fips))
# sum(is.na(split6111$fips))

sum(is.na(data1$fireplacecnt))/nrow(data1) # 90% values are missing, delete the column;
data1 <- data1[,-9]

# fullbathcnt will be filled with median, considering the bigger house will have 
 # bigger bath; garagecarcnt and garagetotalsqft is basically the same. So, delete
 # garagecarcnt column, and replace the NAs in garagetotalsqft with 
sum(is.na(data1$fullbathcnt))/nrow(data1) # replaced by mean
sum(is.na(data1$garagecarcnt))/nrow(data1) # removed
sum(is.na(data1$garagetotalsqft))/nrow(data1) # replaced by mean
data1 <- data1[,-10]
data1$fullbathcnt[is.na(data1$fullbathcnt)] <- median(data1$fullbathcnt, na.rm = TRUE)
data1$garagetotalsqft[is.na(data1$garagetotalsqft)] <- median(data1$garagetotalsqft, na.rm = TRUE)

# checkout hashottuborspa column;
sum(is.na(data1$hashottuborspa))
data1 <- data1[,-11]

# delete the latitude and longitude columns
data1 <- data1[,-12]
data1 <- data1[,-12]

# checkout pool-related columns;
data2 <- data1

# for(i in c(13:16)){
#   data2[,i] <- as.double(data1[,i])
#   print(typeof(data2[,i]))
# }

data2$poolrelated <- double(length = nrow(data2))
data2$pooltypeid7[is.na(data2$pooltypeid7)] <- 0
 # Replace all the NAs in id7 to 0;
# for(i in nrow(data2)){
#   if(!(data2[i,17] %in% data2[i,36])){
#     data2$poolrelated <- 1
#   } # project the 1s to poolrelated
# }
data2$poolrelated[!(data2[,17] %in% data2[,36])] <- 1
sum(data2$pooltypeid7==1) # checkout if the codes works,it works!
sum(data2$poolrelated==1)

data2$pooltypeid2[is.na(data2$pooltypeid2)] <- 0
 # Replace all the NAs in id2 to 0;

data3 <- data2 # just in case 
# define pool with a spa as 1.5 pool, pool without a spa as 1 pool;
poolrelafun <- function(data){     
  if(data[[16]]==0&data[[36]]==0)
    data[[36]] <- data[[36]]
  else if(data[[16]]==1&data[[36]]==1)
    data[[36]] <- data[[36]] + 0.5
  else if(data[[16]]==1&data[[36]]==0)
    data[[36]] <- 1.5
  else if(data[[16]]==0&data[[36]]==1)
    data[[36]] <- data[[36]]
}
data3$poolrelated <- apply(data3,1,poolrelafun)
data3$poolrelated <- as.numeric(data3$poolrelated)
typeof(data3$poolrelated)

sum(data2$poolrelated==1)
sum(data3$poolrelated==1.5)
sum(data3$poolrelated==1)
sum(data3$poolrelated==0)
 # checkout the results, all good;

# same reason, checkout pooltypeid10, shame, didn't work.
data4 <- data3
data4$pooltypeid10[is.na(data4$pooltypeid10)] <- 0
poolrelafun2 <- function(data){
  if(data[[15]]==0&data[[36]]==0)
    data[[36]] <- data[[36]]
   else if(data[[15]]==0&data[[36]]==1)
    data[[36]] <- data[[36]]
   else if(data[[15]]==0&data[[36]]==1.5)
    data[[36]] <- data[[36]]
   else if(data[[15]]==1&data[[36]]==0)
    data[[36]] <- 0.5
   else if(data[[15]]==1&data[[36]]==1)
    data[[36]] <- data[[36]]
   else if(data[[15]]==1&data[[36]]==1.5)
    data[[36]] <- data[[36]]
}

# poolrelafun2 <- function(dset){
#   if(dset[[15]]==1&dset[[36]]==0)
#     dset[[36]] <- 0.5
# }
data4$cct <- data4$poolrelated
data4$cct <- apply(data4,1,poolrelafun2)
typeof(data4$poolrelated)

sum(data4$poolrelated==0)
sum(data4$poolrelated==0.5)
sum(data4$poolrelated==1)
sum(data4$poolrelated==1.5)
table(data4$poolrelated)
 # this part above doesn't works, so will come back to think about it, but not 
  # this time. Pooltypeid10 was not counted in the poolrelated column.

# considering poolcnt has replaced by poolrelated and 99% poolsizesum is empty,
 # so let's delete column pooltypeid10,pooltypeid7,pooltypeid7,poolsizesum and 
 # poolcnt.
data5 <- data3
head(data5)
data5 <- data5[,-c(13:17)]
head(data5)
levels(data5$propertycountylandusecode)
levels(as.factor(data5$propertylandusetypeid))
data5$propertycountylandusecode[[1]]

data6 <- data5[,-c(13,14,15)]
plot_missing(data6)

# for(i in c(15:28)){
#   print(sum(is.na(data6[,i]))/nrow(data6))
# }
# data6 <- data6[,-c(15,19,20)]
# 
# for(i in c(15:ncol(data6))){
#   print(sum(is.na(data6[,i]))/nrow(data6))
# }
# 
# str(data6)

# Delete columns that have too many NAs
yoyodata <- data6
plot_missing(yoyodata)

yoyodata <- yoyodata[,colSums(is.na(yoyodata))/nrow(yoyodata) <= 0.85]


## further clean up the missing data
 # numberofstories
yoyodata$numberofstories[is.na(yoyodata$numberofstories)] <- 1 #number of stories, if missing then define as 1
 # unitcnt
str(as.factor(yoyodata$unitcnt)) # check what's the content on the column
yoyodata$unitcnt[is.na(yoyodata$unitcnt)] <- 1
 # lotsizesquarefeet
yoyodata$lotsizesquarefeet[is.na(yoyodata$lotsizesquarefeet)] <- mean(yoyodata$lotsizesquarefeet,na.rm = TRUE)
 # landtaxvaluedollarcnt
yoyodata$landtaxvaluedollarcnt[is.na(yoyodata$landtaxvaluedollarcnt)] <- mean(yoyodata$landtaxvaluedollarcnt,na.rm = TRUE)
 # regionidcity
yoyodata <- yoyodata[!is.na(yoyodata$regionidcity),]
 # regionidzip 
yoyodata <- subset(yoyodata, select = -regionidzip) # delete this column
 # yearbuilt
yoyodata <- yoyodata[!is.na(yoyodata$yearbuilt),] # remove missing rows
 # taxamount
yoyodata$taxamount[is.na(yoyodata$taxamount)] <- mean(yoyodata$taxamount,na.rm = TRUE)
 # taxvaluedollarcnt
yoyodata$taxvaluedollarcnt[is.na(yoyodata$taxvaluedollarcnt)] <- mean(yoyodata$taxvaluedollarcnt,na.rm = TRUE)
 # structuretaxvaluedollarcnt
yoyodata$structuretaxvaluedollarcnt[is.na(yoyodata$structuretaxvaluedollarcnt)] <- mean(yoyodata$structuretaxvaluedollarcnt,na.rm = TRUE)

yoyodata$roomcnt[is.na(yoyodata$roomcnt)] <- mean(yoyodata$roomcnt,na.rm = TRUE)
 # regionidneighborhood
yoyodata$regionidneighborhood[is.na(yoyodata$regionidneighborhood)] <- mean(yoyodata$regionidneighborhood,na.rm = TRUE)

# Fianl check 
plot_missing(yoyodata)
sum(complete.cases(yoyodata))/nrow(yoyodata) # Great !

# the cleaned data!
newdata <- yoyodata

# write the data into a csv file
write.csv(newdata,file = "zillow_tidy_data.csv")

#========================================================# START FROM HERE!!!!!





newdata <- read.csv("E:\\D\\DDD - Data\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\Bond Hackathon(First Contest)\\zillow_tidy_data.csv")
newdata <- newdata[,-1]

# now change the class of the data
# newdata %>% mutate_each_(funs(as.factor),c("bathroomcnt","bedroomcnt","airconditioningtypeid",
#                                         "buildingqualitytypeid","fullbathcnt","roomcnt","unitcnt",
#                                         "numberofstories","poolrelated"))

  # method 1 failed..... fuck
# cols = c(2,4,5,6,9,16,17,19,25)    
# newdata[,cols] <- apply(newdata[,cols], 2, as.factor)
  # method 2 failed..... fuck

newdata$airconditioningtypeid <- as.factor(newdata$airconditioningtypeid)
newdata$bathroomcnt <- as.factor(newdata$bathroomcnt)
newdata$bedroomcnt <- as.factor(newdata$bedroomcnt)
newdata$buildingqualitytypeid <- as.factor(newdata$buildingqualitytypeid)
newdata$fullbathcnt <- as.factor(newdata$fullbathcnt)
newdata$roomcnt <- as.factor(newdata$roomcnt)
newdata$numberofstories <- as.factor(newdata$numberofstories)
newdata$poolrelated <- as.factor(newdata$poolrelated)

# merge two datasets together
dflogerr <- read.csv("E:\\D\\Bigdatabase\\Datadeposit\\train_2016_v2.csv")
    # read log(error) dataset
realnewdf <- merge(newdata,dflogerr,by='parcelid',all.x=T)

plot_missing(realnewdf)

realnewdf <- realnewdf[!is.na(realnewdf$logerror),]

# Finally the cleaned data
data_tidy <- realnewdf

write.csv(data_tidy,file = "zillow_tidy_data_final.csv")

data_tidy <- read.csv("D:\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\Bond Hackathon(First Contest)\\zillow_tidy_data_final.csv")


#######################
# Feature engineering #
#######################
# what are the independent variables should I choose? Before we build a all-in model,
 # we need to get rid of all the independent variables that has multicolinearlarity.
 ## so, based on the common sense of real-estate, let's find out high-related variables manunally.

Housearearelat <- c("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "fullbathcnt",
                    "garagetotalsqft", "lotsizesquarefeet", "roomcnt", "numberofstories",
                    "poolrelated")

corofarearelat <- cor(subset(data_tidy, select = Housearearelat))
corofarearelat

# library(olsrr)
# corofarearelat2 <- ols_eigen_cindex(subset(data_tidy, select = Housearearelat))
# corofarearelat2
  # this function only works for linear model, i.e.: los_eigen_cindex(lm()).
library(corrplot)
corrplot.mixed(corofarearelat)
  # visual it

# conclusion: remove "fullbathcnt" in the model, because it has strong relation(>= 75%) 
 # with 2 virables, "bathroomcnt", "calculatedfinishedsquearefeet".
str(data_tidy)
Taxrelat <- c("taxvaluedollarcnt","taxamount","landtaxvaluedollarcnt")
coroftaxrelat <- cor(subset(data_tidy, select = Taxrelat))
corrplot.mixed(coroftaxrelat)
# conclusion: choose "taxamount", because it has the low relation, remove the other 2 from
 # the model.

Mixrelat <- c("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet",
              "garagetotalsqft", "lotsizesquarefeet", "roomcnt", "numberofstories",
              "poolrelated","taxamount")

corofmixrelat <- cor(subset(data_tidy, select = Mixrelat))
corrplot.mixed(corofmixrelat)
 # conclusion: all good.


##############
# Split Data #
##############  (train, validatiaon and test)
library(dplyr)
data_tidy_noX_transd <- select(data_tidy,-c(X,transactiondate))

data_tidy_noX_transd$airconditioningtypeid <- as.factor(data_tidy_noX_transd$airconditioningtypeid)
data_tidy_noX_transd$bathroomcnt <- as.factor(data_tidy_noX_transd$bathroomcnt)
data_tidy_noX_transd$bedroomcnt <- as.factor(data_tidy_noX_transd$bedroomcnt)
data_tidy_noX_transd$buildingqualitytypeid <- as.factor(data_tidy_noX_transd$buildingqualitytypeid)
data_tidy_noX_transd$roomcnt <- as.factor(data_tidy_noX_transd$roomcnt)
data_tidy_noX_transd$numberofstories <- as.factor(data_tidy_noX_transd$numberofstories)
data_tidy_noX_transd$poolrelated <- as.factor(data_tidy_noX_transd$poolrelated)

data_tidyXs <- select(data_tidy_noX_transd,-logerror)

library(dplyr)
choosedXs <- colnames(data_tidy_noX_transd) %in% c("fullbathcnt", "taxvaluedollarcnt", "landtaxvaluedollarcnt")
Xs <- colnames(data_tidy_noX_transd)[!choosedXs]

# split into 3 datasets, 70% train, 10% validation, 20% test.
set.seed(13311)
ind <- sample(3,nrow(data_tidy_noX_transd),replace = TRUE,prob = c(0.7,0.1,0.2))

Train <- data_tidy_noX_transd[ind==1,]
Vlida <- data_tidy_noX_transd[ind==2,]
Test <- data_tidy_noX_transd[ind==3,]

TrainXs <- Train[,-26]
VlidaXs <- Vlida[,-26]
TestXs <- Test[,-26]

LogETrain <- Train$logerror
LogEVlida <- Vlida$logerror
LogETest <- Test$logerror


##################
# Model Analysis #
##################
# LASSO to choose independent virables

library(dplyr)
library(glmnet)
rreg1 <- glmnet(as.matrix(TrainXs),LogETrain,family = "gaussian",alpha=1) # standardize=T, ,lambda=c(0.00001:0.0001)
print(rreg1)

cv_rreg <- cv.glmnet(data.matrix(TrainXs),LogETrain, family = "gaussian",alpha=1,type.measure = "mae")
plot(cv_rreg)


rreg2 <- glmnet(as.matrix(TrainXs),LogETrain,family = "gaussian",alpha=1,
                lambda = lambdas_to_try)
# plot(rreg2, xvar = "lambda")
lbs_fun <- function(rreg2, ...) {
  L <- length(rreg2$lambda)
  x <- log(rreg2$lambda[L])
  y <- rreg2$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  legend('topright', legend=labs, col=1:6, lty=1) # only 6 colors
}
plot(rreg2, xvar="lambda", col=1:dim(coef(rreg2))[1])
lbs_fun(rreg2)
  # amazing function to show legend on glmnet().




## ----------------- Here is Chapter Two ------------------- ##
data_tidy <- read.csv("D:\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\Bond Hackathon(First Contest)\\zillow_tidy_data_final.csv")

# before PCA, adjust the data a bit
library(installr)
uninstall.packages("httr")
install.packages("httr")
library(httr)
library(tidyverse)

  # convert the first column into rownames
datatrail <- data_tidy
datatrail <- datatrail[, -1]

  # to test if there are duplications 
yoyo <- datatrail %>%
          select(parcelid)
dim(yoyo)
sum(duplicated(yoyo))
87698 - 121
  # 验证重复row的存在
yoyo[715,]
datatrail[datatrail$parcelid == "10736972", ]
    # yes, there are 121 duplicates, that's the reason the 'parcelid' column can't be
      # rownames, 

datatrail[datatrail$parcelid == dupli_ind, ]
datatrail %>%
  filter(parcelid == dupli_ind)

datatrail %>% 
  distinct(parcelid, .keep_all = TRUE) # maybe this is the datatrail_2

datatrail_finally <- datatrail_2

  # Remove duplicated rows based on parcelid
datatrail_yolo <- datatrail_finally[,-1]
rownames(datatrail_yolo) <- datatrail_finally[,1]
head(datatrail_yolo)
    # confidence up !!!!!
data_tidy <- datatrail_yolo
    # after finish trail, store it back
  # now write the data out
# write.csv(data_tidy, "zillow_tidy_parcelidasrowname")


## now more feature engineering
  # checkout if they should be cataglory as discrete or factor
data_tidy %>%
  select(airconditioningtypeid,bathroomcnt,bedroomcnt) %>%
  mutate(airconditioningtypeid = as.factor(airconditioningtypeid),
         bathroomcnt = as.factor(bathroomcnt),
         bedroomcnt = as.factor(bedroomcnt)) %>%
  summary()
  # it fucking piss me off, there is an 'other' row in the summary result, I need to 
    # figue it out what is it.
yoyo <- c(1, 1.5, 2, 2.5, 3, 4)
datatrail <- data_tidy %>%
             select(airconditioningtypeid,bathroomcnt,bedroomcnt) %>%
             mutate(airconditioningtypeid = as.factor(airconditioningtypeid),
                    bathroomcnt = as.factor(bathroomcnt),
                    bedroomcnt = as.factor(bedroomcnt))
             
table(datatrail$bathroomcnt)
table(datatrail$bedroomcnt)
    # try take bathroomcnt and bedroomcnt as discrete variables

  # borrow the data from the PCA part, using data_tidy_noyearloca
data_tidy <- read.csv("D:\\Bigdatabase\\Datadeposit\\zillow_tidy_parcelidasrowname.csv", 
                      header = TRUE, fill = T, row.names = NULL) ## YOYOYO, start here!!
colnames(data_tidy)
data_tidy_noX <- data_tidy[ , -1]
rownames(data_tidy_noX) <- data_tidy[,1]
# head(data_tidy_noX)
str(data_tidy_noX)
  # ok, great.









## let's try PCA for feature engineering purpose
# before that, remove time and location feature
yoyo <- c("regionidcity", "yearbuilt", "assessmentyear", "transactiondate", "fips")
data_tidy_noyearloca <- data_tidy_noX[ , !(colnames(data_tidy_noX) %in% yoyo)]
str(data_tidy_noyearloca)
    # looks good

# now it is model time!!!
pca <- prcomp(data_tidy_noyearloca, center = TRUE,scale. = TRUE)

# pca model visionlizaiton
devtools::install_github("kassambara/factoextra")
library(factoextra)
fviz_eig(pca, addlabels = TRUE)
    # looks pretty good

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) 
    # don't do this, computer frozed. !!!!!!!!!

# scree plot option 2
screeplot(pca, npcs = min(10, length(pca$sdev)),
          type = "lines")


#yoloooo,,, try the fviz_pca_ind function with smaller sample
# get a sample out, with 5000 obs
set.seed(13311)
sample_ind <- sample(2,nrow(data_tidy_noyearloca),replace = TRUE,prob = c(0.1, 0.9))
yoyo <- data_tidy_noyearloca[sample_ind==1,]
str(yoyo)

pcayoyo <- prcomp(yoyo, center = TRUE,scale. = TRUE)
fviz_pca_ind(pcayoyo, 
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

















