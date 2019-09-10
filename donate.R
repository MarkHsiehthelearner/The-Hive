donors <- read.csv("D:\\Bigdatabase\\Datadeposit\\Donors.csv")
donation <- read.csv("D:\\Bigdatabase\\Datadeposit\\Donations.csv")

library(tidyverse)
fill <- merge(donors, donation, by.x = "Donor.ID", by.y = "Donor.ID")

dim(donors)
dim(donation)
dim(fill)
abs(nrow(fill) - nrow(donation))

sum(complete.cases(fill))
str(fill)
head(fill)

## take a look at the distribution of donation amount
donation %>%
  filter(Donation.Amount <= 500) %>%
  ggplot(aes(x = Donation.Amount)) +
  geom_histogram(bins = 500, fill = "red") +
  labs(x= 'Donation Amount',y = 'Count') +
  theme_bw()



## exploring the data
# take a look for how many people donated more than once?
a <- rle(fill[1])
b <- data.frame(a$values, a$lengths)
colnames(b)[2] <- "frequency"
head(b, 30)
dim(b)
max(b$frequency)

# ----------------------- using a subset data ------------------- #
set.seed(1415)
smp_ind <- sample(nrow(fill), size = 30000)
sample <- fill[smp_ind, ]
sample_fill <- sample
# write.csv(sample_fill, "sample_fill.csv")  
sample_fill <- read.csv("D:\\Turing The Genius\\VERONICA\\kaggle Codes study(Look over here)!!!!!!!\\Donate\\sample_fill.csv")
# YoYo!!  Just start here!!!!!!!!
sample_fill <- sample_fill[, -1]
head(sample_fill)

a <- rle(sample_fill[1])
b <- data.frame(a$values, a$lengths)
colnames(b)[2] <- "frequence"
max(b$frequence)
min(b$frequence)
head(b, 30)

b_combined <- with(b, aggregate(list(y = frequence), list(x = tolower(Donor.ID)), sum))
    # code above showed how many times that each of them donated.
head(b_combined)
arrange(b_combined, desc(y))
  # Great, so far, I know who donate more than once

## who donate most of the money?
b2 <- sample_fill %>%
  select(Donor.ID, Donation.Amount)

b2_combined <- with(b2, aggregate(list(y = Donation.Amount), list(x = tolower(Donor.ID)), sum))
    # code above showed how much money that each of them donated.
b2_combined %>%
  arrange(desc(y))

# inner_join the two dataset
b2_timesandamount <- merge(b_combined, b2_combined, by.x = "x", by.y = "x")
colnames(b2_timesandamount)[2] <- "frequency"
colnames(b2_timesandamount)[3] <- "amount"
b2_timesandamount %>%
  arrange(desc(amount))

b2_timesandamount %>%
  arrange(desc(frequency))

# add another column of amount/frequency and named it average
b2_timesandamount <- b2_timesandamount %>%
  mutate(average = amount / frequency) %>%
  arrange(desc(average)) 

dim(b2_timesandamount)

# draw the point plot of amount vs frequency
b2_timesandamount %>%
  select(frequency, amount, average) %>%
  summary()


ggplot(b2_timesandamount, aes(x = amount, y = frequency)) + 
  geom_point() + 
  geom_jitter(width = 0.5, height = 0.5)


## just have a intuitive that wanna use kmeans to classified
set.seed(20)
clusters <- kmeans(as.matrix(b2_timesandamount), 3)

b2TD <- b2_timesandamount
# convert the first column into rownames
b2TD <- b2TD %>% remove_rownames %>% column_to_rownames(var="x")

b2TD_noavg <- b2TD[, -3]
b2TD_noavg <- scale(b2TD_noavg)

k2 <- kmeans(b2TD_noavg, centers = 3, nstart = 25)
# install.packages("factoextra")
library(factoextra)
fviz_cluster(k2, data = b2TD_noavg)


## see the relationship of if an individual is a teacher
b3 <- sample_fill %>%
  select(Donor.ID, Donor.Is.Teacher)

nrow(b3) - sum(duplicated(b3))
  # make a easy test
b3_rmvredundent <- b3[!duplicated(b3),]
    # looks good

b4 <- merge(b2_timesandamount, b3_rmvredundent, by.x = "x", by.y = "Donor.ID")
colnames(b4)[1] <- "Donor.ID"
b4_Donoridasrowname <- b4 %>% remove_rownames %>% column_to_rownames(var="Donor.ID")
    # looks Great !!!!

library(ggthemes)
ggplot(b4_Donoridasrowname, aes(x = amount, y = frequency)) + 
  geom_point(aes(colour = Donor.Is.Teacher)) + 
  theme_few()


## visionize the propotion of number of donation
a <- rle(sample_fill[1])
b <- data.frame(a$values, a$lengths)
colnames(b)[2] <- "frequence"

b_combined <- with(b, aggregate(list(Frq = frequence), list(Dnr_id = tolower(Donor.ID)), sum))
b_combined <- arrange(b_combined, desc(Frq))

b_trail <- b_combined
count_frq <- function(data, n){
  data %>% 
    filter(Frq == n) %>%
    count()
}

yoyo <- as.numeric(levels(as.factor(b_trail$Frq)))
class(yoyo)
yoyo

emptyvec <- numeric(0)
for (i in yoyo){
  emptyvec[[i]] <- (count_frq(b_trail, i)$n/nrow(b_trail))
}

emptyvec <- as.data.frame(emptyvec)
d <- as.vector(rownames(emptyvec));d

emptyvec <- emptyvec %>% 
            mutate(NO.ofFrq = d) %>%
            na.omit()

emptyvec %>%
arrange(desc(emptyvec)) %>%
  mutate(NO.ofFrq = reorder(NO.ofFrq, emptyvec)) %>%
ggplot(aes(x = NO.ofFrq, y = emptyvec)) + 
  geom_bar(stat = "identity", fill = "green") + 
  geom_text(aes(x = NO.ofFrq, y = 1, label = paste0("( ",round(emptyvec,5)*100,"%"," )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  coord_flip() +
  theme_bw()
  # Looks good Yo!

# this is the second option of visionize the propotion of number of donation
  # brutal strike from the kaggle kernel
getDonorCount <- function(donor_count, n,TotalNoOfRows){
  donor_count_n <- donor_count %>%
  filter(Count == n)
  return(nrow(donor_count_n)/TotalNoOfRows *100)
}

donor_count_df <-   data.frame(no_of_donations = as.numeric(),Percentage = as.numeric())
CalculatePercentageDonations <- function(donor_count, TotalNoOfRows, donor_count_df,no_of_donations){
  Percentage = getDonorCount(donor_count,no_of_donations,TotalNoOfRows)
  donor_count_df <- rbind(donor_count_df,data.frame(no_of_donations= no_of_donations,Percentage))
  return(donor_count_df)
}

donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,1)
donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,2)
donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,3)
donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,4)
donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,5)
donor_count_df <- CalculatePercentageDonations(donor_count,TotalNoOfRows,donor_count_df,6)
donor_count_df %>%
  arrange(desc(Percentage)) %>%
  mutate(no_of_donations = reorder(no_of_donations,Percentage)) %>%
  ggplot(aes(x = no_of_donations,y = Percentage)) +
  geom_bar(stat='identity',fill= fillColor) +
  geom_text(aes(x = no_of_donations, y = 1, label = paste0("( ",round(Percentage,2)," )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'No of Donations', 
       y = 'Percentage', 
       title = 'No of Donations and Percentage') +
  coord_flip() +
  theme_bw()
  ## Finish of visionize the propotion of number of donation





















## how many people donate the 80% of the money? what are their features?


## which state/city has the most people to donate?

## which state/city donate most of the money?



# ----------------------- using a subset data ------------------- #














# ---------------- Backups（教学） -------------------#
## Rename a column in R(教学)
colnames(data)[colnames(data)=="old_name"] <- "new_name"
names(data)[3]<-"new_name"
    # both are good

## In R Merging rows where a column has same value but different case
# option 1
with(df, aggregate(list(y = y, z = z), list(x = tolower(x)), sum))
# option 2
library(data.table)
as.data.table(df)[, lapply(.SD, sum), by = .(x = tolower(x))]
# option 3
xtabs(cbind(y = y, z = z) ~ tolower(x), df)

## Convert the values in a column into row names in an existing data frame in R
# option 1
samp2 <- samp[,-1]
rownames(samp2) <- samp[,1]
# option 2
R> df<-data.frame(a=letters[1:10], b=1:10, c=LETTERS[1:10])
R> rownames(df) <- df[,1]
R> df[,1] <- NULL
# option 3
library(tidyverse)
samp %>% remove_rownames %>% column_to_rownames(var="names")
