
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2) # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
##compare average, sd between heights of males and females
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
# predict male if the height is within two sds
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
#see that the accuracy goes up by typing this
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  #overall proportion that is predicted correctly
  #50% because we're guessing
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


###### make a confusion matrix 
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)



##################################################################
#### Guess male with higher probability to give higher accuracy due to sample bias
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

###### The ROC curve plots sensitivity (TPR) versus 1 - specificity or the 
###### false positive rate (FPR). Here we compute the TPR and FPR needed for
###### different probabilities of guessing male:
  

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")
################################################################################################
### homework practice 

library(dslabs)
library(dplyr)
library(lubridate)
library(e1071)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- factor(dat$type, c("inclass", "online"))

#################### ignore #######################################
onlinecounts <- 0

for(i in 1:nrow(dat)){
 if(dat[i,]$type == "online")
   onlinecounts <- onlinecounts + 1
}
onlinecounts

inclasscounts <- 0
for(i in 1:nrow(dat)){
  if(dat[i,]$type == "inclass")
    inclasscounts <- inclasscounts + 1
}
inclasscounts

counts <- 0
for(i in 1:nrow(dat)){
  if(dat[i,]$sex == "Female" && dat[i,]$type == "online")
    counts <- counts + 1
}
counts/onlinecounts

################################################################
## test alternative method

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# guess the outcome
#y_hat <- sample(c("Male", "Female"), nrow(dat), replace = TRUE)
#guess based on the factor "TYPE"
## their code that doesn't work
levels(dat$sex)
y_hat <- ifelse(x =="inclass", "Male", "Female") %>% factor(levels = levels(dat$sex))

#my code that does work to compute the mean
y_hat <- ifelse(x == "inclass", "Female", "Male") %>%
  factor(levels = c("Female", "Male"))
mean(y_hat == dat$sex)

table(predicted = y_hat, actual = dat$sex)
dat %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

dat_sex <- dat$sex %>% factor(levels = c("Female", "Male"))
cm <- confusionMatrix(data = y_hat, reference = dat_sex)

my_sensitivity <- sensitivity(data = y_hat, reference = dat_sex)
sensitivity(y_hat, dat_sex)
specificity(y_hat, dat_sex)
prev <- mean(y == "Female")
prev

###################################################################################################
### Homework Section 2.1

library(caret)
library(dplyr)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2) # if using R 3.6 or later

test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]


y_hat <- sample(c("versicolor", "virginica"), length(test_index), replace = TRUE)
y_hat <- sample(c("versicolor", "virginica"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test$Species))


mean(y_hat == test$Species)

misl <- min(train$Petal.Length)
masl <- max(train$Petal.Length)


cutoff <- seq(misl, masl, 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  #overall proportion that is predicted correctly
  mean(y_hat == train$Species)
})

best_cutoff <- cutoff[which.max(accuracy)]
max(accuracy)
# compute overall accuracy

best_cutoff


y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = c("virginica", "versicolor"))

species_vec <- test$Species
test_cm <- confusionMatrix(data = y_hat, reference = species_vec)
test_cm[["overall"]][["Accuracy"]]

######### refit using training data 

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2) # if using R 3.6 or later


test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]


y_hat <- sample(c("versicolor", "virginica"), length(test_index), replace = TRUE)
y_hat <- sample(c("versicolor", "virginica"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(train$Species))


mean(y_hat == train$Species)

misl <- min(train$Petal.Width)
masl <- max(train$Petal.Width)


cutoff <- seq(misl, masl, 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  #overall proportion that is predicted correctly
  mean(y_hat == train$Species)
})

best_cutoff <- cutoff[which.max(accuracy)]
petalWidthAccuracy <- max(accuracy)
# compute overall accuracy

best_cutoff


y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = c("virginica", "versicolor"))

species_vec <- test$Species
test_cm <- confusionMatrix(data = y_hat, reference = species_vec)
pl_width_acc <- test_cm[["overall"]][["Accuracy"]]

plot(iris,pch=21,bg=iris$Species)


twofactor_acc <- ifelse(test$Petal.Width > best_cutoff || test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = c("virginica", "versicolor"))

species_vec <- test$Species
test_cm <- confusionMatrix(data = twofactor_acc, reference = species_vec)
pl_widthlength_acc <- test_cm[["overall"]][["Accuracy"]]


################################################################################################
# 2.2 Conditional Probabilities
# Homework


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
## generate all the data
test <- rep(NA, 1e6)
## prob of testing neg when patient is healthy
# test is 90% accurate when patient does not have disease
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
## prob of testing pos when patient has the disease
# test is 85% accurate when patient does
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

## prob that a test is positive
mean(disease[test==1]==1)

## change in change of getting flu in light of a positive test
mean(disease[test == 1]==1)/mean(disease==1)

#conditional prob of being male 
library(dslabs)
data("heights")
#P(x)= Pr(Male | height=x) for each x
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)


### assure each group has same number of points 
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#### generate data from bivariate normal dist.


#1
rm(list=ls())
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y =mean(y), x =mean(x)) %>%
  qplot(x, y, data =.)

#########################################################################################################
### Ch.3 Linear Regression
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
#predict son's height y using father's height x
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg
#ignore dad's height and just guess based on average
mean((avg - test_set$son)^2)

#root mean squared error
sqrt(mean((m - test_set$son)^2))


# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)


#################################################################################################
## comprehension check
#Q1 
### some code I stole and modified
library(tidyverse)
library(dslabs)
library(caret)

##################################################################

set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
results <- sapply(n, function(n){
  #Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  #set.seed(1, sample.kind = "Rounding")
  replicate(100, { test_index <- createDataPartition(dat$y, times =1, p=0.5, list=FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
    
  })
})
## Q4
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
results <- sapply(n, function(n){
  #Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  set.seed(1, sample.kind = "Rounding")
  replicate(100, { test_index <- createDataPartition(dat$y, times =1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
  
  })
})
mean(results)
sd(results)

#Q6 

# set.seed(1) # if using R 3.5 or earlier



# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


#Q8


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


############################################### Regression for a categorical outcome ###################################

library(dslabs)
data("heights")
y <- heights$height

set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

###############################################################
 # comprehension check 
# Q1 


# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()
set.seed(1, sample.kind="Rounding")
mu_1 <- seq(0, 3, len=25)


res <- sapply(mu_1, function(mu_1){
  dat <- make_data(mu_1 = mu_1)
  train <- dat$train %>% mutate(y == 1)
  test <- dat$test  %>% mutate(y == 1)
  fit_glm <- glm(y ~ x, data = train, family = "binomial")
  p_hat <- predict(fit_glm, newdata = test, type = "response")
  y_hat <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
  confusionMatrix(y_hat, test$y)$overall["Accuracy"]
})

res
plot(res)

##############################################################################################################
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(Rcpp)
library(dplyr)
library(stringr)
#Q1  
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(stringr::str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")


dat <- na.omit(dat)

days <- as.numeric(difftime(time2 = min(dat$date),time1 = max(dat$date),units = "days"))
s <- 60/days

span= total_days <- as.numeric(diff(range(dat$date)))

fit <- loess(deaths ~ as.numeric(date), degree=1, span = s, data=dat)

#dat %>%
#  ggplot(aes(date, deaths)) +
#  geom_point() +
#  geom_smooth(color="red", span = s, method = "loess", method.args = list(degree = 1))


#option4
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#redict 2s and 7s in the mnist_27 dataset with just the second covariate
#regression the coefficient for x_2 is not significant
#Q3
library(broom)
library(dslabs)
data("mnist_27")
dat <- mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

#plotting the two predictors and using colors to denote the labels
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_2, data = .)



mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

###################################################################################
#3.3

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

range <- seq(51, 204)
num <- x[x %in% range]

length(num)/length(x)
####################################################################################
### section 4

library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

##############################################################################
#4.1 comprehension check
library(dslabs)
library(caret)

data(tissue_gene_expression)
dim(tissue_gene_expression$x)

y_express <- table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)

image(as.matrix(d))

image(as.matrix(d)[order(x), order(x)])

d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[784,]

image(1:7, 1:7, matrix(d_492, 7, 7))
###########################################################################################
library(dslabs)
library(caret)
#Q1
data("heights")
y <- heights$sex
x <- heights$height
set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)

test_set <- heights %>% slice(test_index)
# use sapply to perform knn with k values of seq(1,101,3)
#calculate F1 scores with F_meas() using default value of arg
ks <- seq(1, 101,3)


F1_score <- sapply(ks, function(ks){
  fit <- knn3(sex ~ height, data = train_set, k= ks)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(test_set$sex))
  f1 <- F_meas(data = y_hat, reference = test_set$sex)
  
})


max(F1_score)
ks[which.max(F1_score)]

#### Q2
library(dslabs)
library(caret)

data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
tissue_df <- data.frame(tissue_gene_expression)
test_index <- createDataPartition(tissue_df$y, times = 1, p = 0.5, list = FALSE)
train_set <- tissue_df[-test_index,]

test_set <- tissue_df[test_index,]
ks <- seq(1, 11,2)
res <- purrr::map_df(ks, function(ks){
  fit <- knn3(y ~ ., data = train_set, k= ks)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(test_set$y))
  confusionMatrix(y_hat, test_set$y)$overall["Accuracy"]
  
})

qplot(ks, res$Accuracy)


res$Accuracy
#############################################################################

library(tidyverse)
library(caret)
library(dplyr)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- caret::train(x_subset, y, method = "glm")
fit$results


library(genefilter)
tt <- colttests(x, y)

#########################################################################
# Q3 
#Create an index ind with the column numbers of the predictors that were 
#"statistically significantly" associated with y. Use a p-value cutoff of 
#0.01 to define "statistically significantly." 
pvals <- tt$p.value

ind <- pvals <= 0.01
ind_sig <- which(ind == 1)
x_subset<- x[ ,ind]

fit_sig <- train(x_subset, y, method = "glm")
fit_sig$results

length(pvals[pvals < 0.01])

# Q4
#Now re-run the cross-validation after redefinining x_subset to be the subset 
#of x defined by the columns showing "statistically significant" association 
#with y. What is the accuracy now?

#######################################################################
# Q5
#Re-run the cross-validation again, but this time using kNN. Try out the following grid k = seq(101, 301, 25)

x_subset<- x[ ,ind]
ks <- seq(101, 301,25)
#res <- sapply(ks, function(ks){
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
#})

#Q7
#Use the train() function with kNN to select the best k for predicting tissue 
#from gene expression on the tissue_gene_expression dataset from dslabs.
#Try k = seq(1,7,2) for tuning parameters. For this question, do not split the
#data into test and train sets
#What value of k results in the highest accuracy?
library(dslabs)
library(caret)

data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

fit <- train(x,y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)

##################################################################################################
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
three <- indexes$Resample10 == 3
which(three == TRUE)
three <- indexes$Resample09 == 3
which(three == TRUE)
three <- indexes$Resample08 == 3
which(three == TRUE)
three <- indexes$Resample07 == 3
which(three == TRUE)
three <- indexes$Resample06 == 3
which(three == TRUE)
three <- indexes$Resample05 == 3
which(three == TRUE)
three <- indexes$Resample04 == 3
which(three == TRUE)
three <- indexes$Resample03 == 3
which(three == TRUE)
three <- indexes$Resample02 == 3
which(three == TRUE)
three <- indexes$Resample01 == 3
which(three == TRUE)

#Q3

set.seed(1, sample.kind="Rounding") # if R 3.6 or later

y <- rnorm(100, 0, 1)


B <- 10

M <- replicate(B, {
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
  indexes <- createResample(X, 10)
X <- rnorm(100, 0,1)

})

mean(M)
sd(M)

## Q4

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")

q75s_boot <- replicate(10, {
  Y_star <- sample(y, 10000, replace = TRUE)
  quantile(Y_star, 0.75)
})

mean(q75s_boot)
sd(q75s_boot)
###################################################################################################
## 4.3

#Q1
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


### Q3
library(dslabs)      
library(caret)
data("tissue_gene_expression")
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


## Q5
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

xy <- data.frame(y = y, x = x)
result <- train(y ~ ., data = xy, preProcess = "center", method = "lda")

result

################################################################################################
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)



fit <- rpart(y ~ .) 


fit <- rpart(y, ., data = dat) 


fit <- rpart(x ~ ., data = dat) 
plot(fit)

fit <- rpart(y ~ ., data = dat) 
plot(fit)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  
  
geom_step(aes(x, y_hat), col=2)

##############################################################################

library(randomForest)
#fit <- randomForest(y ~ x, data = dat)


fit <- randomForest(x ~ y, data = dat)


#fit <- randomForest(y ~ x, data = data)


#fit <- randomForest(x ~ y)



  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
plot(fit)

########
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 25, maxnodes = 25)


#fit <- randomForest(y ~ x, data = dat, nodes = 50, max = 25)


#fit <- randomForest(x ~ y, data = dat, nodes = 50, max = 25)


#fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)


#fit <- randomForest(x ~ y, data = dat, nodesize = 50, maxnodes = 25)



  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)

############ Comprehension check Section 5
##Q1
library(rpart)
#Load the rpart package and then use the caret::train() function with method = "
#rpart" to fit a classification tree to the tissue_gene_expression dataset. Try out
#cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of the 
#best model. Set the seed to 1991. Which value of cp gives the highest accuracy?
set.seed(1991, sample.kind = "Rejection") # if using R 3.6 or later
cp <- seq(0, 0.1, 0.01)
df <- data.frame(tissue_gene_expression$x)
df <- df %>% mutate(y = tissue_gene_expression$y)

train_rpart <- caret::train(y ~ ., 
                     method = "rpart",control = rpart.control(minsplit = 0),
                     tuneGrid = data.frame(cp = cp),
                     data = df)

ggplot(train_rpart, highlight = TRUE)
bestfit <- train_rpart$bestTune
confusionMatrix(data = train_rpart, reference = tissue_gene_expression$y)

plot(train_rpart$finalModel)
text(train_rpart$finalModel, cex = 0.5)



######
#We can see that with just seven genes, we are able to predict the tissue type. 
#Now let's see if we can predict the tissue type with even fewer genes using a Random
#Forest. Use the train() function and the rf method to train a Random Forest model
#and save it to an object called fit. Try out values of mtry ranging from seq(50, 200, 25)
#(you can also explore other values on your own). What mtry value maximizes accuracy? 
#To permit small nodesize to grow as we did with the classification trees, use the 
#following argument: nodesize = 1

library(rpart)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
mtry <- seq(50, 200, 25)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
xy <- data.frame(y = y, x = x)
df <- df %>% mutate(y = tissue_gene_expression$y)

fit <- caret::train(y ~ ., 
                            method = "rf", nodesize = 1,
                            tuneGrid = data.frame(mtry = mtry),
                            data = df)

ggplot(fit, highlight = TRUE)

xy <- data.frame(y = y, x = x)
fit <- train(y ~ ., method = "rf",
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1, data=xy)

ggplot(fit, highlight = TRUE)

imp <- varImp(fit)
#################################################
set.seed(1991, sample.kind = "Rejection") # if using R 3.6 or later
#cp <- seq(0, 0.1, 0.01)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
xy <- data.frame(y = y, x = x)
fit <- train(y ~ ., method = "rf",
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1, data=xy)


#ggplot(train_rpart, highlight = TRUE)
#bestfit <- train_rpart$bestTune
#confusionMatrix(data = train_rpart, reference = tissue_gene_expression$y)

#plot(train_rpart$finalModel)
#text(train_rpart$finalModel, cex = 0.5)

tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms

imp <- varImp(fit)
datt = data.frame(imp$importance,rank=rank(-imp$importance)) %>% 
  filter (rownames(imp$importance) %in% tree_terms)

############## 5.3 #######################################################

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Split titanic_clean into test and training sets - after running the setup code, it should have 891 rows and 9 variables.
#Set the seed to 42, then use the caret package to create a 20% data partition based on the Survived column. Assign the 
#20% partition to test_set and the remaining 80% partition to train_set.
set.seed(42, sample.kind = "Rounding")
y <- titanic_clean$Survived
x <- titanic_clean$Embarked

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]

test_set <- titanic_clean[test_index,]

### Q2
set.seed(3, sample.kind = "Rounding")
randomguess <- sample(c(0,1), nrow(test_set), replace = TRUE) %>% factor()
options(digits = 3)
mean(randomguess == test_set$Survived)

### Q3

byClass <- train_set %>% 
  filter(Survived == 1)
first_class <- which(train_set$Pclass == 1)
second_class <- which(train_set$Pclass == 2)
third_class <- which(train_set$Pclass == 3)



length(first_class)/length(byClass)
length(second_class)/length(byClass)

length(third_class)/length(byClass)


### Q4
y_hat <- ifelse(x == 1, "female", "male") %>%
  factor(levels = c("female", "male"))
mean(y_hat == test_set$Sex)


survival_rate <- train_set %>%
  group_by(Pclass) %>%
  filter(Survived == 1)


## if survival rate for a class is > 0.5 else predict death
y_hat <- ifelse(test_set$Pclass == 1, 1, 0) %>%
  factor(levels = c(1, 0))
mean(y_hat == test_set$Survived)


test <- train_set %>%
  group_by(Pclass) %>% 
  summarize(Survived = mean(Survived==1)) %>% 
  filter(Survived > 0.5)

#### 4c
male_survival_rate <- train_set %>%
  group_by(Sex, Pclass) %>%
  dplyr::summarize(Survived = mean(Survived ==1), Sex = mean(Sex == "male"))

#### 4d
y_hat <- ifelse((test_set$Sex == "male" | test_set$Pclass == 3),0,1) %>%
  factor(levels = c(0, 1))
mean(y_hat == test_set$Survived)

y_hat <- ifelse((test_set$Sex == "female" & test_set$Pclass != 3),1,0) %>%
  factor(levels = c(0, 1))
mean(y_hat == test_set$Survived)


#### Q5
# sex only sensitivity Sensitivity : 0.873 
Pclass_model <- ifelse(test_set$Pclass == 1, 1, 0) %>% 
  factor(levels = c(0,1))

cm_pclass <- confusionMatrix(Pclass_model, test_set$Survived)
cm_pclass
# Balanced Accuracy : 0.659 
#Sensitivity : 0.855

sex_model <- ifelse(test_set$Sex == "female", 1, 0) %>% 
  factor(levels = c(0,1))

cm_sex <- confusionMatrix(sex_model, test_set$Survived)
cm_sex
#Sensitivity : 0.873
# Balanced Accuracy : 0.806

Pclass_sex_model <- ifelse((test_set$Sex == "female" & test_set$Pclass != 3),1,0) %>%
  factor(levels = c(0,1))

cm__sex_pclass <- confusionMatrix(Pclass_sex_model, test_set$Survived)
cm_sex_pclass

#Sensitivity : 0.991
#Balanced Accuracy : 0.771 


#####Q6

f1_sex <- caret::F_meas(data = sex_model, reference = factor(test_set$Survived))
f1_class <- caret::F_meas(data = Pclass_model, reference = factor(test_set$Survived))
f1_sex_class <- caret::F_meas(data = Pclass_sex_model, reference = factor(test_set$Survived))


f1_sex
f1_class
f1_sex_class

################ Section 6
### example code
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
#############################


models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Now that you have all the trained models in a list, use sapply() or map() to create a matrix of 
#predictions for the test set. You should end up with a matrix with length(mnist_27$test$y) rows and 
#length(models) columns.

#What are the dimensions of the matrix of predictions?
#Q2
#used the built in predict function instead of sapply/map to build the matrix
test_results <- predict(fits, newdata = mnist_27$test)


#Q3
test_accuracy <- sapply(test_results, function(t){
  confusionMatrix(t, mnist_27$test$y)$overall[1]
})

mean_accuracy <- mean(test_accuracy)


#Q4
#votes <- data.frame(integer(200))
votes <- rep(0,200)
models_df <- data.frame(test_results)
new_modeldf <- models_df[,-c(2, 4,7,10)]


new_modeldf <- new_modeldf %>% mutate(vote = 0)
for(i in 1:nrow(new_modeldf)){
  numSevens <- length(which(new_modeldf[i,] == 7))
  numTwos <- length(which(new_modeldf[i,] == 2))
  if(numSevens > numTwos){
    new_modeldf$vote[i] <- 7
  } else {
    new_modeldf$vote[i] <- 2
 }
  }

cm <- confusionMatrix(new_modeldf$vote %>% factor(levels = c(2,7))
, mnist_27$test$y)

one <- fits$glm$results$Accuracy
two <- fits$lda$results$Accuracy
three <- fits$naive_bayes$results$Accuracy
four <- fits$svmLinear$results$Accuracy
five <- fits$knn$results$Accuracy
six <- fits$gamLoess$results$Accuracy
seven <- fits$multinom$results$Accuracy
eight <- fits$qda$results$Accuracy
nine <- fits$rf$results$Accuracy
ten <- fits$adaboost$results$Accuracy

mean(one,
two,
min(three),
four,
min(five),
six,
min(seven),
eight,
nine,
min(ten))

########################################################################################
### 6.2

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
movieobj <- movielens %>% select(c(title, year, userId))
movieobj <- movieobj %>% dplyr::group_by(title, drop = FALSE) %>% summarize(n())
testdf <- movielens %>%
  dplyr::group_by(title) %>%
  summarize(count = n())


testdf_titles <- testdf$title
years <- movielens %>%
  select(c(title, year)) %>%
  distinct()

finaldf <- full_join(testdf, years, by = "title")



ggplot(finaldf, aes(x=year,y=count,group=year)) + geom_boxplot() +scale_y_sqrt() +
scale_x_discrete(limits = min(movielens$year, na.rm = T):max(movielens$year, na.rm = T))

##### Q2

post1993 <- movielens %>% filter(year >= 1993 & year <= 2018)
post1993 <- post1993 %>% rowwise() %>% mutate(yearRated = floor(timestamp/(60*60*24*365)+1970))

ratesPerYear <- perYearRatings %>%
  group_by(title) %>%
  summarize(ratings = n())

sortRatesPerYear <- ratesPerYear[order(ratesPerYear$ratings, decreasing = TRUE), ]
first25 <- head(sortRatesPerYear, 25)

test <- perYearRatings %>% count(title, yearRated)

forest <- test %>% filter(title == "Forrest Gump")
sum(forest$n)/(nrow(forest))


forrest2 <- post1993 %>% filter(title == "Forrest Gump")
test2 <- forrest2 %>% count(title, yearRated)
#ratings <- post1993 %>%
#  dplyr::group_by(title) %>% 
#  summarize(aveRating = mean(rating))
#finaldf <- full_join(ratings, testdf3, by = "title")
#finaldf <- finaldf %>% filter(title %in% titles)

###### Q5
#movielens <- mutate(movielens, date = as.date(timestamp))

movielens <- mutate(movielens, date = as_datetime(timestamp))


## Q7

top1000 <- movielens %>% count(genres)
top1000 <- top1000 %>% filter(n > 1000)

ratingByGenre <- movielens %>% select(genres, rating)
genreList <- unfactor(top1000$genres)


filtereddf <- movielens %>% filter(genres %in% genreList)

test3 <- filtereddf %>% count(genres, rating)


rateByGenre <- aggregate(rating ~ genres, data = filtereddf, 
          FUN = function(x) c(mean = mean(x), se = plotrix::std.error(x)))
min(rateByGenre$rating.mean)
