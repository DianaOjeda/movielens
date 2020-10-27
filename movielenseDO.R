
#Load libraries
library(tidyverse)
library(lubridate)
library(caret)
library(mgcv)
myfill <- "skyblue3"
mycolor <- "gray20"

#load data
validation <- read.csv("validation.csv", header=TRUE)
edx <- read.csv("edx.csv", header=TRUE)
#save databases
saveRDS(edx, "edx.rds")
saveRDS(validation, "validation.rds")
save()

############################################
#         Data wrangle                      #
############################################

# Create numeric variable from titles
# new variable called "year" is the year the movie came out

edx <- edx %>% mutate(year=str_extract(title, ".{5}$")) %>%
    mutate(year =as.numeric(str_extract(year,"\\d{4}")))

validation <- validation %>% mutate(year=str_extract(title, ".{5}$")) %>%
    mutate(year =as.numeric(str_extract(year,"\\d{4}")))

#Create auxiliary variables from datestamp
# year of review, month of review


edx <-edx %>% mutate(yearreview = year(as_datetime(timestamp)))
edx <- edx %>% mutate(monthreview = month(as_datetime((timestamp))))

validation <-validation %>% mutate(yearreview = year(as_datetime(timestamp)))
validation <- validation %>% mutate(monthreview = month(as_datetime((timestamp))))

################################################
## Exploratory analysis
###############################################

### data description, basic statistics
nrow(edx)
edx %>% summarize(users =n_distinct(userId),
                  movies =n_distinct(movieId),
                  genres =n_distinct(genres))
edx %>% summarize(mean_rating=mean(rating), sd_rating=sd(rating))


### Histograms of reviews per movie and user
#bar plot: distribution of ratings
p1 <-edx %>% ggplot() +
    aes(rating) +
    geom_bar(col="black", fill="gray") + 
    ggtitle('Distribution of ratings')


#Histogram:  distribution number of reviews per movie
p2 <- edx %>% group_by(movieId) %>%
    summarize(n=n()) %>%  
    ggplot() + 
    aes(x= n)  +
    geom_histogram(bins=50, col="black", fill="gray") +
    xlab("Number of reviews per movie")

# Histogram: distribution of number of reviews per user
p3 <- edx %>% group_by(userId) %>%
    summarize(n=n()) %>%  
    ggplot() + 
    aes(x= n)  +
    geom_histogram(bins=50, col="black", fill="gray") +
    xlab("Number of reviews per user")

grid.arrange(p1, p2, p3, nrow=2)

#summary statistics for number of reviews per movie and per user
rbind(
    edx %>%group_by(movieId) %>%
        summarize(n=n())%>% 
        summarize(Min =min(n), Max =max(n), First_quantile=quantile(n, 0.25), 
                  Median=quantile(n, 0.5), Third_quantile= quantile(n,0.75)) %>%
        mutate(Variable="movieId") %>% select(Variable, everything()),
    
    
    edx %>%group_by(userId) %>%
        summarize(n=n())%>% 
        summarize(Min =min(n), Max =max(n), First_quantile=quantile(n, 0.25), 
                  Median=quantile(n, 0.5), Third_quantile= quantile(n,0.75)) %>%
        mutate(Variable="userId") %>% select(Variable, everything())
) 

### Find number of distinct genres
edx %>% 
    summarize(n_distinct(genres))

# Plot cummulative distribution of nummber of review per genre
edx %>% group_by(genres) %>%
    summarize(n=n()) %>% 
    arrange(n) %>%
    mutate(sum = 100*cumsum(n)/nrow(edx), gen =1:797 ) %>% 
    ggplot(aes(gen, sum)) + geom_point() +
    ylab("Percent of total number of reviews") +
    xlab("Genres")

#find cummulative distribution of number of reviews per genre
edx %>% group_by(genres) %>%
    summarize(n=n()) %>% 
    arrange(n) %>%
    mutate(sum = 100*cumsum(n)/nrow(edx), gen =1:797 ) %>%
    filter(sum>50) %>%
    summarize(s=n())

##############################
## Model Building
##############################

# Function that will evaluate RMSE 

RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}
#####################################
# Create partition of edx  set to
# create a train set and a test set
#####################################
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edxtrain <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in edx set
edxtest <- temp %>% 
    semi_join(edxtrain, by = "movieId") %>%
    semi_join(edxtrain, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, edxtest)
edxtrain <- rbind(edxtrain, removed)

rm(temp)



#####################################
## Model 1, only the averages       #
#####################################

# Find global average
# this will be our baseline
mu_hat <- edxtrain %>%
    summarize(m=mean(rating)) %>%
    .$m

#rmse
model1_rmse <- RMSE(mu_hat, edxtest$rating)
model1_rmse

##############################
# Model2. Add movie effects  #
##############################

# average ratings by movie, after taking general mean
movie_effect <- edxtrain %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu_hat))    

# Plot of distribution of  of movie effects
movie_effect %>% ggplot +
    aes(b_i) +
    geom_histogram(bins=15, color=mycolor, fill=myfill) +
    xlab("Movie effect")

#Predict on validation set

yhat <- left_join(edxtest, movie_effect, by="movieId") %>%
    mutate(yhat=b_i + mu_hat) %>%
    pull(yhat)

# Rmse of model 2.    
model2_rmse <- RMSE(yhat ,edxtest$rating) 
model2_rmse


# #################################
# Model 3. movie and user effects #
###################################


#Histogram of mean rating by user
edxtrain %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat))   %>% 
    ggplot() +
    aes(b_u) +
    geom_histogram(bins=15, color=mycolor, fill=myfill) +
    xlab("User effect")


#Model building. Estimate user effects
user_effect <- edxtrain %>%
    left_join(movie_effect, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - b_i))

yhat <- edxtest %>% 
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    mutate(prediction = mu_hat + b_i + b_u) %>%
    pull(prediction)

model3_rmse <- RMSE(yhat, edxtest$rating)
model3_rmse


#########################################################
# Model 4. Use reguarization on user and movie effect.  #
# add regularized genre effects                         #
#########################################################

#plots to motivate regularization
# Plots of mean rating per number of reviews

p4_1 <-edxtrain %>%
    sample_n(50000) %>%
    group_by(userId) %>%
    summarize(mean = mean(rating), n=n()) %>%
    ggplot(aes(n,mean)) +
    geom_jitter(alpha=0.2, width=0, height=0.5) +
    ggtitle("Mean rating vs number of reviews \n per user")

p4_2<-edxtrain %>%
    sample_n(50000) %>%
    group_by(movieId) %>%
    summarize(mean = mean(rating), n=n()) %>%
    ggplot(aes(n,mean)) +
    geom_jitter(alpha=0.2, width=0, height=0.5)+
    ggtitle("Mean rating vs number of reviews \n per movie")

p4_3 <-edxtrain %>%
    sample_n(50000) %>%
    group_by(genres) %>%
    summarize(mean = mean(rating), n=n()) %>%
    ggplot(aes(n,mean)) +
    geom_jitter(alpha=0.2, width=0, height=0.5) +
    ggtitle("Mean rating vs number of reviews \n per genre")

grid.arrange(p4_1, p4_2, p4_3, ncol=2)


# Use regularization
# Use test set (NOT VALIDATION) to find the optimal lambda


lambdas <- seq(0, 10, 0.25) #lammbdas to test

rmses <- sapply(lambdas, function(l){
    mu <- mean(edxtrain$rating)
    b_i <- edxtrain %>%
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- edxtrain %>%
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    b_g <- edxtrain %>%
        left_join(b_i, by="movieId") %>%
        left_join(b_u, by="userId") %>%
        group_by(genres) %>%
        summarize(b_g =sum(rating -mu - b_i -b_u)/(n()+l)) 
    predicted_ratings <-
        edxtest %>%
        left_join(b_i, by = "movieId") %>%
        left_join(b_u, by = "userId") %>%
        left_join(b_g, by="genres") %>%
        mutate(pred = mu + b_i + b_u + b_g) %>%
        pull(pred)
    return(RMSE(predicted_ratings, edxtest$rating))
})

# plot lambdas against the rmses obtained with the test set.
# Set the lambda to the one that minimizes the error
plot(lambdas, rmses)
l =lambdas[which.min(rmses)]

# Fit regularized model with lambda obtained, including genre effects
mu <- mean(edxtrain$rating)
b_i <- edxtrain %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- edxtrain %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
b_g <- edxtrain %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g =sum(rating -mu - b_i -b_u)/(n()+l)) 


# Find predictions on test set
predicted_ratings <-
    edxtest %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(prediction = mu + b_i + b_u +b_g) %>%
    pull(prediction)
    

# rmse on test set
model4rmse =RMSE(predicted_ratings, edxtest$rating)
model4rmse

                 



#Check out residuals to decide next step
# residuals/ YEAR
# We will check the residuals to see if there is a time effect
# fit model with  user regluarized movie (b_u)
# regularized  user effect (b_i), and genre effect (b_g)
# find predictions on train set
predictions <- edxtrain %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(prediction = mu + b_i + b_u +b_g) %>%
    pull(prediction)

#find redisuals on train set
edxtrain <- edxtrain %>% mutate(residuals = rating - predictions)

#plot residuales against year the movie came
set.seed(1)
edxtrain %>% sample_n(900000) %>%
    ggplot() +
    aes(year, residuals) +
    geom_smooth(alpha=.2)

#############################################
# Model 5, add time variables year. We fit 
#a spline smoothing to THE RESIDUALS and then
#add it to the predictions. So the residuals
# on the test set have to be obtained in order
# to fit the splines.
#############################################

predictionst <- edxtest %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(prediction = mu + b_i + b_u +b_g) %>%
    pull(prediction)

#find redisuals on test set
edxtest <- edxtest %>% mutate(residuals = rating - predictionst)

# fit a spline model on the rsiduals, to find time effects on both
# the year the movie came out and the year the rating was given

#Two loops are run in order to find the k parameter in the 
#spline model that mimimizes the error on the test set.

#values of k to test
ks <- expand.grid(k1=c( 12,14, 16,18), k2=c(8, 10, 12))

errors_spline1 <- vector(mode="numeric", length=nrow(ks))

#loop that tests values of k
for (i in (1:nrow(ks))){
    print(ks[i,1])
    print(ks[i,2])
    fit <- gam(residuals ~ s(year, bs="cr", k=ks[i,1]) +
                   s(timestamp, bs="cr", k=ks[i,2]), 
                   data=edxtrain)
    
    ehat <-predict(fit, edxtest)
    
    yhat <- edxtest %>% 
            left_join(b_i, by="movieId") %>%
            left_join(b_u, by="userId") %>%
            left_join(b_g, by="genres") %>%
            mutate(prediction = mu + b_i + b_u +b_g) %>%
            mutate(prediction2 = prediction + ehat) %>%
            pull(prediction2)
    
    modeltime <- RMSE(yhat, edxtest$rating)
    errors_spline1[i] <-modeltime
}


ks2 <- expand.grid(k1=c(15,16, 17), k2 = c(9,10,11))
errors_spline2 <- vector(mode="numeric", length=nrow(ks))

#loop that tests values of k
for (i in (1:nrow(ks))){
    print(ks[i,1])
    print(ks[i,2])
    fit <- gam(residuals ~ s(year, bs="cr", k=ks[i,1]) + s(timestamp, bs="cr", k=ks[i,2]), 
               data=edxtrain)
    
    ehat <-predict(fit, edxtest)
    
    
    yhat <- edxtest %>% 
        left_join(b_i, by="movieId") %>%
        left_join(b_u, by="userId") %>%
        left_join(b_g, by="genres") %>%
        mutate(prediction = mu + b_i + b_u +b_g) %>%
        mutate(prediction2 = prediction + ehat) %>%
        pull(prediction2)
    
    modeltime <- RMSE(yhat, edxtest$rating)
    errors_spline2[i] <-modeltime
}



#Fit a final model for the RESIDUALS with the correct values for K
spline_time <- gam(residuals ~ s(year, bs="cs", k=15) + s(timestamp, bs="cs", k=11), 
           data=edxtrain)

# fit model on the test set
ehat <-predict(spline_time, edxtest)

#find predicions of test set
yhat <- edxtest %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(prediction = mu + b_i + b_u +b_g) %>%
    mutate(prediction2 = prediction + ehat) %>%
    pull(prediction2)

# check rmse for new model in TEST set
model5rmse <- RMSE(yhat, edxtest$rating)





####################################################
### Full model on whole data and RMSE on VALIDATION
###################################################


# Fit regularized model with lambda obtained, including genre effects
l <- 5
mu_full <- mean(edx$rating)
b_i_full <- edx %>%
    group_by(movieId) %>%
    summarize(b_i_full = sum(rating - mu_full)/(n()+l))
b_u_full <- edx %>%
    left_join(b_i_full, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_full = sum(rating - b_i_full - mu_full)/(n()+l))
b_g_full <- edx %>%
    left_join(b_i_full, by="movieId") %>%
    left_join(b_u_full, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g_full =sum(rating -mu_full - b_i_full -b_u_full)/(n()+l)) 





#Find predictions to find residuals
predictions <- edx %>% 
    left_join(b_i_full, by="movieId") %>%
    left_join(b_u_full, by="userId") %>%
    left_join(b_g_full, by="genres") %>%
    mutate(prediction = mu_full + b_i_full + b_u_full +b_g_full) %>%
    pull(prediction)

edx <- edx %>% mutate(residuals = rating - predictions)


#Predictions and residuals on validation set for splines
predictionst <- validation %>% 
    left_join(b_i_full, by="movieId") %>%
    left_join(b_u_full, by="userId") %>%
    left_join(b_g_full, by="genres") %>%
    mutate(prediction = mu_full + b_i_full + b_u_full +b_g_full) %>%
    pull(prediction)

#find redisuals on train set
validation <- validation %>% mutate(residuals = rating - predictionst)

#Fit splines on time data on edx set (train set.)

fit <- gam(residuals ~ s(year, bs="cs", k=15) + s(timestamp, bs="cs", k=11), 
           data=edx)

#final rmse in VALIDATION set

ehat <-predict(fit, validation)


yhat <- validation %>% 
    left_join(b_i_full, by="movieId") %>%
    left_join(b_u_full, by="userId") %>%
    left_join(b_g_full, by="genres") %>%
    mutate(prediction = mu_full + b_i_full + b_u_full +b_g_full) %>%
    mutate(prediction2 = prediction + ehat) %>%
    pull(prediction2)

finalrmse <- RMSE(yhat, validation$rating)
finalrmse
