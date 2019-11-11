if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")



# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx %>% as_tibble()

edx %>% summarize(
n_users=n_distinct(userId),# unique users from train
n_movies=n_distinct(movieId),# unique movies from train
min_rating=min(rating),  # the lowest rating 
max_rating=max(rating) # the highest rating
)

keep <- edx %>% 
  count(movieId) %>% 
  top_n(5, n) %>% 
  .$movieId

tab <- edx %>% 
filter(movieId%in%keep) %>% 
filter(userId %in% c(13:20)) %>% 
select(userId, title, rating) %>% 
mutate(title = str_remove(title, ", The"),title = str_remove(title, ":.*")) %>% spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(edx$userId), 100)
#rafalib::mypar()
edx%>% filter(userId %in% users) %>% 
select(userId, movieId, rating) %>%
mutate(rating = 1) %>%
spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
as.matrix() %>% t(.) %>%
image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# plot count rating by movie
edx %>% 
count(movieId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "blue") + 
scale_x_log10() + 
ggtitle("Movies")

edx %>% 
count(userId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "orange") + 
scale_x_log10() + 
ggtitle("Users")

rm(tab, keep, users)

mu <- mean(edx$rating) # mean rating
mu

RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

naive_rmse <- RMSE(edx$rating, mu) # root mean square error
naive_rmse

rmse_results <- tibble(method = "Normal average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# create a results table with this and prior approach
model_1_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model on test set",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "orange")


user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# create a results table with this and prior approaches
model_2_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and User Effects Model on test set",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

lambda <- seq(0, 10, 0.25)

rmses <- sapply(lambda, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(edx$rating, predicted_ratings))
})

qplot(lambda, rmses)  

# lamda having minmal RMSE
lambda <- lambda[which.min(rmses)]
lambda

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# compute user effect with regularization on train set
b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# # compute predicted values on test set 
predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

# create a results table with this and prior approaches
model_3_rmse <- RMSE(edx$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie and User Effect Model on test set",  
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

# compute movie effect without regularization on train set
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# compute user effect without regularization on train set
b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - b_i - mu))

# compute residuals on train set
train <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# compute residuals on test set
test <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.r-project.org")

train_data <- data_memory(user_index = train$userId, item_index = train$movieId, 
                          rating = train$res, index1 = T)
  
test_data <- data_memory(user_index = test$userId, item_index = test$movieId, index1 = T)
  
# create a model object
recommender <- Reco()

set.seed(1) 
suppressWarnings(recommender$train(train_data, opts = c(dim = 30, costp_l1 = 0,
                                                        costp_l2 = 0.01, costq_l1 = 0,
                                                        costq_l2 = 0.1, lrate = 0.05,
                                                        verbose = FALSE)))

predicted_ratings <- recommender$predict(test_data, out_memory()) + mu + test$b_i + test$b_u 

ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

# floor rating at 0.50  
ind <- which(predicted_ratings < 0.5)
predicted_ratings[ind] <- 0.5
  
# create a results table with this approach
model_4_rmse <- RMSE(test$rating, predicted_ratings)
  
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and User + Matrix Fact. on test set",  
                                   RMSE = model_4_rmse))
rmse_results %>% knitr::kable()

# compute movies effect without regularization on edx set
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# compute users effect without regularization on edx set
b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - b_i - mu))

# compute residuals on edx set
edx <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# compute residuals on validation set
validation <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# Using recosystem 
# create data saved on disk in 3 columns with no headers

edx_data <- data_memory(user_index = edx$userId, item_index = edx$movieId, 
                          rating = edx$res, index1 = T)
  
validation_data <- data_memory(user_index = validation$userId, item_index = validation$movieId, index1 = T)

# create a model object  
recommender <- Reco()

# Train the model by calling the `$train()` method
# some parameters coming from the result of `$tune()`
# This is a randomized algorithm
set.seed(1)

suppressWarnings(recommender$train(edx_data, opts = c(dim = 30, costp_l1 = 0,
                                                        costp_l2 = 0.01, costq_l1 = 0,
                                                        costq_l2 = 0.1, lrate = 0.05,
                                                        verbose = FALSE)))

# use the `$predict()` method to compute predicted values
# return predicted values in memory

predicted_ratings <- recommender$predict(validation_data, out_memory()) + mu + 
                       validation$b_i + validation$b_u 

# ceiling rating at 5  
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

# floor rating at 5  
ind <- which(predicted_ratings < 0.5)
predicted_ratings[ind] <- 0.5
  
# create a results table with this and prior approaches
model_5_rmse <- RMSE(validation$rating, predicted_ratings)
  
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and User effects and Matrix Fact. on validation set",  
                                   RMSE = model_5_rmse))
rmse_results %>% knitr::kable()
