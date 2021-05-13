##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


#
# set up some pseudo constants that are used throughout
#
# NB:  need to learn if R has proper constants
#
url <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
zip_file_name <- "ml-10m.zip"
zip_file_size <- 65566137
download_timeout <- 600 #seconds == 10 minutes
ratings_dat_name <- "ml-10M100K/ratings.dat"
ratings_dat_size <- 265105635
movies_dat_name <- "ml-10M100K/movies.dat"
movies_dat_size <- 522197
edx_rda <- "edx.rda"
edx_reshaped_rda <- "edx_reshaped.rda"
validation_rda <- "validation.rda"
max_genres_per_movie <- 0   # this will be filled in with max(str_count(edx$genres, "\\|"))
genre_title_vector <-   c(0)  # this will be filled in with as.character(1:max_genres_per_movie)
genre_names_rda<- "genre_names.rda"  # genre names found in train file.  Takes a while to do
genre_names <- NA                    # force to global scope
train_rda <- "train.rda"
train_reshaped_rda <- 'train_reshaped.rda'
test_rda <- "test.rda"
test_reshaped_rda <- 'test_reshaped.rda'
reshape_data <- FALSE     # if this flag is true, it forces a reshape of train and test
build_own_model <- TRUE   # build my own models
#
# functions written for this project
#
#
# function to reshape the data set by adding/deleting columns
# it must be executed on both the train and test data set
# for efficiency purposes, it saves the retrained datasets
# change the time stamp to a full date since movie preferences
# may be related to seasons of the year.  For example "summer movies"
# "Christmas romances", etc
# 
# timestamp becomes year, month, day_of_month, day_of_week
#
# the year of the movie's release is extracted from the title
#
reshape <- function (dataset) {     
  print(paste("inside reshape nrow=", nrow(dataset), sep=" "))
  print(ls())
  print(head(dataset, 3))
  print(genre_names)
  tmp <- dataset %>%  
    mutate(review_date = as.Date(as.POSIXct(as.numeric(timestamp), origin="1970-01-1"))) %>%
    mutate(#rating = as.factor(rating),
	   review_year = year(review_date), 
           review_month = month(review_date), 
           review_day = day(review_date),
           review_week_day = wday(review_date)) %>%
    select(-timestamp) %>%
    extract(title, c("short_title", "release_year"), regex="(^.*) \\((\\d\\d\\d\\d)\\)") %>%
    mutate(release_year = as.numeric(release_year)) %>% 
    mutate(span_years = as.numeric(review_year) - as.numeric(release_year))
  print("date extracted from title and timestamp split into smaller pieces of data");
  rm(dataset)
  print("removed original dataset to free up memory")
  print(head(tmp, 3))
  tmp
}

RMSE <- function(true_ratings, predicted_ratings){
	sqrt(mean((true_ratings - predicted_ratings)^2))
}

#
# begin executable flow
#

#
# this first section of the code merely downloads the data if required.
# there is a sequence of if statements to download zip file if necessary
# and then unzip it if necessary
#
# this just speeds up development
#

#
# test to see if the file exists, if it does not exist, download it
# OR if the file is not the correct size, download it
#
# NB:  short circuited logical operator
#
if ((!file.exists(zip_file_name) || 
    (file.info(zip_file_name)$size) != zip_file_size)  ) {
    options(timeout = download_timeout)
    download.file(url, zip_file_name)
}

#
# check to see if the entire fire was downloaded.  If not, give
# manual instructions to the operator on what to do
#
if ( !file.exists(zip_file_name) || 
     (file.info(zip_file_name)$size != zip_file_size) ) {
  print("The file was not properly downloaded.  I suggest you")
  print("download it through your browser.  Point your browser to ")
  print(url)
  print("and download it to the same directory where you started rstudio")
  stop()
}

print("zip file download ok")

#
# if the ratings file does not exist OR is not the right size OR
# if the movies file does not exist OR is not the right size
# then unzip the file (it obviously exists at this point or the
# script would have stopped
#
if ( !file.exists(ratings_dat_name) || 
     (file.info(ratings_dat_name)$size != ratings_dat_size) ||
     !file.exists(movies_dat_name) ||
     (file.info(movies_dat_name)$size != movies_dat_size) ) {
  unzip(zip_file_name)
}

#
# check to see if they have unzipped ok and are the correct size
#
if ( !file.exists(ratings_dat_name) || 
     (file.info(ratings_dat_name)$size != ratings_dat_size) ||
     !file.exists(movies_dat_name) ||
     (file.info(movies_dat_name)$size != movies_dat_size) ) {
  print("The files did not unzip properly.")
  print("Do you have space on your workstation?")
  print("Or are there permissions issues?")
  stop()
}

print("data files unzipped ok")

#
# the true part of the if statement is the original code 
# from edx website
#
if ( !file.exists(edx_rda) || !file.exists(validation_rda) ) {
  print("building the edx and validation data frames")
  #
  # read ratings - this is 
  ratings <- fread(text = gsub("::", "\t", readLines(ratings_dat_name)),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  
  movies <- str_split_fixed(readLines(movies_dat_name), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  
  # if using R 4.0 or later:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(ratings, movies, test_index, temp, movielens, removed)
  
  print(paste("edx nrow ", nrow(edx), sep=" "))
  print(paste("validation nrow ", nrow(validation), sep=" "))
  save(edx, file = edx_rda)
  save(validation,file = validation_rda)
  print("saved edx.rda and validation.rda")
} 

#
# this calculates the maximum number of genres listed in the genre file
# the number of pipe symbols plus one.
#
# if the validation file has more than this, this will be an issue
#
load(edx_rda)
max_genres_per_movie <- max(str_count(edx$genres, "\\|")) + 1
print(max_genres_per_movie)
genre_title_vector <-   as.character(1:max_genres_per_movie)

#
# if the training and test data have not been split off, then
# do so now.  50/50 split
#
if ( !file.exists(train_rda) || !file.exists(test_rda) ) {
	load(edx_rda)
	#
	# break the edx file into a train and test set
	#
	set.seed(1, sample.kind="Rounding")
	#
	#
	test_index <- createDataPartition(edx$rating, times = 1, p = 0.25, list = FALSE)
	train <- edx[-test_index,]
	test <- edx[test_index,]
	print(paste("edx has ", nrow(edx), sep=" "))
	print(paste("train has ", nrow(train), sep=" "))
	print(paste("test has ", nrow(test), sep=" "))
	save(train, file=train_rda)
	save(test, file=test_rda)
	rm(edx, train, test)			# clean up memory
} 


#
# now find the exising genres names in the train file.  Again if the valiation
# file has different genres, this will be a problem
#
if ( !file.exists(genre_names_rda) ) {
  	#
  	# need to free up some memory so get rid of the validation and test set
  	# then recreate it after wards
  	#
  	print("extracting genre names from test data")
	load(edx_rda)
  	genre_names <- edx %>%  select(genres) %>% unique %>%
    		separate(genres, genre_title_vector, sep="\\|") %>%
    		gather(to_remove, genre, genre_title_vector) %>% filter(!is.na(genre)) %>%
    		select(-to_remove) %>% unique %>% filter( !grepl("no genres listed", genre) )
    	# change from data.frame to vector
	# and ignore the name with the parenthesis in it
	#
	print(genre_names)
    	genre_names <- str_replace(genre_names$genre, "-", "_")
    	print(genre_names)
  	save(genre_names, file = genre_names_rda)
	rm(edx)
  	print("saved genre_names")
} else {
	load(genre_names_rda)
	print("loaded genre names")
}

rm(edx)
print(genre_names)


#
# reshape the edx data when necessary
#
if ( reshape_data || !file.exists(edx_reshaped_rda)) {
   print("reshaping edx data" )
   load(edx_rda)
   edx_reshaped <- reshape(edx)
   rm(edx)
   head(edx_reshaped, 10)
   save(edx_reshaped, file = edx_reshaped_rda)
   rm(edx_reshaped)
   print(paste("reshaped edx data saved on", edx_reshaped_rda, sep=" "))
}


#
#
#
# reshape the training data when necessary
#
if ( reshape_data || !file.exists(train_reshaped_rda)) {
   print("reshaping training data" )
   #rm(test, edx)
   load(train_rda)
   train_reshaped <- reshape(train)
   head(train_reshaped, 5)
   save(train_reshaped, file = train_reshaped_rda)
   rm(train, train_reshaped)
   print(paste("reshaped training data saved on", train_reshaped_rda, sep=" "))
} 

#
# reshape the test data when necessary
#
if ( reshape_data || !file.exists(test_reshaped_rda)) {
  print("reshaping test data" )
  load(test_rda)
  test_reshaped <- reshape(test)
  save(test_reshaped, file = test_reshaped_rda)
  rm(test, test_reshaped)
  print(paste("reshaped test data saved on", test_reshaped_rda, sep=" "))
} 


if ( build_own_model ) {
	print("building own model")
	print("loading train")
	load(train_rda)
	print("loading test")
	load(test_rda)

	#
	# this just determines the average of all ratings
	# and uses that value as the prediction
	#
	print("Average rating (naive approach)")
	overall_average <- mean(train$rating)
	print(RMSE(test$rating, overall_average))

	#
	# this uses the averages per movie and uses that
	# as the prediction
	#
	print("User Averages (User Effect)")
	user_averages <- train %>% group_by(userId) %>%
		summarize(userId = first(userId),
			  user_average = mean(rating))
	model <- test %>% inner_join(user_averages, by="userId")
	print(RMSE(test$rating, model$user_average))
	
	#
	# this uses the averages per movie and uses that
	# as the prediction
	#
	print("Movie Averages (Movie Effect)")
	movie_averages <- train %>% group_by(movieId) %>%
		summarize(movieId = first(movieId),
			  movie_average = mean(rating))
	model <- test %>% inner_join(movie_averages, by="movieId")
	print(RMSE(model$rating, model$movie_average))

	#
	# this averages the user average and the movie average
	#
	print("Average of Averages (Average Effect)")
	model <- test %>% inner_join(user_averages, by="userId") %>%
		inner_join(movie_averages, by="movieId") %>%
		mutate(prediction = (movie_average + user_average)/2)
	print(RMSE(model$rating, model$prediction))


	#
	# Include Overall average only once
	#
	print("Include Overall Averages Only Once")
	user_averages <- train %>% group_by(userId) %>%
                 summarize(userId = first(userId),
                 user_average = mean(rating - overall_average))
	movie_averages <- train %>% group_by(movieId) %>%
                 summarize(movieId = first(movieId),
                 movie_average = mean(rating - overall_average))
	model <- test %>% inner_join(user_averages, by="userId") %>%
                 inner_join(movie_averages, by="movieId") %>%
                 mutate(prediction = movie_average + user_average + overall_average)
	print(RMSE(model$rating, model$prediction))

	#
	# Remove User Average from Movie Average
	#
	print("Remove user average from movie average")
	user_averages <- train %>% group_by(userId) %>%
			summarize(userId = first(userId),
			user_average = mean(rating - overall_average))
	movie_averages <- train %>% group_by(movieId) %>%
			inner_join(user_averages, by="userId") %>%
			summarize(movieId = first(movieId),
			movie_average = mean(rating - overall_average - user_average ))
	model <- test %>% inner_join(user_averages, by="userId") %>%
			inner_join(movie_averages, by="movieId") %>%
			mutate(prediction = movie_average + user_average + overall_average)
	print(RMSE(model$rating, model$prediction))

	#
	# Remove Movie Average from User Average
	#
	print("remove movie average from user average")
	movie_averages <- train %>% group_by(movieId) %>%
			summarize(movieId = first(movieId),
			movie_average = mean(rating - overall_average))
	user_averages <- train %>% group_by(userId) %>%
			inner_join(movie_averages, by="movieId") %>%
			summarize(userId = first(userId),
			user_average = mean(rating - overall_average - movie_average ))
	model <- test %>% inner_join(user_averages, by="userId") %>%
			inner_join(movie_averages, by="movieId") %>%
			mutate(prediction = movie_average + user_average + overall_average)
	print(RMSE(model$rating, model$prediction))

	#
	# optimise qty_constant
	#
	print("optimising the qty_constant (lambda)")
	print("NB:  This will take some time")
	#
	# NB:  this range was shorted to speed up evaluation
	#      time for the code reviewer.  I originally ran
	#      it iwht (1, 10, .1) to find the minimum
	#
	qty_constants <- seq(3, 5, .1)
	rmses <- sapply(qty_constants, function(qty_constant) {
		movie_averages <- train %>% group_by(movieId) %>%
				summarize(movieId = first(movieId), movie_review_count = n(),
				movie_average = sum(rating - overall_average)/
					(movie_review_count + qty_constant))
		user_averages <- train %>% group_by(userId) %>%
				inner_join(movie_averages, by="movieId") %>%
				summarize(userId = first(userId), user_review_count = n(),
				user_average = sum(rating - overall_average - movie_average )/
					(user_review_count + qty_constant))
		model <- test %>% inner_join(user_averages, by="userId") %>%
				inner_join(movie_averages, by="movieId") %>%
				mutate(prediction = movie_average + user_average + overall_average)
		rmse <- RMSE(model$rating, model$prediction)
	})

	#
	# pick up minimum value
	#
	print("qty_constant (lamda) is")
	qty_constant <- qty_constants[which.min(rmses)]
	print(qty_constant)

	#
	# Cater for Users and Movies with few reviews
	#
	print("Compensate for Users and Movies with few reviews")
	movie_averages <- train %>% group_by(movieId) %>%
			summarize(movieId = first(movieId), movie_review_count = n(),
			movie_average = sum(rating - overall_average)/
			(movie_review_count + qty_constant))
	user_averages <- train %>% group_by(userId) %>%
			inner_join(movie_averages, by="movieId") %>%
			summarize(userId = first(userId), user_review_count = n(),
			user_average = sum(rating - overall_average - movie_average)/
			(user_review_count + qty_constant))
	model <- test %>% inner_join(user_averages, by="userId") %>%
			inner_join(movie_averages, by="movieId") %>%
			mutate(prediction = movie_average + user_average + overall_average)
	print(RMSE(model$rating, model$prediction))

	#
	# rebuild model with entire edx set (excluding validation set)
	#
	print("rebuilding with edx set (excluding validation set)")
	rm(train, test)
	load(edx_rda)
	movie_averages <- edx %>% group_by(movieId) %>%
				summarize(movieId = first(movieId), movie_review_count = n(),
					movie_average = sum(rating - overall_average)/
						(movie_review_count + qty_constant))
	user_averages <- edx %>% group_by(userId) %>%
				inner_join(movie_averages, by="movieId") %>%
				summarize(userId = first(userId), user_review_count = n(),
					user_average = sum(rating - overall_average - movie_average )/
						(user_review_count + qty_constant))
	#
	# clean up memory
	#
	rm(edx)

	#
	# test with validation set
	#
	load(validation_rda)
	print("final RMSE with validation set")
	model <- validation %>% inner_join(user_averages, by="userId") %>%
			inner_join(movie_averages, by="movieId") %>%
			mutate(prediction = movie_average + user_average + overall_average)
	print(RMSE(model$rating, model$prediction))
}


