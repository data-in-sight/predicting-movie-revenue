#### Movie Revenue Prediction & Profit Maximization ####

source("DataAnalyticsFunctions.R")
data <- read.csv("~/Desktop/Team Project_Movie Revenue/AllMoviesDetailsCleaned 3.csv", sep = ";")
#write.csv(data$genres,"~/Desktop/Team Project_Movie Revenue/genredata.csv")
genredata <- read.csv("~/Desktop/Team Project_Movie Revenue/genredata.csv", sep = "|")
library(caret)

# for our convenience...
id <- data$id
budget <- data$budget
genres <- data$genres
imdb_id <- data$imdb_id
original_language <- data$original_language
original_title <-data$original_title
production_companies <- data$production_companies
production_countries <- data$production_countries
release_date <- data$release_date 
revenue <- data$revenue
runtime <- data$runtime
spoken_languages <- data$spoken_languages
title <- data$title
production_companies_number <- data$production_companies_number
production_countries_number <- data$production_countries_number
spoken_languages_number <-  data$spoken_languages_number

summary(data)
str(data)
#drop anything that is missing revenue or budget
data<-data[(data$revenue>=1000),]
nrow(data)
# 5685 of movies has revenue data
data<-data[(data$budget>=1000),]
nrow(data)
# 5685 of movies has both revenue and budget data

#creating_dummies
unique(spoken_languages) 
#total 76 unique values
# Borderline TOO MANY(?)
unique(original_language) 
# total 135 unique values
# Borderline TOO MANY(?)
summary(unique(genres))
# limited number of genres but too many combination of genres (7620)... 
# need to extract the main genre and create a new column, 
# and another column for the number of genres for each movie
summary(unique(production_companies)) 
# total 35990 unique values, 
# select top N top performers and create dummies
summary(unique(production_countries)) 
# total 235 unique values, 
# select top N top performers and create dummies

#genre dummies
all_unique_genres<-unique(genredata$Genre1)
genredf<- data.frame(matrix(ncol = 20, nrow = nrow(data)))
colnames(genredf) <- c('Movie genre',all_unique_genres[1:19])
genredf[,1]<-data$genres
for (i in 2:20){
  genredf[,i]<-ifelse (grepl( all_unique_genres[i-1], genredf[,1], fixed = TRUE),1,0)
}
genredf <- genredf[,2:ncol(genredf)]

#Production Company Dummies
library(dplyr)
Prod_comp_grp<-data %>% count(production_companies,sort=TRUE)
pc_dummy<-Prod_comp_grp[2:21,1]
pcdf<- data.frame(matrix(ncol = 20, nrow = nrow(data)))
colnames(pcdf) <- pc_dummy
for (i in 1:20){
  pcdf[,i]<-ifelse(data$production_companies==pc_dummy[i],1,0)
}

#spoken languages dummies
spokenlang_grp<-data %>% count(spoken_languages,sort=TRUE)
sl_dummy<-spokenlang_grp[c(1,3:17),1]
sldf<- data.frame(matrix(ncol = 16, nrow = nrow(data)))
colnames(sldf) <- sl_dummy
for (i in 1:16){
  sldf[,i]<-ifelse(data$spoken_languages==sl_dummy[i],1,0)
}

#Production Countries Dummies
Prod_count_grp<-data %>% count(production_countries,sort=TRUE)
pct_dummy<-Prod_count_grp[c(1:3, 5:16),1]
pctdf<- data.frame(matrix(ncol = 15, nrow = nrow(data)))
colnames(pctdf) <- pct_dummy
for (i in 1:15){
  pctdf[,i]<-ifelse(data$production_countries==pct_dummy[i],1,0)
}

#Original languages dummies
orig_lang_grp <-data %>% count(original_language,sort=TRUE)
ol_dummy<-orig_lang_grp[1:13,1]
oldf<- data.frame(matrix(ncol = 13, nrow = nrow(data)))
colnames(oldf) <- ol_dummy
for (i in 1:13){
  oldf[,i]<-ifelse(data$original_language==ol_dummy[i],1,0)
}


#redefining the data set with dummies and without unneccessary columns
data <- subset(data,select=-c(overview, popularity, status, tagline, vote_average, vote_count,
                              original_language, production_countries, spoken_languages, production_companies))
data <- cbind(data, genredf, pcdf, sldf, pctdf, oldf)
data <- data[complete.cases(data), ]
which(is.na(data))
nrow(data)

#xnrow=1:nrow(data)
#profit_lm<-lm(ytrain~xtrain)
#abs_prof<-abs(data$Profit_Perc)
#plot(xnrow, abs_prof)
#text(xnrow, abs_prof, round(xnrow))

#split data set to test and train
set.seed(20211009)
trainIndex <- createDataPartition(data$en, p = .8,
                                  list = FALSE,
                                  times = 1)
tdata <- data[ trainIndex,]
test <- data[-trainIndex,]

#let keep 80% of data for training
#test data

#NAIVE model (=mean)
mean(tdata$revenue)
  #85282867 for all movies
sqrt(mean((test$revenue -mean(tdata$revenue))^2))
  #186821359
  #

xtrain<-subset(tdata,select=-c(id, imdb_id, original_title, release_date, title, genres))
ytrain<-tdata$revenue

revLM<-lm(log(ytrain)~as.matrix(xtrain))
summary(revLM)
predtrainlm<-predict(revLM,xtrain)
predtestlm<-predict(revLM,test)
predtrainlm <- exp(predtrainlm)
predtrainlm
R2lm<-summary(revLM)$r.squared 
R2lm
#0.541373

# Random Forest
set.seed(101)
library(randomForest)

xtrain<-subset(tdata,select=-c(id, imdb_id, original_title, release_date, title, genres))
xtrain <- as.data.frame(xtrain)
colnames(xtrain) <- gsub(" ", ".", colnames(xtrain))
xtrain <- janitor::clean_names(xtrain)
rf<-randomForest(revenue~.,data=xtrain, ntree=500)
summary(rf)

xtest<-subset(test,select=-c(id, revenue, imdb_id, original_title, release_date, title, genres))
xtest <- as.data.frame(xtest)
colnames(test) <- gsub(" ", ".", colnames(test))
xtest <- janitor::clean_names(test)
xtest <- xtest %>% rename(none_1 = none_2,) #column name was not matching for somehow reason and threw me an error, so I renamed it

predtrainRF<-predict(rf, xtrain, type='response')
predtestRF<-predict(rf, xtest, type='response')

R2rf<-R2(predtrainRF,ytrain)
R2rf

R2rf<-R2(predtrainRF,ytrain)
R2rf
  #0.9088581
sqrt(mean((xtrain$revenue - predtrainRF)^2))
  #51130269

R2rf<-R2(predtestRF,xtest$revenue)
R2rf
# 0.6119733
sqrt(mean((test$revenue - predtestRF)^2))
  #106835697


####attempt for NN#####
library(keras)
model<- keras_model_sequential()

library(devtools)
devtools::install_github("rstudio/keras")
#### one needs to install Python 3.x via Anaconda
#### https://www.anaconda.com/download/
reticulate::install_miniconda()


data <- data[complete.cases(data),]

#file.edit('./.Renviron')
#http_proxy=http://myusename:password@proxy.server.com:port/
#https_proxy=http://myusename:password@proxy.server.com:port/
set.seed(20211009)
trainIndex <- createDataPartition(data$en, p = .8,
                                  list = FALSE,
                                  times = 1)
data <- data[ trainIndex,]
data.holdout <- data[-trainIndex,]


x_test<- model.matrix(Revenue ~ ., data=data.holdout)[,-1]
y_test<- data.holdout$Revenue


x_train<- model.matrix(Revenue ~ ., data=data)[,-1]
y_train<- data$Revenue

num.inputs <- ncol(x_test)


model <- keras_model_sequential() %>%
  layer_dense(units=16,activation="relu",input_shape = c(num.inputs)) %>%
  layer_dense(units=16,activation="relu") %>%
  layer_dense(units=16,activation="relu") %>%
  layer_dense(units=1,activation="linear")


summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 256, 
  validation_split = 0.5
)
results.NN1 <- model %>% evaluate(x_train,y_train)
results.NN1

results.NN1 <- model %>% evaluate(x_test,y_test)
results.NN1

pred.NN1 <- model%>% predict(x_test)
PerformanceMeasure(actual=y_test, prediction=pred.NN1, threshold=.5)

plot(history)

R2(pred.NN1, y_test)
rmse(y_test,pred.NN1)


