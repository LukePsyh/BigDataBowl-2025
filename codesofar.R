#load libraries
library(tidyverse)
library(keras)
#read in data
pitch_data <- read_csv("pitch_data.csv") %>%
  filter(is_swing==0)
unique(pitch_data$pitch_type)
#stuff
pitches <- data.frame(
  pitch_type = c("FF", "SL", "CH", "FT", "CB", "FS", "FC", "OT"),
  classification = c("fast", "breaking", "offspeed", "fast", "breaking", "fast", "fast", "offspeed"),
  stringsAsFactors = FALSE
)
pitch_data<-merge(pitch_data, pitches, by="pitch_type")
pitch_data$pitch_type <- NULL
#set random seed
set.seed(123)
#format data into numerics
for (i in 1:ncol(pitch_data)){
  if (is.character(pitch_data[[i]])){
    pitch_data[[i]] = as.numeric(as.factor(pitch_data[[i]]))
  }
}
max <- apply(pitch_data, 2, max)
min <- apply(pitch_data, 2, min)
scaled <- scale(pitch_data, center=min, scale=max-min)
#generate random indices
train_split <- 0.8
sample_indices <- sample(1:nrow(pitch_data), size = train_split * nrow(pitch_data))
#split into train and test
train <- scaled[sample_indices,]
test <- scaled[-sample_indices,]
#run network for plate location
{
train_x <- as.matrix(train[,c(17, 18)])
colnames(train_x) <- NULL
train_y <- as.matrix(train[,1])
#get test
test_x <- as.matrix(test[,c(17, 18)])
colnames(test_x) <- NULL
test_y <- as.matrix(test[,1])
#initialize starting model
model <- keras_model_sequential() %>%
  layer_dense(units = 64, input_shape = c(ncol(train_x)), activation = 'relu') %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate=0.1) %>%
  #sigmoid loss for binary clasiffication
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# fit model
history <- model %>% fit(
  train_x, train_y,
  epochs = 30,
  batch_size = 64
)
# get test loss
loss1<-model %>% evaluate(test_x, test_y)
}
print(loss1)
#run network for plate location and pitch classification
{
  train_x <- as.matrix(train[,c(17, 18, 23)])
  colnames(train_x) <- NULL
  train_y <- as.matrix(train[,1])
  #get test
  test_x <- as.matrix(test[,c(17, 18, 23)])
  colnames(test_x) <- NULL
  test_y <- as.matrix(test[,1])
  #initialize starting model
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, input_shape = c(ncol(train_x)), activation = 'relu') %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dropout(rate=0.1) %>%
    #sigmoid loss for binary clasiffication
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  
  # fit model
  history <- model %>% fit(
    train_x, train_y,
    epochs = 30,
    batch_size = 64
  )
  # get test loss
  loss2<-model %>% evaluate(test_x, test_y)
}
print(loss1)
print(loss2)
#loss went down, pitch matters?
#run network for plate location, pitch type, and pitcher handedness
{
  train_x <- as.matrix(train[,c(17, 18, 8, 23)])
  colnames(train_x) <- NULL
  train_y <- as.matrix(train[,1])
  #get test
  test_x <- as.matrix(test[,c(17, 18, 8, 23)])
  colnames(test_x) <- NULL
  test_y <- as.matrix(test[,1])
  #initialize starting model
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, input_shape = c(ncol(train_x)), activation = 'relu') %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dropout(rate=0.1) %>%
    #sigmoid loss for binary clasiffication
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  
  # fit model
  history <- model %>% fit(
    train_x, train_y,
    epochs = 30,
    batch_size = 64
  )
  # get test loss
  loss3<-model %>% evaluate(test_x, test_y)
}
print(loss1)
print(loss2)
print(loss3)


