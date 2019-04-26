library(keras)
library(kerasR)

prepare_data <- function() {
  mnist <<- dataset_mnist()
  # INSTRUCS: use the functions array_reshape to convert to 60k by 784
  x_train <<-  array_reshape(mnist$train$x, c(60000, 784));
  x_test <<-  array_reshape(mnist$test$x, c(10000, 784));
  
  # INSTRUCS: rescale between 0 and 1 by dividing by 255
  x_train <<- x_train / 255;
  x_test <<- x_test / 255; 

  # INSTRUCS: use to_categorical() to convert labels to 10 elems, 9 of them 0's, 1 of them a 1
  # NOTE: this essentially makes a vec(10) where the "1" is in the place of the number
  # before. i.e. if the number was 5 it is all 0's and a "1" in the 5th idx
  y_train <<- to_categorical(mnist$train$y, 10)
  y_test <<- to_categorical(mnist$test$y, 10)
}

create_MLP <- function() {
  model <- keras_model_sequential();
  model %>% 
    layer_dense(units = 256,  input_shape = c(784)) %>%
    layer_dense(units = 10, activation = 'softmax');
  
  # summary(model);
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy'));
  
  history_mlp <<- model %>% fit(
    x_train, 
    y_train, 
    batch_size = 128, 
    epochs = 12,
    verbose = 1,
    validation_split = 0.2);
  
  score_mlp <<- model %>% 
    evaluate(x_test, y_test,verbose = 0)
  
}

plot_history_mlp <- function() {
  plot(history_mlp);
}





