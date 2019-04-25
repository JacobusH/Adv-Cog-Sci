library(keras)
library(kerasR)

prepare_data <- function() {
  mnist <<-dataset_mnist()
}