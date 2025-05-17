# DEFINE DEFAULTS
FLAGS <- tfruns::flags(
  flag_integer("layers", default = 3),
  flag_integer("units", default = 10),
  flag_numeric("learning_rate", default = 0.01),
  flag_numeric("dropout", default = 0.1)
)

# CREATE INPUT AND FIRST LAYER
network <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units, activation = "relu", input_shape = c(ncol(train.x))) %>%
  layer_dropout(rate = FLAGS$dropout)

# CREATE MORE HIDDEN LAYERS
for (u in seq_len(FLAGS$layers - 1)) {
  network %>% 
    layer_dense(units = FLAGS$units, activation = "relu") %>%
    layer_dropout(rate = FLAGS$dropout)
}

# CREATE OUTPUT LAYER
network %>% layer_dense(units = 2, activation = "softmax")

# COMPILE MODEL
network <- network %>% keras::compile(
  optimizer = optimizer_rmsprop(FLAGS$learning_rate),
  loss = "binary_crossentropy",
  metrics = c('accuracy', 'AUC'))

# TRAIN MODEL
history <- network %>% 
  keras::fit(train.x, train.y, epochs = 10, validation_data = list(cv.x, cv.y))
