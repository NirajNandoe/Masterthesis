#Load the necessary libraries
library(keras)
library(tensorflow)
library(dplyr)
library(kerastuneR) 
library(devtools)

#Load the dataset
fulldata <- read.csv("monthlyfull.csv")

fulldata <- fulldata %>%
  mutate(orig_row = row_number())

# Chronological 70/30 split per currency
train_idx <- fulldata %>%
  group_by(currency) %>%
  mutate(rownum = row_number(), n = n()) %>%
  filter(rownum <= floor(0.7 * n)) %>%
  ungroup() %>%
  pull(orig_row)
test_idx <- setdiff(seq_len(nrow(fulldata)), train_idx)

feature_cols <- fulldata %>%
  select(-currency, -orig_row) %>%
  select(where(is.numeric)) %>%
  colnames()

#Train test set creation
x_train_raw <- as.matrix(fulldata[train_idx, feature_cols])
x_test_raw  <- as.matrix(fulldata[test_idx, feature_cols])

# Normalize using training statistics
train_means <- apply(x_train_raw, 2, mean)
train_sds   <- apply(x_train_raw, 2, sd)
x_train_scaled <- scale(x_train_raw, center = train_means, scale = train_sds)
x_test_scaled  <- scale(x_test_raw,  center = train_means, scale = train_sds)

# Autoencoder model for tuning
build_autoencoder <- function(hp) {
  input_dim <- ncol(x_train_scaled)
  latent_dim <- hp$Int("latent_dim", min_value = 2L, max_value = 8L, step = 2L)
  noise_std <- hp$Choice("noise_std", values = c(0.0, 0.01, 0.05, 0.1))
  dropout_rate <- hp$Float("dropout_rate", min_value = 0.0, max_value = 0.5, step = 0.1)
  reg_strength <- hp$Choice("reg_strength", values = c(1e-3, 1e-4, 1e-5))
  
  inputs <- layer_input(shape = input_dim)
  x <- inputs %>%
    layer_gaussian_noise(stddev = noise_std) %>%
    layer_dense(units = 128, activation = "relu",
                kernel_regularizer = regularizer_l2(reg_strength)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = latent_dim, activation = "relu",
                kernel_regularizer = regularizer_l2(reg_strength), name = "latent") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 128, activation = "relu",
                kernel_regularizer = regularizer_l2(reg_strength)) %>%
    layer_dropout(rate = dropout_rate)
  outputs <- x %>%
    layer_dense(units = input_dim, activation = "linear")
  
  model <- keras_model(inputs, outputs)
  model %>% compile(optimizer = "adam", loss = "mse")
  return(model)
}

# Tuner setup
tuner <- RandomSearch(
  build_autoencoder,
  objective = 'val_loss',
  max_trials = 20,
  executions_per_trial = 1,
  directory = 'autoencoder_tuning',
  project_name = 'fx_autoencoder',
  overwrite = TRUE,
  seed = 382
)

earlystop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 10,
  restore_best_weights = TRUE
)

#Tuner run
tuner %>% fit_tuner(
  x = x_train_scaled,
  y = x_train_scaled,
  validation_data = list(x_test_scaled, x_test_scaled),
  epochs = 100,
  batch_size = 64,
  shuffle = FALSE, 
  callbacks = list(earlystop),
  verbose = 1
)

# Extract the best model
best_model <- get_best_models(tuner, num_models = 1)[[1]]

# Build encoder to extract latent factors
encoder <- keras_model(
  inputs = best_model$input,
  outputs = get_layer(best_model, "latent")$output
)

# extract latent variables over the full dataset
x_full_raw <- as.matrix(fulldata[, feature_cols])
x_full_scaled <- scale(x_full_raw, center = train_means, scale = train_sds)
latent_factors_full <- predict(encoder, x_full_scaled)
#Save the latent variables as a CSV file
write.csv(latent_factors_full, "latent_factors_full.csv", row.names = FALSE)
