library(reshape2)

set.seed(123)
your_data <- data.frame(
  individual = rep(1:10, each = 5),
  timeframe = rep(1:5, times = 10),
  var1 = round(rnorm(50),2),
  var2 = round(rnorm(50),2),
  var3 = round(rnorm(50),2),
  dependent_variable = rbinom(n=10,size=1,prob=0.5)
)
your_data


#- ensure all records have same lenth


# Assuming your_data is in wide format, convert it to long format
#long_data <- melt(your_data, id.vars = c("individual", "timeframe", "dependent_variable"))
long_data <- your_data %>%
  pivot_longer(cols = starts_with("var"), names_to = "variable", values_to = "value")

long_data %>% str
long_data

# Reshape data for LSTM
n_individuals <- length(unique(long_data$individual))
n_timeframes <- length(unique(long_data$timeframe))
n_features <- ncol(your_data) - 3  # Number of predictor variables

n_individuals
n_timeframes
n_features

# Create a 3D array (tensor)
X <- array(
  data = matrix(long_data$value, nrow = n_individuals * n_timeframes * n_features),
  dim = c(n_individuals, n_timeframes, n_features)
)
X %>% dim
X
y <- matrix(long_data$dependent_variable, nrow = n_individuals, ncol = n_timeframes)
y

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 50, input_shape = c(n_timeframes, n_features)) %>%
  layer_dense(units = 5,activation='sigmoid')

# Compile the model
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam'
)

# Print the model summary
summary(model)

# Fit the model
model %>% fit(X, y, epochs = 10, batch_size = 5)

predict(model,X,verbose=1)
