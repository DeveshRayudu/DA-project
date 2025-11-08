d1 = read.csv("/home/pranav/tmp/devesh_ip/DA-project/dataset/mobile_phone_screen_time_dataset.csv")

str(d1)
summary(d1$Productivity_Score)

## Linear Regression (~90% accuracy) Cost - 9.9391

cor(d1$Age, d1$Productivity_Score) # -0.001331965
cor(d1$Daily_Screen_Time_Hours, d1$Productivity_Score) # - 0.8497503
cor(d1$Gaming_Hours, d1$Productivity_Score) # 0.024

set.seed(1234)
n <- nrow(d1)
train_index <- sample(1:n, size = 0.8 * n)

train_data <- d1[train_index, ]
test_data <- d1[-train_index, ]

model <- lm(Productivity_Score ~ Daily_Screen_Time_Hours + Gaming_Hours, data = train_data)

predictions <- predict(model, newdata = test_data)

actual <- test_data$Productivity_Score
rmse <- sqrt(mean((predictions - actual)^2))
cat("RMSE:", round(rmse, 4), "\n") # Cost - 9.9391

## XGBoost (~90% accuracy) Cost - 9.9917

library(xgboost)

# Remove non-numeric columns (XGBoost only handles numeric input)
features <- d1[, sapply(d1, is.numeric)]
target <- features$Productivity_Score
features$Productivity_Score <- NULL

# Convert to matrix format
X <- as.matrix(features)
y <- target

set.seed(123)
n <- nrow(X)
split <- sample(1:n, size = 0.8 * n)
X_train <- X[train_index, ]
X_test  <- X[-train_index, ]
y_train <- y[train_index]
y_test  <- y[-train_index]

xgb_model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = 1000,              # number of boosting rounds
  objective = "reg:squarederror",
  eta = 0.01,                  # learning rate
  max_depth = 4,              # depth of each tree
  subsample = 0.8,            # sample fraction per tree
  colsample_bytree = 0.8,     # feature sampling
  verbose = 0                 # 0 = silent
)

predictions <- predict(xgb_model, X_test)
rmse <- sqrt(mean((predictions - y_test)^2))

cat("RMSE:", round(rmse, 4), "\n") # 9.9917
