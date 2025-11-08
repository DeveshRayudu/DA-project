library(rpart)

d1 = read.csv("D:/college/IP_proj/dataset/mobile_phone_screen_time_dataset.csv")
colSums((is.na(d1)))
View(d1)

str(d1)

numeric_data = d1[, sapply(d1, is.numeric)]
set.seed(123)  # for reproducibility
split = sample(1:nrow(d1), 0.8 * nrow(d1))

train_data = d1[split,]
test_data  = d1[-split,]

multi_linear = glm("Productivity_Score ~ Daily_Screen_Time_Hours",data = d1)
summary(multi_linear)

predictions = predict(multi_linear, newdata = test_data)

results = data.frame(
  Actual = test_data$Productivity_Score,
  Predicted = predictions
)
print(results)

mae = (1 - mean(abs(predictions - test_data$Productivity_Score) / nrow(test_data))) * 100
print(mae)

#decision tree
model_dt = rpart("Productivity_Score ~ Daily_Screen_Time_Hours", data = train_data) 
predictions_dt = predict(model_dt, newdata = test_data)
summary(predictions_dt)

results_dt = data.frame(
  Actual = test_data$Productivity_Score,
  Predicted = predictions_dt
)
print(results_dt)

