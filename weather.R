# Load required libraries
install.packages(c("dplyr", "ggplot2", "GGally", "caret", "DiagrammeR", "DiagrammeRsvg", "rsvg"))
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Load dataset with a unique variable name
weather_data <- read.csv("weather_data.csv")

# Convert date column to Date format
weather_data$date <- as.Date(weather_data$date, format="%Y-%m-%d")

# Check for missing values
print(colSums(is.na(weather_data)))

# Handle missing values
weather_data <- weather_data %>% mutate(
  avg_temperature = ifelse(is.na(avg_temperature), mean(avg_temperature, na.rm = TRUE), avg_temperature),
  humidity = ifelse(is.na(humidity), mean(humidity, na.rm = TRUE), humidity),
  avg_wind_speed = ifelse(is.na(avg_wind_speed), mean(avg_wind_speed, na.rm = TRUE), avg_wind_speed),
  cloud_cover = ifelse(is.na(cloud_cover), mean(cloud_cover, na.rm = TRUE), cloud_cover)
)

# Encode categorical variables
weather_data$rain_or_not <- as.factor(weather_data$rain_or_not)

# Exploratory Data Analysis (EDA)

# Plot Histogram
ggplot(weather_data, aes(x=avg_temperature)) +
  geom_histogram(binwidth=2, fill="skyblue", color="black") +
  ggtitle("Temperature Distribution")

# Correlation Analysis
ggpairs(weather_data, columns = c("avg_temperature", "humidity", "avg_wind_speed", "cloud_cover", "pressure"))

# Split data into training & test sets
set.seed(42)
trainIndex <- createDataPartition(weather_data$rain_or_not, p=0.8, list=FALSE)
trainData <- weather_data[trainIndex,]
testData <- weather_data[-trainIndex,]

# Train Machine Learning Models
log_model <- train(rain_or_not ~ ., data=trainData, method="glm", family=binomial)
tree_model <- train(rain_or_not ~ ., data=trainData, method="rpart")
rf_model <- train(rain_or_not ~ ., data=trainData, method="rf")

# Model Evaluation
rf_pred <- predict(rf_model, testData)
print(confusionMatrix(rf_pred, testData$rain_or_not))

# Predict the Next 21 Days
future_predictions <- predict(rf_model, newdata=testData[1:21,])
print(future_predictions)

# Generate System Diagram
DiagrammeR("graph TD;
  A[IoT Sensors] -->|Raw Data| B[Data Processing];
  B -->|Stored Data| C[Database & Storage];
  C -->|Training & Prediction| D[Machine Learning Model];
  D -->|Predictions| E[API Layer];
  E -->|Visualization| F[Frontend Dashboard];")
