#install.packages("igraph")
library(igraph)
#install.packages("tidyverse")
library(tidyverse)

#install.packages("e1071")
library(e1071)
#install.packages("GGally")
library(GGally)

library(sna)
library(psych)

# loading the data into dataframe
Obesity <- read.csv("~/Documents/GWU/Sem 2/Intro to Big Data/Project/Project 2/Project 2/ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors = TRUE)

# Functions to better understand our dataset
summary(Obesity)
str(Obesity)
describe(Obesity)

# Creating a function to convert factors to numeric based on their levels
convert_factor_to_numeric <- function(data_frame, column_name, levels_order) {
  factor_column <- factor(data_frame[[column_name]], levels = levels_order)
  numeric_column <- as.numeric(factor_column)
  data_frame[[column_name]] <- numeric_column
  return(data_frame)
}

Obesity <- convert_factor_to_numeric(Obesity, "Gender", c("Male", "Female"))
Obesity <- convert_factor_to_numeric(Obesity, "family_history_with_overweight", c("no", "yes"))
Obesity <- convert_factor_to_numeric(Obesity, "FAVC", c("no", "yes"))
Obesity <- convert_factor_to_numeric(Obesity, "CAEC", c("no", "Sometimes", "Frequently", "Always"))
Obesity <- convert_factor_to_numeric(Obesity, "SMOKE", c("no", "yes"))
Obesity <- convert_factor_to_numeric(Obesity, "SCC", c("no", "yes"))
Obesity <- convert_factor_to_numeric(Obesity, "CALC", c("no", "Sometimes", "Frequently", "Always"))
Obesity <- convert_factor_to_numeric(Obesity, "MTRANS", c("Automobile", "Motorbike", "Bike", "Public_Transportation", "Walking"))

# converting target variable 'NObeyesdad' to numeric
Obesity <- convert_factor_to_numeric(Obesity, "NObeyesdad", c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

str(Obesity)


# function to plot obesity levels against various factors
plot_obesity_levels <- function(x, y_label) {
  plot(x, Obesity$NObeyesdad, main = paste("Scatterplot of", y_label, "vs Obesity"), xlab = y_label, ylab = "Obesity Levels")
}

plot_obesity_levels(Obesity$TUE, "Time spent on Tech")
plot_obesity_levels(Obesity$CH2O, "Water Consumption")
plot_obesity_levels(Obesity$FCVC, "Eating Vegetables")
plot_obesity_levels(Obesity$FAVC, "Eating High Caloric Food")
plot_obesity_levels(Obesity$NCP, "Number of Main Meals")
plot_obesity_levels(Obesity$SMOKE, "Smoking")
plot_obesity_levels(Obesity$Height, "Height")
plot_obesity_levels(Obesity$SCC, "Monitoring Calories")
plot_obesity_levels(Obesity$FAF, "Physical Activity Frequency")
plot_obesity_levels(Obesity$CALC, "Alcohol Consumption")
plot_obesity_levels(Obesity$MTRANS, "Mode of Transport")
plot_obesity_levels(Obesity$Age, "Age")
plot_obesity_levels(Obesity$Gender, "Gender")
plot_obesity_levels(Obesity$Weight, "Weight")
plot_obesity_levels(Obesity$CAEC, "Food Between Main Meals")
plot_obesity_levels(Obesity$family_history_with_overweight, "Family History with Overweight")

cor_matrix <- cor(Obesity[, sapply(Obesity, is.numeric)], use = "complete.obs")
print(cor_matrix)

if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)

# correlation matrix
corrplot(cor_matrix, method = "circle")

if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
library(GGally)

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

obesity_data_normalized <- as.data.frame(lapply(Obesity, normalize))

# Display the first few rows of the normalized dataset
head(obesity_data_normalized)

# clustering and plotting
perform_clustering <- function(data, centers) {
  cluster_result <- kmeans(data, centers = centers)
  print(cluster_result)
  factoextra::fviz_cluster(cluster_result, data)
}

#install.packages("factoextra")
library(factoextra)

test_obesity <- Obesity[, c("Gender", "Age", "Height", "Weight", "family_history_with_overweight", "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC", "FAF", "TUE", "CALC", "MTRANS", "NObeyesdad")]

perform_clustering <- function(data, centers) {
  numeric_data <- data[, sapply(data, is.numeric)]
  cluster_result <- kmeans(numeric_data, centers = centers, nstart = 25)
  print(cluster_result)
  fviz_cluster(cluster_result, data = numeric_data, geom = "point", stand = FALSE, ellipse.type = "convex", palette = "jco") +
    ggtitle(paste("K-Means Clustering with", centers, "Centers"))
}
perform_clustering(test_obesity, 2)
perform_clustering(test_obesity, 3)
perform_clustering(test_obesity, 4)
perform_clustering(test_obesity, 5)
perform_clustering(test_obesity, 6)

# Subset data based on specific conditions
subset_with_history <- Obesity[Obesity$family_history_with_overweight == 2, ]
subset_without_history <- Obesity[Obesity$family_history_with_overweight == 1, ]

if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)
set.seed(123) # For reproducibility
wcss <- sapply(1:10, function(k) {
  kmeans(test_obesity[, sapply(test_obesity, is.numeric)], centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares", main = "Elbow Method")

# optimal number of clusters
fviz_nbclust(test_obesity[, sapply(test_obesity, is.numeric)], kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) +
  labs(subtitle = "Elbow Method")

#KNN
set.seed(42)

# Splitting the dataset into training and testing sets (70% training, 30% testing)
total_rows <- nrow(test_obesity)
train_indices <- sample(total_rows, size = 0.7 * total_rows)

train_data <- test_obesity[train_indices, ]
test_data <- test_obesity[-train_indices, ]

# Display the size of training and testing sets
cat("Training set rows:", nrow(train_data), "\nTesting set rows:", nrow(test_data), "\n")

# Determine optimal cluster number with K-Means (assuming k=5 from previous analysis)
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)

optimal_k <- 5
kmeans_result <- kmeans(train_data, centers = optimal_k)

# Attaching cluster labels to training data as a preparation step for KNN
train_data$Cluster <- kmeans_result$cluster

# class package 
if (!requireNamespace("class", quietly = TRUE)) install.packages("class")
library(class)

train_labels <- train_data$Cluster
train_features <- train_data[, -ncol(train_data)]
test_features <- test_data[, -ncol(test_data)]

feature_columns <- colnames(test_obesity)[1:(ncol(test_obesity)-1)] # Adjust based on your dataset

# Splitting datasets using the identified features, ensuring both sets have identical features
train_data_features <- test_obesity[train_indices, feature_columns]
test_data_features <- test_obesity[-train_indices, feature_columns]

# Verify dimensions
cat("Features in Training Data:", ncol(train_data_features), "\n")
cat("Features in Testing Data:", ncol(test_data_features), "\n")

# Proceed with clustering to get labels for KNN if not already done
train_labels <- kmeans(train_data_features, centers = optimal_k)$cluster
predicted_labels <- knn(train = train_data_features, test = test_data_features, cl = train_labels, k = 5)

refined_kmeans_result <- kmeans(test_features, centers = optimal_k)
test_data$RefinedCluster <- refined_kmeans_result$cluster

# Comparing KNN-predicted clusters vs. Refined clusters
cat("First 20 Predicted Clusters:", head(test_data$PredictedCluster, 20), "\n")
cat("First 20 Refined Clusters:", head(test_data$RefinedCluster, 20), "\n")

# Install and load 'caret' for classification performance metrics
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
library(caret)
test_data$PredictedCluster <- as.numeric(predicted_labels)
# Confusion Matrix

unique(test_data$PredictedCluster)
unique(test_data$RefinedCluster)

confusionMatrix(as.factor(test_data$PredictedCluster), as.factor(test_data$RefinedCluster))

# linear modeling
if (!require("gmodels")) install.packages("gmodels", dependencies = TRUE)
library(gmodels)

#install.packages(c("tidyverse", "broom"))
library(tidyverse)
library(broom)

set.seed(123) # For reproducibility
split <- sample.int(n = nrow(Obesity), size = floor(.7*nrow(Obesity)), replace = FALSE)
train <- Obesity[split, ]
test <- Obesity[-split, ]

lm_model <- glm(NObeyesdad ~ NCP + Weight + family_history_with_overweight + MTRANS + CALC + FAVC + FAF + SCC, data = train, family = gaussian())

summary(lm_model)

anova_lm <- anova(lm_model, test = "Chisq")
print(anova_lm)

predictions <- predict(lm_model, newdata = test, type = "response")

# Summary of predictions
summary(predictions)

conf_intervals <- confint(lm_model)
test <- test %>%
  mutate(predicted_NObeyesdad = predictions)
head(test)

if (!require("ggfortify")) install.packages("ggfortify", dependencies = TRUE)
library(ggfortify)

# Residuals vs Fitted plot
autoplot(lm_model, which = 1)

# Normal Q-Q Plot
autoplot(lm_model, which = 2)

# Scale-Location Plot
plot(lm_model, which = 3)

# Cross Table
crosstable = CrossTable(predictions$cluster,train_labels$cluster,prop.chisq=TRUE)