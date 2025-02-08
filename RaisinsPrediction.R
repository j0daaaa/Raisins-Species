#UTS Data Mining - R Code
#Jonathan David (01112210010)

#Kumpulan library yang digunakan
library(tidyverse)
library(magrittr)
library(Metrics)
library(ModelMetrics)
library(ggcorrplot)
library(plotly)
library(corrplot)
library(gridExtra)
library(ggExtra)
library(caret)
library(MLmetrics)
library(gplots)

set.seed(1) #supaya prediksi stabil, tidak berganti"

########## Data Loading ##########
df <- read.csv("Raisin_Dataset.csv")

########## Data Descriptive ##########
dim(df)     #Dimension/ size of the data
head(df)    #First 5 Data
names(df)   #Feature Names
str(df)     #Data Structure

#Data type for each features
Type <- sapply(df, class)
data.frame(Type)

summary(df) #Summary Data
# we can see that Feature Class is the target (y variable)
# and the feature 1 to 7 is the X data

#Kecimen data
Kecimen <- df[df$Class == "Kecimen", ]
head(Kecimen)
#Besni data
Besni <- df[df$Class == "Besni", ]
head(Besni)

# The data also doesn't have any categorical data, we can process the data immediately
any(is.na(df)) #Null data check
#FALSE = the data has no null data

# Duplicate data check
df = distinct(df) #There is no duplicate data

########## Outlier Handling ##########
### Feature Area ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(Area)) +
  geom_density(col = "red")
p2 <- df %>% ggplot(aes(x = Area)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$Area)
str(outlier)
data_new <- filter(df, !(Area %in% outlier$out))
df <- data_new

### Feature MajorAxisLength ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(MajorAxisLength)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = MajorAxisLength)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$MajorAxisLength)
data_new <- filter(df, !(MajorAxisLength %in% outlier$out))
df <- data_new

### Feature MinorAxisLength ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(MinorAxisLength)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = MinorAxisLength)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$MinorAxisLength)
data_new <- filter(df, !(MinorAxisLength %in% outlier$out))
df <- data_new

### Feature Eccentricity ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(Eccentricity)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = Eccentricity)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$Eccentricity)
data_new <- filter(df, !(Eccentricity %in% outlier$out))
df <- data_new

### Feature ConvexArea ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(ConvexArea)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = ConvexArea)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$ConvexArea)
data_new <- filter(df, !(ConvexArea %in% outlier$out))
df <- data_new

### Feature Extent ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(Extent)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = Extent)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$Extent)
data_new <- filter(df, !(Extent %in% outlier$out))
df <- data_new

### Feature Perimeter ###
# Outlier Checking through Box-Plot
p1 <- df %>% ggplot(aes(Perimeter)) +
  geom_density(col = "red")

p2 <- df %>% ggplot(aes(x = Perimeter)) +
  geom_boxplot(col = "blue1", )

grid.arrange(p1, p2, ncol = 1)

# Outlier Removing
outlier <- boxplot(df$Perimeter)
data_new <- filter(df, !(Perimeter %in% outlier$out))
df <- data_new
##################################################################
########## Feature Correlation ##########
df_cor = df
df_cor <- subset(df_cor, select = -c(Class))
# Compute the correlation matrix
correlation_matrix <- cor(df_cor)

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black",  # Color of text labels
         tl.srt = 45)  # Rotate text labels for better readability

########## Feature Selection ##########
##Box-Plot Analysis##
# Define the numerical columns
numerical_columns <- c('Area', 'MajorAxisLength', 'MinorAxisLength', 'Eccentricity', 'ConvexArea', 'Extent', 'Perimeter')

# Create a boxplot for each numerical column
for (col in numerical_columns) {
  # Create a new plot
  boxplot(df[[col]] ~ df$Class, data=df, main=paste("Boxplot of", col, "by Raisin Variety"), xlab='', ylab=col, col="skyblue", border="black")
}
# Box-plot Summary
# Eccentric feature distributions between the two classes are slightly different

# Extent feature distributions between the two classes are quite similar

# Meanwhile Area, MajorAxisLength, MinorAxisLength, ConvexArea, and Perimeter 
# features between the two classes are different. It means that these features
# are the key in indicating the species

# with the conclusion above, pick feature (Area, MajorAxisLength, 
# MinorAxisLength, ConvexArea, and Perimeter)
# and drop feature (Eccentric and Extent)

########## Data Preparation ##########
# drop unnecessary feature based on box-plot analysis
df <- subset(df, select = -c(Eccentricity, Extent))

# Re-check Feature Correlation after Feature Selection
df_cor = df
df_cor <- subset(df_cor, select = -c(Class))
# Compute the correlation matrix
correlation_matrix <- cor(df_cor)

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black",  # Color of text labels
         tl.srt = 45)  # Rotate text labels for better readability

#######################################################
#Since the data in "Class" feature is categorical, we need to change it to numerical
#for better results and easier to process

# Convert "Class" column to a factor with custom labels
df$Class <- factor(df$Class, levels = c("Kecimen", "Besni"))

# Convert the factor to numeric
df$Num_Class <- as.numeric(df$Class)

#Drop column Class (categorical)
df <- subset(df, select = -c(Class))

#rename the column Num_Class to Class
df <- df %>% rename(Class = "Num_Class")

#reset the index of the target (feature = Class)
df$Class[df$Class == 1] <- 0
df$Class[df$Class == 2] <- 1

#randomize the order of the data
df <- df[sample(nrow(df)), ] #randomize data row
rownames(df) <- NULL         #reset row index

########## Data Visualization ##########
# Check the number of occurrences of 0 in the "Class" column
count_0 <- sum(df$Class == 0)

# Check the number of occurrences of 1 in the "Class" column
count_1 <- sum(df$Class == 1)

# Print the results
print(count_0)
print(count_1)

# Calculate the frequency of each value (1 and 0)
frequency <- table(df$Class)

# Create a bar plot
barplot(frequency, main = "Bar Plot of Binary Data", 
        xlab = "Value", ylab = "Frequency", col = c("skyblue", "salmon"))

#Histogram for each feature
hist(df$Area)
hist(df$MajorAxisLength)
hist(df$MinorAxisLength)
hist(df$ConvexArea)
hist(df$Perimeter)

########## Data Normalization (Scaling) ##########
# Standardization (Min-Max Normalization)
process <- preProcess(df[,-c(1,2)], method=c("range"))
df <- predict(process, df)
df <- as.data.frame(df)
# Re-check histogram for each features after normalization
hist(df$Area)
hist(df$MajorAxisLength)
hist(df$MinorAxisLength)
hist(df$ConvexArea)
hist(df$Perimeter)

########## Data Train and Testing Split ##########
index <- createDataPartition(df$Class, p = 0.8, list = FALSE)

df_train <- df[index, ]
df_test <- df[-index, ]

xtrain <- subset(df_train, select = -c(Class))
ytrain <- df_train$Class

xtest <- subset(df_test, select = -c(Class))
ytest <- df_test$Class

########## Model (Logistic Regression) ##########
# Fit logistic regression model using training data
model <- glm(ytrain ~ Area + MajorAxisLength + MinorAxisLength + 
               ConvexArea + Perimeter,
             data = xtrain, family = "binomial")

# Summarize the model
summary(model)

########## Prediction ##########
# Predict probabilities on testing data
probabilities <- predict(model, newdata = xtest, type = "response")

# Convert probabilities to binary predictions (e.g., using a threshold of 0.5)
predictions <- ifelse(probabilities > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predictions == ytest)
accuracy

# Calculate F1-Score
f1_score <- F1_Score(y_pred = predictions, y_true = ytest)
f1_score

# Convert the confusion matrix table to a matrix (if it's not already)
conf_matrix <- as.matrix(confusionMatrix(factor(predictions), factor(ytest))$table)

# Create a custom heatmap with numbers inside the boxes
heatmap.2(conf_matrix,
          Rowv = NULL, Colv = NULL,  # Turn off row and column clustering
          dendrogram = "none",        # Do not show dendrograms
          col = heat.colors(10),   # Color palette for the heatmap
          scale = "none",             # Do not scale
          main = "Confusion Matrix Heatmap",  # Title of the plot
          xlab = "Predicted Class",   # X-axis label
          ylab = "True Class",        # Y-axis label
          trace = "none",             # Do not show trace lines
          density.info = "none",      # Do not show density plot
          key = FALSE,                # Do not show color key
          cellnote = conf_matrix,     # Add numbers inside the boxes
          notecol = "black",          # Color of the numbers
          notecex = 1               # Size of the numbers
)