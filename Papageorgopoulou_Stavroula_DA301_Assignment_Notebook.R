## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.

# Import the necessary libraries.

library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(corrplot)


setwd("C:/Users/stavp/OneDrive/Desktop/ergasia3/LSE_DA301_assignment_files/LSE_DA301_assignment_files_new")

turtle <- read.csv("turtle_reviews.csv", header=TRUE)

view(turtle)

# View first few rows of sales DataFrame.
head(turtle)

# View first ten entries and view variable types.
as_tibble(turtle)

# Different view of each variable.
glimpse(turtle)

# Drop unnecessary columns
turtle <- turtle %>%
  select(-platform, -language)

# View the data
head(turtle)

# Rename the spending_score (1-100), remuneration (k£)
turtle <- turtle %>%
  rename(remuneration = 'remuneration..k..')

turtle <- turtle %>%
  rename(spending_score = 'spending_score..1.100.')

# Check the column names
print(names(turtle))


# Create a summary of the new data frame
summary(turtle)

# Have a look at data types
column_data_types <- sapply(turtle, class)
print(column_data_types)

# Determine the sum of missing values. 
sum(is.na(turtle))

# Distribution of Loyalty points
ggplot(turtle, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loyalty points", x = "Loyalty points", y = "Frequency")


# Histogram of loyalty points
ggplot(turtle, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Loyalty Points",
       x = "Loyalty Points",
       y = "Frequency")

# Boxplot of loyalty points by gender
ggplot(turtle, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() +
  labs(title = "Loyalty Points Distribution by Gender",
       x = "Gender",
       y = "Loyalty Points")

# Scatterplot of loyalty points vs age
ggplot(turtle, aes(x = age, y = loyalty_points)) +
  geom_point(alpha = 0.5) +
  labs(title = "Loyalty points vs Age", x = "Age", y = "Loyalty points")

# Scatterplot of loyalty points vs remuneration
ggplot(turtle, aes(x = remuneration, y = loyalty_points)) +
  geom_point(alpha = 0.5) +
  labs(title = "Loyalty points vs Remuneration", x = "Remuneration", y = "Loyalty points")

# Scatterplot of loyalty points vs remuneration
ggplot(turtle, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 0.5) +
  labs(title = "Loyalty points vs Spending score", x = "Spending score", y = "Loyalty points")





###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.

# Correlation

numeric_turtle <- turtle %>%
  select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_turtle, use = "complete.obs") #basically uses rows with complete data i.e no nulls

# Create the correlation plot
corrplot(correlation_matrix, method = "circle")


# View the correlation matrix
correlation_matrix

# Notes for correlation
# Age has a very weak negative correlation with loyalty_points (-0.04244465). This suggests that age has little to no linear relationship with loyalty points accumulation.
# Age has a weak negative correlation with spending_score (-0.224334309), indicating that older customers tend to have slightly lower spending scores, but the relationship is not strong.
# Remuneration has a strong positive correlation with loyalty_points (0.61606475). This suggests that higher-paid customers tend to accumulate more loyalty points.
# Remuneration has a moderate positive correlation with product (0.305309289), indicating some relationship between income and product choice.
# Spending_score has the strongest positive correlation with loyalty_points (0.67231011). This indicates that customers who spend more (higher spending score) tend to accumulate more loyalty points, which makes sense for a loyalty program.
# Spending_score has very weak correlations with other variables except age (as mentioned earlier).
# As noted, loyalty_points are most strongly correlated with spending_score and remuneration.
# There's a weak positive correlation with product (0.183599612), suggesting that product choice may have some influence on loyalty point accumulation, but it's not a strong relationship.

# Pair plots to visualize relationships
pairs(turtle[, c('loyalty_points', 'age', 'remuneration', 'spending_score')])

# loyalty_points and age: The scatter plot suggests some clustering, indicating different age groups might have varying loyalty points.
# loyalty_points and remuneration: There's a positive trend suggesting that higher remuneration might be associated with higher loyalty points.
# loyalty_points and spending_score: A positive trend is visible, suggesting that higher spending scores might correlate with higher loyalty points.
# age and remuneration: This scatter plot shows several clusters, which might indicate different groups of people with distinct remuneration patterns across age ranges.
# age and spending_score: The scatter plot shows some clustering but does not show a clear trend.
# remuneration and spending_score: There appears to be a positive trend indicating that higher remuneration might be associated with higher spending scores.

# Measures of Shape

# Shapiro-Wilk test for normality
shapiro.test(turtle$loyalty_points)

# The W value (0.84307) is significantly lower than 1, suggesting that the data does not follow a normal distribution closely.
# The p-value (< 2.2e-16) is much smaller than the common significance level (0.05), providing strong evidence against the null hypothesis of normality.

# Skewness and Kurtosis
skewness(turtle$loyalty_points)
kurtosis(turtle$loyalty_points)

# A skewness of 1.463694 suggests that the loyalty_points distribution is skewed to the right. This indicates there is a greater concentration of values at the lower end and a tail that stretches towards the higher end of the loyalty points range.
# A kurtosis value of 4.70883 suggests that the loyalty_points distribution has thicker tails compared to a normal distribution. This indicates the existence of additional extreme values or outliers within the dataset.

# Calculate Range
range_satisfaction <- range(turtle$loyalty_points)

# Calculate Difference between highest and lowest values
difference_high_low <- diff(range_loyalty_points)

# Calculate the range of loyalty points
range_loyalty_points <- range(turtle$loyalty_points)

# Calculate Interquartile Range (IQR)
iqr_loyalty <- IQR(turtle$loyalty_points)

# Calculate Variance
variance_loyalty <- var(turtle$loyalty_points)

# Calculate Standard Deviation
std_deviation_loyalty <- sd(turtle$loyalty_points)

# Display results
list(
  Range = range_loyalty_points,
  Difference = difference_high_low,
  IQR = iqr_loyalty,
  Variance = variance_loyalty,
  Standard_Deviation = std_deviation_loyalty
)

# Range (6822): Indicates the broad distribution of loyalty points. A wide range indicates a considerable difference in the number of points received by diverse customers.

# Interquartile Range (IQR) of 979.25 represents the dispersion of the central 50% of the data. It is a more concentrated way to measure dispersion than the range, which can be skewed by outliers.

# Variance (1646704): A high variance indicates substantial differences in the number of points customers accumulate. It takes into account the squared distances from the mean, meaning that greater deviations from the average have a greater impact on the variance.

# Standard Deviation (1283.24): It indicates the spread of data in loyalty points units. Understanding variance is simpler because it is measured on the same scale as the data. A larger standard deviation indicates a broader range of loyalty points from the average.

# Visualisations

# Select numeric features for the model
model_data <- turtle %>% 
  select(loyalty_points, age, remuneration, spending_score)

# Create the multiple linear regression model
model <- lm(loyalty_points ~ age + remuneration + spending_score, data = model_data)

# Print the model

summary(model)

# Residual plots
par(mfrow = c(2,2))
plot(model)

# Check for multicollinearity
vif(model)

# Predict using an example scenario
new_data <- data.frame(
  age = c(30, 45, 60),
  remuneration = c(50, 75, 100),
  spending_score = c(40, 60, 80)
)

predictions <- predict(model, newdata = new_data)
prediction_results <- cbind(new_data, predicted_loyalty_points = predictions)
print(prediction_results)

# Create multiple linear regression

model <- lm(remuneration ~ age + spending_score + loyalty_points, data = turtle)
summary(model)

turtle$predicted_remuneration <- predict(model, turtle)

# Actual vs Predicted values plot
ggplot(turtle, aes(x = remuneration, y = predicted_remuneration)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ggtitle("Actual vs Predicted Remuneration") +
  theme_minimal()


# Explaining the above results:
# Model Suitability:
# The model accounts for around 84% of the variability in loyalty points, showing a strong fit with a Multiple R-squared of 0.8399 and an Adjusted R-squared of 0.8397.
# Important factors that forecast outcomes:
# Age: Every extra year of age corresponds to a rise of 11.06 loyalty points (p < 2e-16).
# Salary: The more you are paid, the more loyalty points you earn.
# For every additional point spent, loyalty points increase by 34.18 (p < 2e-16).
# Statistics of the model:
# The Residual Standard Error is 513.8, showing the average difference between the predicted and observed loyalty points.
# The model shows high significance overall with an F-statistic of 3491 and p-value less than 2.2e-16.
# Remainders:
# The residuals vary from -1819.11 to 1894.62, with a median near zero (4.61), indicating a symmetrical distribution around zero.

###############################################################################
###############################################################################
