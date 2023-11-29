if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

renv::init()

renv::restore()

if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

library(readr)
#dailySales <- read.csv("data/salesdaily.csv", header = FALSE,
                                     #stringsAsFactors = TRUE)

library(readr)
dailySales <- read_csv("data/saleshourly.csv", 
    col_types = cols(datum = col_datetime(format = "%m/%d/%Y %H:%M"), 
                     M01AB = col_double(), M01AE = col_double(), 
                     N02BA = col_double(), N02BE = col_double(), 
                     N05B = col_double(), N05C = col_double(), 
                     R03 = col_double(), R06 = col_double(), 
                     Year = col_number(), Month = col_number(), 
                     Hour = col_number(), `Weekday Name` = col_factor())
)

View(dailySales)

## Measures of Frequency ----

dailySales_freq <- dailySales$M01AB
cbind(frequency = table(dailySales_freq),
      percentage = prop.table(table(dailySales_freq)) * 100)

## Measures of Central Tendency ----

dailySales_mode <- names(table(dailySales$M01AB))[
  which(table(dailySales$M01AB) == max(table(dailySales$M01AB)))
]
print(dailySales_mode)

## Measures of Distribution ----
summary(dailySales)

## Measures of Relationship ----
dailySales_cov <- cov(dailySales[, 2:12])
View(dailySales_cov)

# Inferential Statistics ----
dailySales_anova <- aov(M01AB ~ Month + Hour, data = dailySales)
summary(dailySales_anova)

## Univariate Plots ----

dailySales_plot <- as.numeric(unlist(dailySales[,]))
hist(dailySales_plot, main = names(dailySales)[])

## Multivariate Plots ----
if (!is.element("caret", installed.packages()[, 1])) {
  install.packages("caret", dependencies = TRUE)
}
require("caret")
featurePlot(x = dailySales[, 1:4], y = dailySales[, 5], plot = "box")

## Missing Data ----


if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")

if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

any_na(dailySales)

# How many?
n_miss(dailySales)

# What is the percentage of missing data in the entire dataset?
prop_miss(dailySales)

# How many missing values does each variable have?
dailySales %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(dailySales)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(dailySales)

# Which variables contain the most missing values?
gg_miss_var(dailySales)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(dailySales) + theme(axis.text.x = element_text(angle = 80))

# Which combinations of variables are missing together?
gg_miss_upset(dailySales)


missmap(dailySales, col = c("red", "grey"), legend = TRUE)

## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## 1. Split the dataset ====

train_index <- createDataPartition(dailySales$M01AB,
                                   p = 0.75,
                                   list = FALSE)
dailySales_train <- dailySales[train_index, ]
dailySales_test <- dailySales[-train_index, ]
print(dailySales_train)
print(dailySales_test)

## 1. Bootstrapping ====

train_control <- trainControl(method = "boot", number = 500)


## 1. Cross validation (repeated) ====

train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

## 1. Model Training ====

demand_forecasting_dataset_model_lm <- # nolint
  caret::train(M01AB ~
                 datum +
                 Year + Month + Hour + `Weekday Name`,
               data = dailySales_train,
               trControl = train_control,
               na.action = na.omit, method = "lm", metric = "RMSE")

predictions_lm <- predict(demand_forecasting_dataset_model_lm,
                          dailySales_test[, 1:13])

print(demand_forecasting_dataset_model_lm)
print(predictions_lm)

if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## 3.a. Train the Models ----
# We train the following models, all of which are using 10-fold repeated cross
# validation with 3 repeats:
#   LDA
#   CART
#   KNN
#   SVM
#   Random Fores


### KNN ----
set.seed(7)
dailySales_model_knn <- train(M01AB ~ ., data = dailySales,
                            method = "knn", trControl = train_control)


### Random Forest ----
set.seed(7)
dailySales_model_rf <- train(M01AB ~ ., data = dailySales,
                           method = "rf", trControl = train_control)

## 3.b. Call the `resamples` Function ----
# We then create a list of the model results and pass the list as an argument
# to the `resamples` function.

results <- resamples(list(
                          KNN = dailySales_model_knn, SVM = dailySales_model_svm,
                          RF = dailySales_model_rf))

# STEP 4. Display the Results ----
## 1. Table Summary ----
# This is the simplest comparison. It creates a table with one model per row
# and its corresponding evaluation metrics displayed per column.

summary(results)


# STEP 5. Apply a "Grid Search" to identify the best parameter value ----
## RRF ----



train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "grid")
set.seed(7)

getModelInfo("RRFglobal")

tunegrid <- expand.grid(.mtry = c(1:10),
                        .coefReg = seq(from = 0.1, to = 1, by = 0.1))

seed <- 7
metric <- "RMSE"

dailySales_model_grid_search_rrf_global <- train(M01AB ~ ., data = dailySales, # nolint
                                            method = "RRFglobal",
                                            metric = metric,
                                            tuneGrid = tunegrid,
                                            trControl = train_control)
print(dailySales_model_grid_search_rrf_global)
plot(dailySales_model_grid_search_rrf_global)

if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

predict_sales <- function(start_date, end_date, drug) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = 'day')
  df_test <- data.frame(datum = dates, Year = year(dates), Month = month(dates), `Weekday Name` = wday(dates), day = day(dates), Drug = drug)
  df_test$predicted_quantity <- predict(loaded_model, newdata = as.matrix(df_test))
  return(df_test)
}

