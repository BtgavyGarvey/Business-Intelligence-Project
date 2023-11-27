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
                     Hour = col_number(), `WeekdayName` = col_factor())
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

