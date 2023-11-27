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
if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")
corrplot(cor(dailySales[, 4]), method = "circle")

