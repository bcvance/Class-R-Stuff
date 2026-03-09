library(rstatix)
library(modeest)
library(BBmisc)

mean(data1$RT, na.rm = TRUE)

median(data1$RT, na.rm = TRUE)

range(data1$RT, na.rm = TRUE)

var(data1$RT, na.rm = TRUE)

sd(data1$RT, na.rm = TRUE)

sd(data1$RT, na.rm = TRUE)/sqrt(nrow(data1))

IQR(data1$RT, na.rm = TRUE)

boxplot(data1$RT, main = "Main reaction times", ylab = "reaction time, ms")

identify_outliers(data1, RT)

data1$ZRT <- scale(data1$RT)

identify_outliers(data1, ZRT)

outlier <- which(abs(data1$ZRT) >= 2)

data1_remove <- data[-outlier,]

dim(data1_remove)