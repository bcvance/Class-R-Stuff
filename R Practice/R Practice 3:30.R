library(car)
library(lm.beta)

ELP <- read.csv("ELP.csv")

str(ELP)

head(ELP)

par(mar = c(5,5,4,2))

plot(ELP$Mean_RT ~ ELP$Length,
     main = "Scatterplot of Word Lengths and Mean RTs",
     xlab = "Word Length", ylab = "Mean RT")

par(mfrow = c(1,2))

ELP$LogRT <- log(ELP$Mean_RT)

LengthOutliers <- boxplot.stats(ELP$Length)$out

ELP_1 <- ELP[ELP$Length < 15,]

par(mfrow = c(1,2))

par(mfrow = c(1,1))

LogRTOutliers_1 <- boxplot.stats(ELP_1$LogRT)$out

sort(LogRTOutliers_1)

ELP_2 <- ELP_1[ELP_1$LogRT < 7,]

m <- lm(ELP_2$LogRT ~ ELP_2$Length)

summary(m)

par(mar = c(5,5,4,2))

plot(ELP_2$LogRT ~ ELP_2$Length,
     main = "Scatterplot of Word Lengths and Log-Transformed RTs",
     xlab = "Word Length", ylab = "Log-Transformed RT")

abline(m)


