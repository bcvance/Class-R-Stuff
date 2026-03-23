library(gplots)
library(ggplot2)
library(car)
library(carData)

pym_high <- read.csv("pym_high.csv")
pym_low <- read.csv("pym_low.csv")

summary(pym_high$assoc)
summary(pym_low$assoc)

boxplot(pym_high$assoc, pym_low$assoc, names = c("high", "low"),
        main = "Box Plot of Average Mumber of Associations",
        xlab = "Frequency Group",
        ylab = "Average Number of Associations")

boxplot.stats(pym_low$assoc)$out

pym_low[pym_low$assoc == 3,]

outlier <- which(pym_low$assoc == 3,)

pym_low_cleaned <- pym_low[-outlier,]

dim(pym_low_cleaned)


boxplot(pym_high$assoc, pym_low_cleaned$assoc, names = c("high", "low"),
        main = "Box Plot of Average Mumber of Associations",
        xlab = "Frequency Group",
        ylab = "Average Number of Associations")

pym_high$freq <- "high"
pym_low_cleaned$freq <- "low"

pym_all <- rbind(pym_high, pym_low_cleaned)

pym_all$freq <- as.factor(pym_all$freq)

str(pym_all)
dim(pym_all)

leveneTest(pym_all$assoc ~ pym_all$freq)

t.test(pym_all$assoc ~ pym_all$freq, var.equal = TRUE, alternative = "greater")


mean = c(mean.high, mean.low)

ci.lower = c(ci.lower.high, ci.lower.low)
ci.upper = c(ci.upper.high, ci.upper.low)

barplot2(mean, plot.ci = TRUE, ci.l = ci.lower, ci.u = ci.upper,
         main = "Bar Plot with 95% Confidence Intervals",
         xlab = "Frequency Groups", ylab = "Average Number of Associations",
         names = c("High", "Low"), ylim = c(0,7))