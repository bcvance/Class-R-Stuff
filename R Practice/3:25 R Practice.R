library(energy)
library(car)

ldt <- read.csv("ldt-1.csv")

plot(ldt$Mean_RT ~ ldt$Length,
     main = "Scatterplot of Word Length and Mean RTs",
     xlab = "Word Length",
     ylab = "Mean RT")

boxplot(ldt$Mean_RT, main = "Boxplot of Mean RTs", ylab="Mean RT")

ldt_remove <- ldt[ldt$Mean_RT <= 1200, ]

boxplot(ldt_remove$Mean_RT, main = "Boxplot of Mean RTs", ylab = "Mean RT")

plot(ldt_remove$Mean_RT ~ ldt_remove$Length,
     main = "Scatterplot of Word Length and Mean RTs",
     xlab = "Word Length",
     ylab = "Mean RT")

m <- lm(ldt_remove$Mean_RT ~ ldt_remove$Length)

abline(m)

