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

par(mfrow = c(1,2))

plot(density(ldt_remove$Length),
     main = "Density of Word Length", xlab = "Mean RT")

par(mfrow = c(1,1))

ncvTest(m)

durbinWatsonTest(m)

cor.test(ldt_remove$Length, ldt_remove$Mean_RT,
         method = "pearson", alternative = "greater")

lex <- c(47, 89, 131, 186, 245, 284, 362, 444, 553, 627)

gram <- c(0, 2, 1, 3, 5, 9, 7, 16, 25, 34)

child <- data.frame(lex, gram)

str(child)

plot(child$lex, child$gram,
     main = "Scatterplot of Vocabulary Size and Grammatical Complexity",
     xlab = "Lexical Unit", ylab = "Grammatical Complexity Score")

cor.test(child$lex, child$gram, method = "spearman", alternative = "greater")

cor.test(child$lex, child$gram, method = "kendall", alternative = "greater")
