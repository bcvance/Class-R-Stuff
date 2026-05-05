library(car); library(coin); library(nparcomp); library(ggplot2)

NSL <- read.csv("NSL.csv")

head(NSL)

NSL_2 <- NSL[NSL$PowerMannerPath < 0.8,]

outlier <- boxplot.stats(NSL_2[NSL_2$Cohort == 3,]$PowerMannerPath)$out

NSL_3 <- NSL_2[NSL_2$PowerMannerPath != outlier,]

boxplot(PowerMannerPath ~ Cohort, xlab = "Cohort",
        ylab = "Squared Proportion of Separate Gestures",
        main = "Path and Motion in NSL", data = NSL_3)

NSL_3$Cohort <- as.factor(NSL_3$Cohort)

NSL_3.anova <- aov(PowerMannerPath ~ Cohort, data = NSL_3)

summary(NSL_3.anova)

ggplot(NSL_3, aes(x = Cohort, y = PowerMannerPath)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", shape = 23, fill = "darkblue", size = 3)

tukey_result <- TukeyHSD(NSL_3.anova)
tukey_result

plot(tukey_result)

tukey_result_df <- as.data.frame(tukey_result$Cohort)
tukey_result_df$comparison <- rownames(tukey_result_df)

ggplot(tukey_result_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Tukey HSD Pairwise Comparisons",
       y = "Mean Difference", x = "Group Comparison")