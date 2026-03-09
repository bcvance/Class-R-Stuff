library(ggplot2)
library(gridExtra)
library(ggpubr)

data <- read.csv("GJT analysis.csv")
head(data)

relevant_data <- data[,c("GJT.score", "Age.of.acquisition", "Age.of.arrival",
                         "Length.of.residence", "Percentage.of.L1.usage", 
                         "Percentage.of.L2.usage", "L2.proficiency.score")]

head(relevant_data)

correlation_matrix <- cor(relevant_data, use="complete.obs", 
                          method="pearson")

print(correlation_matrix)

ggplot(relevant_data, aes(x = Age.of.acquisition, y = GJT.score)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson") +
  theme_minimal()

variables <- c("Age.of.acquisition", "Age.of.arrival",
               "Length.of.residence", "Percentage.of.L1.usage", 
               "Percentage.of.L2.usage", "L2.proficiency.score")

create_plot <- function(var_name) {
  plot <- ggplot(data, aes_string(x = var_name, y = "GJT.score")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method="lm", se=FALSE, color="blue") +
    stat_cor(method = "pearson") +
    theme_minimal() +
    labs(title = paste("Correlation between GJT score and", var_name),
         x = var_name,
         y = "GJT score")
  return(plot)
}

plots <- lapply(variables, create_plot)

plots_arranged <- grid.arrange(grobs=plots, ncol=3, top="Correlation Analysis")

# ggsave("plots_arranged.pdf", plots_arranged, width = 16, height = 8)



