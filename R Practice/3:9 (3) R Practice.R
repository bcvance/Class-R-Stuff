library(ggplot2)

data <- read.csv("lexical_decision-data.csv")
head(data)

table(data$condition, data$related)

agr.rt <- aggregate(data = data, rt ~ condition + related, FUN = mean)
agr.rt

ggplot(data = agr.rt) +
  geom_bar(mapping = aes(x = condition, y = rt, fill = related), stat = "identity",
           position = "dodge") +
  ylim(0, 1000) +
  theme_bw() +
  ggtitle("Mean Reaction Times")

ggplot(data = data, aes(x = condition, y = rt, fill = related)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  ylim(0, 800) +
  theme_bw() +
  ggtitle("Reaction times by condition")
  
agr.calcs <- do.call(data.frame, aggregate(data = data, rt ~ condition + related,
                                           FUN = function(x) c(mean = mean (x),
                                                               se = sqrt(var(x)/length(x)))))  

data("diamonds")
ggplot(diamonds, aes(cut, price, fill = color)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")

ggplot(data = data, aes(x = condition, y = rt, fill = related)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  ylim(0, 800) +
  theme_bw() +
  ggtitle("Reaction times by condition")