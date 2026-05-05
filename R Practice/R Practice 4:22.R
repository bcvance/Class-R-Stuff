library(car)
library(stats)

sharedref <- read.csv("sharedref.csv")


table.1 <- aggregate(mod ~ age + cohort, data = sharedref, FUN = mean)
table.1

boxplot(sharedref$mod ~ sharedref$age + sharedref$cohort,
        main = "Average Number of Spatial Modulations per Verb",
        names = c("C1, Early", "C1, Middle", "C1, Late", "C2, Early", "C2, Middle", "C2, Late"),
        cex.axis = 1)

sharedref.anova <- aov(mod ~ age * cohort, data = sharedref)

summary(sharedref.anova)

interaction.plot(table.1$age, table.1$cohort,
                 table.1$mod, main = "Interaction Between Age and Cohort",
                 xlab = "Age", ylab = "number of Spatial Modulations",
                 trace.label = "Cohort")

early <- sharedref[sharedref$age == "early",]
middle <- sharedref[sharedref$age == "middle",]
late <- sharedref[sharedref$age == "late",]

e.c1 <- early$mod[which(early$cohort == 1)]
e.c2 <- early$mod[which(early$cohort == 2)]

m.c1 <- middle$mod[which(middle$cohort == 1)]
m.c2 <- middle$mod[which(middle$cohort == 2)]

l.c1 <- late$mod[which(late$cohort == 1)]
l.c2 <- late$mod[which(late$cohort == 2)]

t.test(e.c1, e.c2, var.equal = TRUE, alternative = "less")
t.test(m.c1, m.c2, var.equal = TRUE, alternative = "less")
t.test(l.c1, l.c2, var.equal = TRUE, alternative = "less")



alpha <- 0.05/3

