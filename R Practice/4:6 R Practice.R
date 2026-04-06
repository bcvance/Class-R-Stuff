library(visreg)
library(MASS)
library(car)
library(plyr)
library(dplyr)

ELP_2 <- read.csv("ELP_2.csv")

ELP_2$LogFreq <- log(ELP_2$WF)

ELP_3 <- ELP_2[ELP_2$LogFreq >= -4 & ELP_2$LogFreq <= 4.4,]

m1 <- lm(LogRT ~ Length + LogFreq, data = ELP_3)

summary(m1)

unique(ELP_3$POS)

ELP_3$POS <- factor(ELP_3$POS)

m.POS <- lm(LogRT ~ POS, data = ELP_3)

ELP_3$POS <- relevel(ELP_3$POS, ref = "NN")

m.POS.2 <- lm(LogRT ~ POS, data = ELP_3)

summary(m.POS.2)


m.Length <- lm(LogRT ~ Length + POS, data = ELP_3)

visreg(m.Length, xvar = "Length", by = "POS")

summary(m.Length)

m.Length.int <- lm(LogRT ~ Length * POS, data = ELP_3)

summary(m.Length.int)

anova(m.Length, m.Length.int)

doenLaten <- read.csv("doenLaten.csv")

head(doenLaten)
str(doenLaten)
