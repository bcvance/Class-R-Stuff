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

doenLaten$Aux <- as.factor(doenLaten$Aux)
doenLaten$Country <- as.factor(doenLaten$Country)
doenLaten$Causation <- as.factor(doenLaten$Causation)

doenLaten$Aux <- relevel(doenLaten$Aux, ref = "laten")
doenLaten$Country <- relevel(doenLaten$Country, ref = "NL")

par(mfrow = c(2, 2))

plot(doenLaten$Country[which(doenLaten$Causation == "Affective")],
     doenLaten$Aux[which(doenLaten$Causation == "Affective")],
     main = "Affective", xlab = "Country", ylab = "Auxiliary")

plot(doenLaten$Country[which(doenLaten$Causation == "Inducive")],
     doenLaten$Aux[which(doenLaten$Causation == "Inducive")],
     main = "Inducive", xlab = "Country", ylab = "Auxiliary")

plot(doenLaten$Country[which(doenLaten$Causation == "Physical")],
     doenLaten$Aux[which(doenLaten$Causation == "Physical")],
     main = "Physical", xlab = "Country", ylab = "Auxiliary")

plot(doenLaten$Country[which(doenLaten$Causation == "Volitional")],
     doenLaten$Aux[which(doenLaten$Causation == "Volitional")],
     main = "Volitional", xlab = "Country", ylab = "Auxiliary")

par(mfrow = c(1,1))

m1a.glm <- glm(Aux ~ Country, family = binomial, data = doenLaten)

m1b.glm <- glm(Aux ~ Causation, family = binomial, data = doenLaten)

m2.glm <- glm(Aux ~ Country + Causation, family = binomial, data = doenLaten)

anova(m1a.glm, m2.glm, test = "Chisq")

anova(m1b.glm, m2.glm, test = "Chisq")

m3.glm <- glm(Aux ~ Country * Causation, family = binomial, data = doenLaten)

anova(m2.glm, m3.glm, test = "Chisq")

summary(m2.glm)

