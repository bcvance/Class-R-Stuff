library(rstatix)
library(modeest)
library(BBmisc)
library(lattice)

sent <- read.csv("sent.csv")

head(sent)
summary(sent)

table(sent)

sent.t <- table(sent$clause)

prop.table(sent.t)*100

sent_labels <- prop.table(sent.t)*100

sent_labels <- paste(sent_labels, "%", sep = "")

sent_colors <- c("black", "grey40", "grey80")

pie(sent.t, main = "Pie Chart of Clause Types", 
    labels = sent_labels, col = sent_colors)

sent_levels = c("Ditransitive", "Intransitive", "Transitive")

legend("bottomleft", legend = sent_levels,
       fill = sent_colors, y.intersp = 1.3, bty = "n")

pie(sent.t, main = "Pier Chart of Clause Types",
    labels = sent_labels, col = rainbow(3))

legend("bottomleft", legend = sent_levels,
       fill = "grey50", y.intersp = 1.3, bty = "n")

barplot(sent.t, main = "Bar Plot of Clause Types",
        col = rainbow(3), cex.names = 1.2, xlab = "Clause Type",
        ylab = "Frequency")

rownames(sent.t) <- c("Ditransitive", "Intransitive", "Transitive")

barplot(sent.t, main = "Bar Plot of Clause Types",
        col = "grey50", cex.names = 1.2, xlab = "Clause Type",
        ylab = "Frequency")

sent.t2 <- table(sent)

rownames(sent.t2) <- c("Ditransitive", "Intransitive", "Transitive")
colnames(sent.t2) <- c("Abstract", "Animal", "Human", "Material-Object")

barplot(t(sent.t2), main = "Bar Plot of Clause and SUbject Types", col = rainbow(4),
        xlab = "Clause Type", ylab = "Frequency", beside = TRUE)

legend("topleft", legend = colnames(sent.t2), title = "Subject Types",
       fill = rainbow(4), y.intersp = 1.3, bty = "n")

dotchart(sent.t, main = "Dot Chart of Clause Types",
         xlab = "Frequency", ylab = "", color = "black", pch = 16)

title(ylab="Clause Type", line = 2.5, cex.lab = 1.1)

dotchart(t(sent.t2), main = "Dot Chart of Clause and Subject Types",
         xlab = "Frequency", ylab = "",
         color = c("grey30", "red", "blue", "darkorange"), pch = 16)

title(ylab="Clause and Subject Types", line=2.0, cex.lab=1.1)

dotplot(sent.t2, main = "Dot Plot of Clause and Subject Types",
        xlab = "Frequency", ylab = "Clause Type", pch = 1,
        cex = 1.2, auto.key = list(space = "right", title = "Subject Types"),
        cex.title = 1.2)

dotplot(sent.t2, group = colnames(sent.t2),
        main = "Dot Plot of Clause and Subject Types",
        xlab = "Frequency", ylab = "Clause Type, cex = 1.2")



