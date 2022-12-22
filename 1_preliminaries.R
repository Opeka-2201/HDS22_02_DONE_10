library(stats)
library(corrplot)
library(xtable)
library(ggplot2)

data <- read.csv("data.csv")

binary_indicator <- data[,1]
explanatory_vars <- data[,-1]

colors <- ifelse(binary_indicator == 1, "red", "green")

benign <- explanatory_vars[binary_indicator == 0, ]
malignant <- explanatory_vars[binary_indicator == 1, ]

n_benign <- nrow(benign)
n_malignant <- nrow(malignant)

# pairplot colored with the 2 classes
pdf("figs/q1_pairs.pdf")
pairs(explanatory_vars, col=colors)
dev.off()

# boxplot of the two groups for each variable
pdf("figs/q1_boxplot.pdf")
par(mfrow = c(2, 5))
for (i in seq_len(ncol(explanatory_vars))) {
  boxplot(explanatory_vars[,i] ~ binary_indicator, col = colors, main = colnames(explanatory_vars)[i])
}
dev.off()

# histogram of the two groups for each variable as ratio with the number of observation
pdf("figs/q1_histogram.pdf")
par(mfrow = c(3, 4))
for (i in seq_len(ncol(explanatory_vars))) {
  hist(benign[,i], col = "green", main = colnames(explanatory_vars)[i], xlim = range(explanatory_vars[,i]), breaks = 20, freq = FALSE, xlab = colnames(explanatory_vars)[i])
  hist(malignant[,i], col = "red", add = TRUE, xlim = range(explanatory_vars[,i]), breaks = 20, freq = FALSE, xlab = colnames(explanatory_vars)[i])
}
dev.off()

# plot correlation matrix
pdf("figs/q1_correlation.pdf")
corrplot(cor(explanatory_vars), method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()

pdf("figs/q1_correlation_benign.pdf")
corrplot(cor(benign), method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()

pdf("figs/q1_correlation_malignant.pdf")
corrplot(cor(malignant), method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()

# print("MEAN TABLE")
mean_table <- matrix(NA, ncol(explanatory_vars), 2)
colnames(mean_table) <- c("benign", "malignant")
rownames(mean_table) <- colnames(explanatory_vars)
for (i in seq_len(ncol(explanatory_vars))) {
  mean_table[i, 1] <- mean(benign[,i])
  mean_table[i, 2] <- mean(malignant[,i])
}
# print(xtable(mean_table, digits = 3))

# print("VARIANCE TABLE")
var_table <- matrix(NA, ncol(explanatory_vars), 2)
colnames(var_table) <- c("benign", "malignant")
rownames(var_table) <- colnames(explanatory_vars)
for (i in seq_len(ncol(explanatory_vars))) {
  var_table[i, 1] <- var(benign[,i])
  var_table[i, 2] <- var(malignant[,i])
}
# print(xtable(var_table, digits = 3))
