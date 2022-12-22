library(dplyr)
library(stats)
library(broom)
library(CCA)
library(MASS)
library(pROC)

data <- read.csv("data.csv")
n <- nrow(data)
set.seed(123)

indices <- sample(1:n)
train_size <- round(0.8 * n)
train_indices <- indices[1:train_size]
test_indices <- indices[(train_size + 1):n]

# Split the dataset into the training set and test set
train <- data[train_indices, ]
test <- data[test_indices, ]

# Convert the malignant column to a factor
train$malignant <- as.factor(train$malignant)
test$malignant <- as.factor(test$malignant)


model_glm <- glm(malignant ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean, data = train, family = "binomial")
print(summary(model_glm))

# Use the model to predict the probabilities of success on the test set
predicted_probs_glm <- predict(model_glm, newdata = train, type = "response")
predicted_success_glm <- ifelse(predicted_probs_glm > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_success_glm, Actual = train$malignant)
accuracy_glm <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_glm <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall_glm <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

model_lda1 <- lda(malignant ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean, data = train)
print(summary(model_lda1))

# Use the model to predict the probabilities of success on the test set
predicted_lda1 <- predict(model_lda1, newdata = train, type = "response")
confusion_matrix_lda1 <- table(Predicted = predicted_lda1$class, Actual = train$malignant)
accuracy_lda1 <- sum(diag(confusion_matrix_lda1)) / sum(confusion_matrix_lda1)
precision_lda1 <- confusion_matrix_lda1[2, 2] / sum(confusion_matrix_lda1[2, ])
recall_lda1 <- confusion_matrix_lda1[2, 2] / sum(confusion_matrix_lda1[, 2])
prob_lda1 <- predicted_lda1$posterior[, 1]
discriminant_power_lda1 <- mean(prob_lda1[train$malignant == 1]) - mean(prob_lda1[train$malignant == 0])

# Build the linear discriminant analysis
model_lda2 <- lda(malignant ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean, data = train)
print(summary(model_lda2))

# Use the model to predict the probabilities of success on the test set
predicted_lda2 <- predict(model_lda2, newdata = train, type = "response")
confusion_matrix_lda2 <- table(Predicted = predicted_lda2$class, Actual = train$malignant)
accuracy_lda2 <- sum(diag(confusion_matrix_lda2)) / sum(confusion_matrix_lda2)
precision_lda2 <- confusion_matrix_lda2[2, 2] / sum(confusion_matrix_lda2[2, ])
recall_lda2 <- confusion_matrix_lda2[2, 2] / sum(confusion_matrix_lda2[, 2])
prob_lda2 <- predicted_lda2$posterior[, 1]
discriminant_power_lda2 <- mean(prob_lda2[train$malignant == 1]) - mean(prob_lda2[train$malignant == 0])

# Print accuracy, precision, and recall
print("accuracy")
print("GLM")
print(accuracy_glm)
print("LDA1")
print(accuracy_lda1)
print("LDA2")
print(accuracy_lda2)

print("precision")
print("GLM")
print(precision_glm)
print("LDA1")
print(precision_lda1)
print("LDA2")
print(precision_lda2)

print("recall")
print("GLM")
print(recall_glm)
print("LDA1")
print(recall_lda1)
print("LDA2")
print(recall_lda2)

# Print the discriminant power
print("discriminant power")
print("LDA1")
print(discriminant_power_lda1)
print("LDA2")
print(discriminant_power_lda2)

model_lda <- model_lda2

pred_logistic <- predict(model_glm, train, type = "response")
pred_lda <- predict(model_lda, train)$class

scores_logistic <- predict(model_glm, train, type = "response")
scores_lda <- predict(model_lda, train)$posterior[, 1]

# Calculate the ROC curve
roc_logistic <- roc(train$malignant, scores_logistic)
roc_lda <- roc(train$malignant, scores_lda)

youden_logistic <- roc_logistic$sensitivities + roc_logistic$specificities - 1
youden_lda <- roc_lda$sensitivities + roc_lda$specificities - 1

threshold_logistic <- roc_logistic$thresholds[which.max(youden_logistic)]
threshold_lda <- roc_lda$thresholds[which.max(youden_lda)]

# Calculate the AUC
auc_logistic <- auc(roc_logistic)
auc_lda <- auc(roc_lda)

# Plot the ROC curve
pdf("figs/q2_roc.pdf")
plot(roc_logistic, col = "red", main = "ROC Curve")
par(new = TRUE)
plot(roc_lda, col = "blue", add = TRUE)
legend("bottomright", c("Logistic Regression", "LDA"), col = c("red", "blue"), lty = 1)
dev.off()

# Print the AUC
print("AUC")

print(auc_logistic)
print(auc_lda)

# Print the threshold
print("thresholds")
print(threshold_logistic)
print(threshold_lda)

# predict on the test set
predicted_probs_glm <- predict(model_glm, newdata = test, type = "response")
predicted_success_glm <- ifelse(predicted_probs_glm > 0.5, 1, 0)

predicted_lda <- predict(model_lda, newdata = test, type = "response")
predicted_success_lda <- ifelse(predicted_lda$posterior[, 1] < 0.5, 1, 0)

# Print the confusion matrix
confusion_matrix_glm <- table(Predicted = predicted_success_glm, Actual = test$malignant)
confusion_matrix_lda <- table(Predicted = predicted_success_lda, Actual = test$malignant)
print(confusion_matrix_glm)
print(confusion_matrix_lda)