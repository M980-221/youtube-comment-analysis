# ============================================================
# 3032ICT ASSIGNMENT - QUESTION 10
# Decision tree for predicting the artist discussed in a comment
# ============================================================

# 1. Packages

required_packages <- c(
  "tm", "slam", "rpart", "rpart.plot", "caret",
  "ggplot2", "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tm)
library(slam)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(scales)

# 2. Load and validate data

q10_input <- new.env(parent = baseenv())
load("04_RData/Q5_Term_Document_Matrices.RData", envir = q10_input)
q5_cleaned_comments <- q10_input$q5_cleaned_comments
rm(q10_input)

stopifnot(exists("q5_cleaned_comments"))
stopifnot(nrow(q5_cleaned_comments) == 10800)
stopifnot(all(c("comment_id", "artist", "cleaned_text") %in%
                names(q5_cleaned_comments)))

dir.create("03_Clean_Data", showWarnings = FALSE, recursive = TRUE)
dir.create("04_RData", showWarnings = FALSE, recursive = TRUE)
dir.create("05_Plots", showWarnings = FALSE, recursive = TRUE)

q10_data <- q5_cleaned_comments[, c(
  "comment_id", "artist", "cleaned_text"
)]

q10_data$cleaned_text[is.na(q10_data$cleaned_text)] <- ""

q10_data$artist <- factor(
  q10_data$artist,
  levels = c("Taylor Swift", "Ariana Grande", "Billie Eilish")
)

stopifnot(all(table(q10_data$artist) == 3600))

# 3. Balanced 80/20 train-test split

set.seed(3032)

q10_train_indices <- unlist(lapply(
  split(seq_len(nrow(q10_data)), q10_data$artist),
  function(indices) {
    sample(indices, size = floor(length(indices) * 0.80))
  }
))

q10_train_indices <- sort(q10_train_indices)

q10_train_text <- q10_data[q10_train_indices, ]
q10_test_text <- q10_data[-q10_train_indices, ]

row.names(q10_train_text) <- NULL
row.names(q10_test_text) <- NULL

stopifnot(nrow(q10_train_text) == 8640)
stopifnot(nrow(q10_test_text) == 2160)
stopifnot(all(table(q10_train_text$artist) == 2880))
stopifnot(all(table(q10_test_text$artist) == 720))

# 4. Create a training vocabulary without test-data leakage

q10_train_corpus <- VCorpus(
  VectorSource(q10_train_text$cleaned_text)
)

q10_train_dtm_all <- DocumentTermMatrix(
  q10_train_corpus,
  control = list(wordLengths = c(3, Inf))
)

q10_training_term_frequency <- sort(
  col_sums(q10_train_dtm_all),
  decreasing = TRUE
)

q10_number_of_features <- min(
  250,
  length(q10_training_term_frequency)
)

q10_top_terms <- names(head(
  q10_training_term_frequency,
  q10_number_of_features
))

q10_train_dtm <- q10_train_dtm_all[, q10_top_terms]

q10_test_corpus <- VCorpus(
  VectorSource(q10_test_text$cleaned_text)
)

q10_test_dtm <- DocumentTermMatrix(
  q10_test_corpus,
  control = list(dictionary = q10_top_terms)
)

# Q10 correction: tm sorts dictionary terms alphabetically. Align both
# matrices by the original term names BEFORE assigning model-variable names.
stopifnot(setequal(colnames(q10_test_dtm), q10_top_terms))
q10_test_dtm <- q10_test_dtm[, q10_top_terms]
stopifnot(identical(colnames(q10_train_dtm), q10_top_terms))
stopifnot(identical(colnames(q10_test_dtm), q10_top_terms))

# 5. Convert terms to binary predictors

q10_train_matrix <- as.matrix(q10_train_dtm)
q10_test_matrix <- as.matrix(q10_test_dtm)

q10_train_matrix[q10_train_matrix > 0] <- 1
q10_test_matrix[q10_test_matrix > 0] <- 1

q10_feature_names <- make.unique(make.names(q10_top_terms))

colnames(q10_train_matrix) <- q10_feature_names
colnames(q10_test_matrix) <- q10_feature_names

q10_feature_dictionary <- data.frame(
  original_term = q10_top_terms,
  model_variable = q10_feature_names,
  training_frequency = as.numeric(
    q10_training_term_frequency[q10_top_terms]
  ),
  stringsAsFactors = FALSE
)

q10_train_model_data <- data.frame(
  artist = q10_train_text$artist,
  q10_train_matrix,
  check.names = FALSE
)

q10_test_model_data <- data.frame(
  artist = q10_test_text$artist,
  q10_test_matrix,
  check.names = FALSE
)

# 6. Baseline decision tree

set.seed(3032)

q10_baseline_tree <- rpart(
  artist ~ .,
  data = q10_train_model_data,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 30,
    minbucket = 10,
    maxdepth = 8,
    xval = 10
  )
)

q10_baseline_prediction <- predict(
  q10_baseline_tree,
  newdata = q10_test_model_data,
  type = "class"
)

# 7. Improved tree using five-fold cross-validation

q10_cross_validation <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = "final",
  allowParallel = FALSE
)

q10_tuning_grid <- data.frame(
  cp = c(0.0005, 0.001, 0.002, 0.005, 0.010, 0.020)
)

set.seed(3032)
message("Running five-fold decision-tree tuning...")

q10_tuned_training <- train(
  artist ~ .,
  data = q10_train_model_data,
  method = "rpart",
  metric = "Accuracy",
  trControl = q10_cross_validation,
  tuneGrid = q10_tuning_grid,
  control = rpart.control(
    minsplit = 20,
    minbucket = 7,
    maxdepth = 20,
    xval = 0
  )
)

q10_best_cp <- q10_tuned_training$bestTune$cp
q10_final_tree <- q10_tuned_training$finalModel

q10_tuned_prediction <- predict(
  q10_tuned_training,
  newdata = q10_test_model_data,
  type = "raw"
)

# 8. Performance calculations

calculate_model_metrics <- function(actual, predicted, model_name) {
  class_levels <- levels(actual)
  
  actual_factor <- factor(actual, levels = class_levels)
  predicted_factor <- factor(predicted, levels = class_levels)
  
  confusion <- table(
    Actual = actual_factor,
    Predicted = predicted_factor
  )
  
  true_positive <- diag(confusion)
  precision <- true_positive / colSums(confusion)
  recall <- true_positive / rowSums(confusion)
  f1_score <- 2 * precision * recall / (precision + recall)
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1_score[is.na(f1_score)] <- 0
  
  overall <- data.frame(
    model = model_name,
    accuracy = sum(true_positive) / sum(confusion),
    balanced_accuracy = mean(recall),
    macro_precision = mean(precision),
    macro_recall = mean(recall),
    macro_f1 = mean(f1_score)
  )
  
  per_class <- data.frame(
    model = model_name,
    artist = class_levels,
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1_score = as.numeric(f1_score),
    support = as.numeric(rowSums(confusion))
  )
  
  list(
    overall = overall,
    per_class = per_class,
    confusion = confusion
  )
}

q10_baseline_evaluation <- calculate_model_metrics(
  q10_test_model_data$artist,
  q10_baseline_prediction,
  "Baseline tree"
)

q10_tuned_evaluation <- calculate_model_metrics(
  q10_test_model_data$artist,
  q10_tuned_prediction,
  "Tuned tree"
)

q10_model_performance <- rbind(
  q10_baseline_evaluation$overall,
  q10_tuned_evaluation$overall
)

q10_per_class_metrics <- rbind(
  q10_baseline_evaluation$per_class,
  q10_tuned_evaluation$per_class
)

q10_baseline_confusion <- q10_baseline_evaluation$confusion
q10_tuned_confusion <- q10_tuned_evaluation$confusion

q10_test_predictions <- data.frame(
  comment_id = q10_test_text$comment_id,
  actual_artist = q10_test_text$artist,
  baseline_prediction = q10_baseline_prediction,
  tuned_prediction = q10_tuned_prediction,
  baseline_correct = q10_baseline_prediction == q10_test_text$artist,
  tuned_correct = q10_tuned_prediction == q10_test_text$artist
)

# 9. Variable importance

q10_variable_importance <- varImp(
  q10_tuned_training,
  scale = TRUE
)$importance

q10_variable_importance$term <- row.names(q10_variable_importance)

if ("Overall" %in% names(q10_variable_importance)) {
  q10_variable_importance$importance <-
    q10_variable_importance$Overall
} else {
  numeric_columns <- names(q10_variable_importance)[
    vapply(q10_variable_importance, is.numeric, logical(1))
  ]
  
  q10_variable_importance$importance <- apply(
    q10_variable_importance[, numeric_columns, drop = FALSE],
    1,
    max
  )
}

q10_variable_importance <- q10_variable_importance[
  order(-q10_variable_importance$importance),
  c("term", "importance")
]

row.names(q10_variable_importance) <- NULL

q10_top20_importance <- head(q10_variable_importance, 20)

q10_top20_importance$original_term <-
  q10_feature_dictionary$original_term[
    match(
      q10_top20_importance$term,
      q10_feature_dictionary$model_variable
    )
  ]

# 10. Validate model outputs

stopifnot(nrow(q10_test_predictions) == 2160)
stopifnot(nrow(q10_model_performance) == 2)
stopifnot(all(q10_model_performance$accuracy >= 0))
stopifnot(all(q10_model_performance$accuracy <= 1))
stopifnot(sum(q10_tuned_confusion) == 2160)
stopifnot(nrow(q10_per_class_metrics) == 6)
stopifnot(length(q10_top_terms) == q10_number_of_features)

# 11. Tuned decision-tree plot

png(
  filename = "05_Plots/Q10_Tuned_Decision_Tree.png",
  width = 3600,
  height = 2400,
  res = 250
)

rpart.plot(
  q10_final_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "auto",
  branch.lty = 3,
  shadow.col = "grey85",
  tweak = 0.85,
  main = paste(
    "Tuned Decision Tree for Artist Prediction - Best CP:",
    q10_best_cp
  )
)

dev.off()

# 12. Confusion-matrix plot

q10_confusion_plot_data <- as.data.frame(q10_tuned_confusion)

names(q10_confusion_plot_data) <- c(
  "Actual", "Predicted", "Comments"
)

q10_confusion_plot_data$RowTotal <- ave(
  q10_confusion_plot_data$Comments,
  q10_confusion_plot_data$Actual,
  FUN = sum
)

q10_confusion_plot_data$Percentage <- round(
  q10_confusion_plot_data$Comments /
    q10_confusion_plot_data$RowTotal * 100,
  1
)

q10_confusion_plot <- ggplot(
  q10_confusion_plot_data,
  aes(x = Predicted, y = Actual, fill = Comments)
) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(
    aes(label = paste0(Comments, "\n", Percentage, "%")),
    fontface = "bold",
    size = 4
  ) +
  scale_fill_gradient(low = "#F7FBFF", high = "#2166AC") +
  labs(
    title = "Tuned Decision-Tree Confusion Matrix",
    subtitle = "Independent 20% test dataset",
    x = "Predicted artist",
    y = "Actual artist",
    fill = "Comments"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  "05_Plots/Q10_Confusion_Matrix.png",
  q10_confusion_plot,
  width = 9,
  height = 7,
  dpi = 300
)

# 13. Baseline-versus-tuned comparison plot

q10_model_plot_data <- rbind(
  data.frame(
    model = q10_model_performance$model,
    metric = "Accuracy",
    score = q10_model_performance$accuracy
  ),
  data.frame(
    model = q10_model_performance$model,
    metric = "Balanced accuracy",
    score = q10_model_performance$balanced_accuracy
  ),
  data.frame(
    model = q10_model_performance$model,
    metric = "Macro F1",
    score = q10_model_performance$macro_f1
  )
)

q10_model_comparison_plot <- ggplot(
  q10_model_plot_data,
  aes(x = metric, y = score, fill = model)
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.68
  ) +
  geom_text(
    aes(label = percent(score, accuracy = 0.1)),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(values = c(
    "Baseline tree" = "#BAB0AC",
    "Tuned tree" = "#4E79A7"
  )) +
  scale_y_continuous(
    labels = percent,
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Decision-Tree Performance Comparison",
    subtitle = "Baseline model and five-fold tuned model",
    x = "Performance measure",
    y = "Test performance",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "05_Plots/Q10_Model_Performance_Comparison.png",
  q10_model_comparison_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# 14. Feature-importance plot

q10_importance_plot_data <- q10_top20_importance

q10_importance_plot_data$display_term <- ifelse(
  is.na(q10_importance_plot_data$original_term),
  q10_importance_plot_data$term,
  q10_importance_plot_data$original_term
)

q10_importance_plot_data$display_term <- factor(
  q10_importance_plot_data$display_term,
  levels = rev(q10_importance_plot_data$display_term)
)

q10_importance_plot <- ggplot(
  q10_importance_plot_data,
  aes(x = display_term, y = importance)
) +
  geom_col(fill = "#4E79A7", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Important Terms for Artist Prediction",
    subtitle = "Scaled importance from the tuned decision tree",
    x = "Comment term",
    y = "Variable importance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "05_Plots/Q10_Top20_Feature_Importance.png",
  q10_importance_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# 15. Export evidence

write.csv(
  q10_model_performance,
  "03_Clean_Data/Q10_Model_Performance.csv",
  row.names = FALSE
)

write.csv(
  q10_per_class_metrics,
  "03_Clean_Data/Q10_Per_Class_Metrics.csv",
  row.names = FALSE
)

write.csv(
  q10_confusion_plot_data,
  "03_Clean_Data/Q10_Tuned_Confusion_Matrix.csv",
  row.names = FALSE
)

write.csv(
  q10_test_predictions,
  "03_Clean_Data/Q10_Test_Predictions.csv",
  row.names = FALSE
)

write.csv(
  q10_feature_dictionary,
  "03_Clean_Data/Q10_Feature_Dictionary.csv",
  row.names = FALSE
)

write.csv(
  q10_top20_importance,
  "03_Clean_Data/Q10_Top20_Feature_Importance.csv",
  row.names = FALSE
)

write.csv(
  q10_tuned_training$results,
  "03_Clean_Data/Q10_Cross_Validation_Results.csv",
  row.names = FALSE
)

save(
  q10_baseline_tree,
  q10_tuned_training,
  q10_final_tree,
  q10_best_cp,
  q10_model_performance,
  q10_per_class_metrics,
  q10_baseline_confusion,
  q10_tuned_confusion,
  q10_test_predictions,
  q10_feature_dictionary,
  q10_top20_importance,
  q10_confusion_plot,
  q10_model_comparison_plot,
  q10_importance_plot,
  file = "04_RData/Q10_Decision_Tree.RData"
)

# 16. Final results

print(q10_best_cp)
print(q10_model_performance, row.names = FALSE)
print(q10_per_class_metrics, row.names = FALSE)
print(q10_tuned_confusion)
print(q10_top20_importance, row.names = FALSE)
