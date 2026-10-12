# Fast checks for the public snapshot and saved results. Requires base R only.
stopifnot(file.exists("data/comments.csv"))
files <- c(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
           list.files("scripts", pattern = "\\.R$", full.names = TRUE))
for (file in files) parse(file = file, encoding = "UTF-8")
read <- function(name) read.csv(file.path("results/tables", name), stringsAsFactors = FALSE,
                              check.names = FALSE, fileEncoding = "UTF-8")
comments <- read.csv("data/comments.csv", stringsAsFactors = FALSE,
                     check.names = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(comments) == 10800L, !anyDuplicated(comments$comment_id),
          all(table(comments$artist) == 3600L), all(table(comments$video_id) == 400L),
          length(unique(comments$author_channel_id)) == 8713L,
          all(grepl("^actor_[0-9]{5}$", comments$author_channel_id)),
          all(grepl("^comment_[0-9]{5}$", comments$comment_id)),
          all(!is.na(comments$comment_text) & nzchar(trimws(comments$comment_text))))
activity <- aggregate(comment_id ~ artist + author_channel_id, comments, length)
summary <- read("Q4_Unique_Actor_Summary.csv")
for (i in seq_len(nrow(summary))) {
  a <- activity[activity$artist == summary$artist[i], ]
  stopifnot(nrow(a) == summary$unique_actors[i],
            sum(a$comment_id > 1) == summary$repeat_actors[i])
}
sentiment <- read("Q9_Artist_Sentiment_Summary.csv")
stopifnot(all(sentiment$positive_comments + sentiment$neutral_comments +
              sentiment$negative_comments == 3600L))
performance <- read("Q10_Model_Performance.csv")
confusion <- read("Q10_Tuned_Confusion_Matrix.csv")
stopifnot(all(c("Actual", "Predicted", "Comments") %in% names(confusion)))
matrix <- xtabs(Comments ~ Actual + Predicted, data = confusion)
stopifnot(sum(matrix) == 2160, all(rowSums(matrix) == 720),
          abs(sum(diag(matrix)) / sum(matrix) -
              performance$accuracy[performance$model == "Tuned tree"]) < 1e-10)
stopifnot(all(file.exists(c("report/music-audience-analysis.pdf",
  "dashboard/3032ICT_Music_Dashboard.pbip", "docs/images/audience_participation.png",
  "docs/images/sentiment_comparison.png", "docs/images/model_evaluation.png"))))
message("PASS: R syntax, saved data, participation counts, sentiment totals and model accuracy.")
