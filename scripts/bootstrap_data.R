# Create the local R snapshots needed by the original analysis modules.
# No API call is made. The published CSV row order is preserved.
bootstrap_data <- function() {
  if (!file.exists("data/comments.csv")) {
    stop("Run from the repository root, or open youtube-comment-analysis.Rproj.")
  }
  comments <- read.csv("data/comments.csv", stringsAsFactors = FALSE,
                       check.names = FALSE, fileEncoding = "UTF-8")
  videos <- read.csv("data/videos.csv", stringsAsFactors = FALSE,
                    check.names = FALSE, fileEncoding = "UTF-8")
  required <- c("comment_id", "artist", "video_id", "video_title",
                "comment_text", "author_channel_id", "like_count")
  stopifnot(all(required %in% names(comments)), nrow(comments) == 10800L,
            !anyDuplicated(comments$comment_id),
            all(table(comments$artist) == 3600L),
            all(table(comments$video_id) == 400L),
            nrow(videos) == 27L, !anyDuplicated(videos$video_id),
            setequal(comments$video_id, videos$video_id),
            all(!is.na(comments$comment_text) & nzchar(trimws(comments$comment_text))))
  for (path in c("03_Clean_Data", "04_RData", "05_Plots")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  youtube_comments_clean <- comments
  selected_videos <- videos
  save(youtube_comments_clean, file = "04_RData/Q2_YouTube_Comments_Clean.RData")
  save(selected_videos, file = "04_RData/Q2_Selected_Videos.RData")
  write.csv(comments, "03_Clean_Data/Q2_YouTube_Comments_Clean.csv", row.names = FALSE,
            fileEncoding = "UTF-8")
  write.csv(videos, "03_Clean_Data/Q2_Selected_Videos.csv", row.names = FALSE,
            fileEncoding = "UTF-8")
  message("Prepared 10,800 saved comments and 27 videos. No live recollection.")
  invisible(list(youtube_comments_clean = comments, selected_videos = videos))
}
invisible(list2env(bootstrap_data(), envir = environment()))
