# QUESTION 2: Revised YouTube collection following lecturer feedback
# Run from the 3032ICT_Music_Analytics project, after existing tuber OAuth login.
# Uses the previously selected 27 videos. Does not repeat searches.
# Requests 350 top-level comments/video and retains ALL available replies to
# those parents. No cap is applied to combined comments and replies.
# Original files are untouched. Completed videos are checkpointed separately.
# API results can change during collection (deleted/added comments, for example).

if (!requireNamespace("tuber", quietly = TRUE)) {
  stop("Load your existing project and install/authenticate tuber first.")
}
required_api <- c("get_comment_threads", "get_comments")
if (!all(required_api %in% getNamespaceExports("tuber"))) {
  stop("This script needs the legacy tuber get_comment_threads/get_comments API. Do not upgrade tuber; send me your packageVersion('tuber') result.")
}

q2_files <- c("04_RData/Q2_Selected_Videos.RData",
              "04_RData/Q2_YouTube_Data.RData")
q2_files <- q2_files[file.exists(q2_files)]
if (!length(q2_files)) {
  stop("Open your original RStudio project: the saved selected-video RData is missing from this working directory.")
}
q2_saved <- new.env(parent = emptyenv())
load(q2_files[1], envir = q2_saved)
q2_videos <- q2_saved$selected_videos
stopifnot(is.data.frame(q2_videos), nrow(q2_videos) == 27,
          all(c("artist", "video_id", "title", "search_term") %in% names(q2_videos)),
          !anyNA(q2_videos$video_id), !anyDuplicated(q2_videos$video_id),
          length(unique(q2_videos$artist)) == 3,
          all(table(q2_videos$artist) == 9))

q2_top_level_target <- 350L
q2_videos$target_comments <- NULL
q2_videos$target_top_level_comments <- q2_top_level_target
q2_checkpoint_dir <- "04_RData/Q2_Revised_Checkpoints"
for (folder in c(q2_checkpoint_dir, "02_Raw_Data", "03_Clean_Data")) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# QUESTION 2: Helpers for raw API pages and consistent comment records.
q2_value <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  as.character(x[[1]])
}
q2_has_token <- function(x) !is.null(x) && length(x) == 1L && nzchar(x)
q2_unique_objects <- function(x) {
  ids <- vapply(x, function(z) q2_value(z$id), character(1))
  if (anyNA(ids) || any(!nzchar(ids))) stop("API returned a missing comment ID.")
  x[!duplicated(ids)]
}
q2_record <- function(x, video, parent = NA_character_) {
  s <- x$snippet
  original <- q2_value(s$textOriginal)
  display <- q2_value(s$textDisplay)
  text <- if (!is.na(original) && nzchar(trimws(original))) original else display
  author_id <- s$authorChannelId
  if (is.list(author_id)) author_id <- author_id$value
  data.frame(
    comment_id = q2_value(x$id), artist = as.character(video$artist),
    video_id = as.character(video$video_id), video_title = as.character(video$title),
    search_term = as.character(video$search_term), comment_text = text,
    author_channel_id = q2_value(author_id),
    like_count = suppressWarnings(as.numeric(q2_value(s$likeCount))),
    published_at = q2_value(s$publishedAt),
    collection_date = as.character(Sys.Date()), source = "YouTube",
    author_display_name = q2_value(s$authorDisplayName),
    parent_comment_id = parent, is_reply = !is.na(parent),
    text_original = original, text_display = display,
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
}

q2_download_video <- function(video) {
  threads <- list()
  page_token <- NULL
  seen_tokens <- character()
  repeat {
    page <- tuber::get_comment_threads(
      filter = c(video_id = as.character(video$video_id)),
      part = "snippet,replies", text_format = "plainText", simplify = FALSE,
      max_results = min(100L, q2_top_level_target - length(threads)),
      page_token = page_token
    )
    # YouTube's default order is time. A thread contains one top-level comment.
    threads <- c(threads, page$items)
    if (length(threads)) {
      ids <- vapply(threads, function(z) q2_value(z$snippet$topLevelComment$id), character(1))
      if (anyNA(ids)) stop("Thread has no top-level comment ID.")
      threads <- threads[!duplicated(ids)]
    }
    if (length(threads) >= q2_top_level_target) break
    page_token <- page$nextPageToken
    if (!q2_has_token(page_token)) break
    if (page_token %in% seen_tokens) stop("Repeated thread pagination token.")
    seen_tokens <- c(seen_tokens, page_token)
  }
  if (!length(threads)) stop("No top-level comments returned for ", video$video_id)

  records <- list()
  for (j in seq_along(threads)) {
    thread <- threads[[j]]
    top <- thread$snippet$topLevelComment
    parent <- q2_value(top$id)
    records[[length(records) + 1L]] <- q2_record(top, video)
    reply_count <- suppressWarnings(as.integer(q2_value(thread$snippet$totalReplyCount)))
    replies <- thread$replies$comments
    # Embedded replies can be incomplete. Fetch every page for this parent.
    if ((!is.na(reply_count) && reply_count > 0L) || length(replies)) {
      token <- NULL
      seen <- character()
      repeat {
        page <- tuber::get_comments(
          filter = c(parent_id = parent), part = "snippet",
          max_results = 100L, text_format = "plainText",
          page_token = token, simplify = FALSE
        )
        replies <- c(replies, page$items)
        token <- page$nextPageToken
        if (!q2_has_token(token)) break
        if (token %in% seen) stop("Repeated reply pagination token for ", parent)
        seen <- c(seen, token)
      }
    }
    if (length(replies)) {
      replies <- q2_unique_objects(replies)
      for (reply in replies) {
        records[[length(records) + 1L]] <- q2_record(reply, video, parent)
      }
    }
  }
  result <- do.call(rbind, records)
  result[!duplicated(result$comment_id), , drop = FALSE]
}

# QUESTION 2: Download/resume. An API error stops the script; earlier videos stay saved.
q2_parts <- vector("list", nrow(q2_videos))
for (i in seq_len(nrow(q2_videos))) {
  video <- q2_videos[i, , drop = FALSE]
  checkpoint <- file.path(q2_checkpoint_dir, paste0(video$video_id, ".rds"))
  message("Video ", i, "/", nrow(q2_videos), ": ", video$title)
  if (file.exists(checkpoint)) {
    cached <- readRDS(checkpoint)
    stopifnot(identical(cached$target, q2_top_level_target),
              identical(cached$video_id, as.character(video$video_id)))
    q2_parts[[i]] <- cached$data
    message("Loaded completed checkpoint.")
  } else {
    q2_parts[[i]] <- q2_download_video(video)
    cached <- list(target = q2_top_level_target,
                   video_id = as.character(video$video_id), data = q2_parts[[i]])
    temporary <- paste0(checkpoint, ".tmp")
    saveRDS(cached, temporary)
    if (!file.rename(temporary, checkpoint)) stop("Could not save checkpoint.")
  }
  message("Kept ", sum(!q2_parts[[i]]$is_reply), " top-level comments + ",
          sum(q2_parts[[i]]$is_reply), " replies.")
}
youtube_comments_raw_revised <- do.call(rbind, q2_parts)
row.names(youtube_comments_raw_revised) <- NULL
stopifnot(!anyDuplicated(youtube_comments_raw_revised$comment_id))
raw <- youtube_comments_raw_revised
stopifnot(all(raw$parent_comment_id[raw$is_reply] %in% raw$comment_id[!raw$is_reply]))

# QUESTION 2: Keep raw records; exclude blank text only from the text-analysis dataset.
usable <- !is.na(raw$comment_text) & nzchar(trimws(raw$comment_text))
youtube_comments_clean_revised <- raw[usable, , drop = FALSE]
row.names(youtube_comments_clean_revised) <- NULL
q2_summary <- function(rows) {
  good <- !is.na(rows$comment_text) & nzchar(trimws(rows$comment_text))
  data.frame(top_level = sum(!rows$is_reply), replies = sum(rows$is_reply),
             raw_comments = nrow(rows), usable_comments = sum(good),
             blank_text = sum(!good),
             missing_usernames = sum(is.na(rows$author_display_name) |
                                       trimws(rows$author_display_name) == ""),
             missing_actor_ids = sum(is.na(rows$author_channel_id) |
                                       trimws(rows$author_channel_id) == ""))
}
q2_revised_per_video <- do.call(rbind, lapply(seq_len(nrow(q2_videos)), function(i) {
  v <- q2_videos[i, , drop = FALSE]
  cbind(v[, c("artist", "video_id", "title")], q2_summary(raw[raw$video_id == v$video_id, ]))
}))
q2_revised_per_artist <- do.call(rbind, lapply(unique(raw$artist), function(a) {
  cbind(data.frame(artist = a), q2_summary(raw[raw$artist == a, ]))
}))
q2_revised_per_artist$within_3000_to_4000 <- with(
  q2_revised_per_artist, usable_comments >= 3000 & usable_comments <= 4000
)
write.csv(raw, "02_Raw_Data/Q2_YouTube_Comments_Raw_Revised.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(youtube_comments_clean_revised,
          "03_Clean_Data/Q2_YouTube_Comments_Clean_Revised.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(q2_revised_per_artist, "03_Clean_Data/Q2_Revised_Per_Artist.csv", row.names = FALSE)
write.csv(q2_revised_per_video, "03_Clean_Data/Q2_Revised_Per_Video.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
save(youtube_comments_raw_revised, youtube_comments_clean_revised,
     q2_videos, q2_top_level_target, q2_revised_per_artist, q2_revised_per_video,
     file = "04_RData/Q2_YouTube_Data_Revised.RData")
print(q2_revised_per_artist, row.names = FALSE)
print(q2_revised_per_video, row.names = FALSE)
if (!all(q2_revised_per_artist$within_3000_to_4000)) {
  message("Some usable totals are outside the brief's range. All records are saved; do not trim replies. Review these totals before updating the report.")
}
message("Revised collection saved. Original Q2-Q11 results have not been overwritten.")
