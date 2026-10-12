# QUESTION 2: Collection and reproducibility of the submitted capped sample.
# Run this file from the project root. The default is OFFLINE: load the supplied
# original raw and final repaired clean snapshots, validate them, and regenerate
# their count summaries. It does not collect new comments or add missing replies.
#
# Historical online collection is kept in q2_collect_live() for reference/use with
# your own credentials. It is NOT run when this file is normally sourced.
# To explicitly recollect in a separate project copy:
#   options(q2.recollect = TRUE)
#   source("01_Code/Q2_Data_Collection..R")
#   options(q2.recollect = FALSE)
# A new API run may return different records and overwrite this copy's Q2 files.
# It retains the original 400-row sampling method and is not the revised reply
# workflow. Downstream results must be recomputed if that dataset is replaced.

q2_collect_live <- function() {
  if (!isTRUE(getOption("q2.recollect", FALSE))) {
    stop("Live collection is disabled. Default source() reproduces the saved data without OAuth.")
  }
  for (folder in c("02_Raw_Data", "03_Clean_Data", "04_RData")) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  # Viewer calls are optional presentation only, not an analysis dependency.
  View <- function(x, ...) {
    if (interactive()) utils::View(x, ...) else invisible(x)
  }
  # Question 2

  if (!requireNamespace("tuber", quietly = TRUE)) {
    stop("Live recollection needs the original tuber package and your own OAuth setup. Default saved-data mode needs neither.")
  }
  if (!all(c("yt_oauth", "yt_search", "get_stats", "get_all_comments") %in%
           getNamespaceExports("tuber"))) {
    stop("The installed tuber API does not match this historical collector. Use saved-data mode to reproduce the submitted results.")
  }

  library(tuber)

  options(httr_oauth_cache = FALSE)

  yt_oauth(
    Sys.getenv("YOUTUBE_CLIENT_ID"),
    Sys.getenv("YOUTUBE_CLIENT_SECRET"),
    token = path.expand("~/.youtube_token_3032.rds")
  )

  search_taylor_video <- yt_search(
    term = "Taylor Swift official music video",
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  View(search_taylor_video)
  View(search_taylor_video[, c(
    "video_id",
    "title",
    "channelTitle"
  )])
  taylor_channel_id <- search_taylor_video$channelId[
    search_taylor_video$channelTitle == "Taylor Swift"
  ][1]

  search_taylor_lyric <- yt_search(
    term = "Taylor Swift lyric video",
    channel_id = taylor_channel_id,
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  search_taylor_audio <- yt_search(
    term = "Taylor Swift official audio",
    channel_id = taylor_channel_id,
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  taylor_music_candidates <- search_taylor_video[
    search_taylor_video$channelId == taylor_channel_id &
      grepl("Official Music Video|Official Video",
            search_taylor_video$title,
            ignore.case = TRUE),
  ]

  taylor_lyric_candidates <- search_taylor_lyric[
    grepl("Lyric", search_taylor_lyric$title, ignore.case = TRUE),
  ]

  taylor_audio_candidates <- search_taylor_audio[
    grepl("Audio", search_taylor_audio$title, ignore.case = TRUE),
  ]

  taylor_music_candidates$search_term <- rep(
    "official music video",
    nrow(taylor_music_candidates)
  )

  taylor_lyric_candidates$search_term <- rep(
    "lyric video",
    nrow(taylor_lyric_candidates)
  )

  taylor_audio_candidates$search_term <- rep(
    "official audio",
    nrow(taylor_audio_candidates)
  )

  taylor_candidates <- rbind(
    taylor_music_candidates,
    taylor_lyric_candidates,
    taylor_audio_candidates
  )

  taylor_candidates <- taylor_candidates[
    !duplicated(taylor_candidates$video_id),
  ]

  row.names(taylor_candidates) <- NULL

  View(taylor_candidates[, c(
    "video_id",
    "publishedAt",
    "title",
    "channelTitle",
    "search_term"
  )])
  taylor_stats <- get_stats(
    video_ids = taylor_candidates$video_id
  )

  View(taylor_stats[, c(
    "id",
    "statistics_viewCount",
    "statistics_likeCount",
    "statistics_commentCount"
  )])
  taylor_stats_small <- data.frame(
    video_id = taylor_stats$id,
    view_count = as.numeric(taylor_stats$statistics_viewCount),
    like_count = as.numeric(taylor_stats$statistics_likeCount),
    comment_count = as.numeric(taylor_stats$statistics_commentCount)
  )

  taylor_candidates <- merge(
    taylor_candidates,
    taylor_stats_small,
    by = "video_id",
    all.x = TRUE,
    sort = FALSE
  )

  taylor_music_selected <- taylor_candidates[
    taylor_candidates$search_term == "official music video" &
      taylor_candidates$comment_count >= 400,
  ]

  taylor_lyric_selected <- taylor_candidates[
    taylor_candidates$search_term == "lyric video" &
      taylor_candidates$comment_count >= 400,
  ]

  taylor_music_selected <- taylor_music_selected[
    order(-taylor_music_selected$comment_count),
  ]

  taylor_lyric_selected <- taylor_lyric_selected[
    order(-taylor_lyric_selected$comment_count),
  ]

  taylor_selected <- rbind(
    head(taylor_music_selected, 5),
    head(taylor_lyric_selected, 4)
  )

  taylor_selected$artist <- "Taylor Swift"
  taylor_selected$target_comments <- 400
  row.names(taylor_selected) <- NULL

  View(taylor_selected[, c(
    "artist",
    "video_id",
    "title",
    "search_term",
    "view_count",
    "like_count",
    "comment_count",
    "target_comments"
  )])
  ariana_vevo_id <- "UC0VOyT2OCBKdQhF3BAbZ-1g"

  search_ariana_video <- yt_search(
    term = "Ariana Grande official music video",
    channel_id = ariana_vevo_id,
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  search_ariana_lyric <- yt_search(
    term = "Ariana Grande lyric video",
    channel_id = ariana_vevo_id,
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  search_ariana_audio <- yt_search(
    term = "Ariana Grande official audio",
    channel_id = ariana_vevo_id,
    max_results = 10,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  ariana_music_candidates <- search_ariana_video[
    grepl(
      "Official Music Video|Official Video",
      search_ariana_video$title,
      ignore.case = TRUE
    ),
  ]

  ariana_lyric_candidates <- search_ariana_lyric[
    grepl(
      "Lyric",
      search_ariana_lyric$title,
      ignore.case = TRUE
    ),
  ]

  ariana_audio_candidates <- search_ariana_audio[
    grepl(
      "Audio",
      search_ariana_audio$title,
      ignore.case = TRUE
    ),
  ]

  ariana_music_candidates$search_term <- rep(
    "official music video",
    nrow(ariana_music_candidates)
  )

  ariana_lyric_candidates$search_term <- rep(
    "lyric video",
    nrow(ariana_lyric_candidates)
  )

  ariana_audio_candidates$search_term <- rep(
    "official audio",
    nrow(ariana_audio_candidates)
  )

  ariana_candidates <- rbind(
    ariana_music_candidates,
    ariana_lyric_candidates,
    ariana_audio_candidates
  )

  ariana_candidates <- ariana_candidates[
    !duplicated(ariana_candidates$video_id),
  ]

  row.names(ariana_candidates) <- NULL

  View(ariana_candidates[, c(
    "video_id",
    "publishedAt",
    "title",
    "channelTitle",
    "search_term"
  )])

  table(ariana_candidates$search_term)
  ariana_stats <- get_stats(
    video_ids = ariana_candidates$video_id
  )

  ariana_stats_small <- data.frame(
    video_id = ariana_stats$id,
    view_count = as.numeric(ariana_stats$statistics_viewCount),
    like_count = as.numeric(ariana_stats$statistics_likeCount),
    comment_count = as.numeric(ariana_stats$statistics_commentCount)
  )

  View(ariana_stats_small[
    order(-ariana_stats_small$comment_count),
  ])

  sum(
    ariana_stats_small$comment_count >= 400,
    na.rm = TRUE
  )
  ariana_candidates <- merge(
    ariana_candidates,
    ariana_stats_small,
    by = "video_id",
    all.x = TRUE,
    sort = FALSE
  )

  ariana_music_selected <- ariana_candidates[
    ariana_candidates$search_term == "official music video" &
      ariana_candidates$comment_count >= 400,
  ]

  ariana_lyric_selected <- ariana_candidates[
    ariana_candidates$search_term == "lyric video" &
      ariana_candidates$comment_count >= 400,
  ]

  ariana_music_selected <- ariana_music_selected[
    order(-ariana_music_selected$comment_count),
  ]

  ariana_lyric_selected <- ariana_lyric_selected[
    order(-ariana_lyric_selected$comment_count),
  ]

  ariana_selected <- rbind(
    head(ariana_music_selected, 5),
    head(ariana_lyric_selected, 4)
  )

  ariana_selected$artist <- "Ariana Grande"
  ariana_selected$target_comments <- 400

  row.names(ariana_selected) <- NULL

  View(ariana_selected[, c(
    "artist",
    "video_id",
    "title",
    "search_term",
    "view_count",
    "like_count",
    "comment_count",
    "target_comments"
  )])

  table(ariana_selected$search_term)
  sum(ariana_selected$target_comments)
  billie_channel_id <- "UCiGm_E4ZwYSHV3bcW1pnSeQ"
  billie_vevo_id <- "UCDGmojLloWpXok597xYo8cg"

  billie_official_ids <- c(
    billie_channel_id,
    billie_vevo_id
  )

  search_billie_video <- yt_search(
    term = "Billie Eilish official music video",
    max_results = 50,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  search_billie_lyric <- yt_search(
    term = "Billie Eilish lyric video",
    max_results = 50,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  search_billie_audio <- yt_search(
    term = "Billie Eilish official audio",
    max_results = 50,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  billie_music_candidates <- search_billie_video[
    search_billie_video$channelId %in% billie_official_ids,
  ]

  billie_lyric_candidates <- search_billie_lyric[
    search_billie_lyric$channelId %in% billie_official_ids,
  ]

  billie_audio_candidates <- search_billie_audio[
    search_billie_audio$channelId %in% billie_official_ids,
  ]

  billie_music_candidates$search_term <- rep(
    "official music video",
    nrow(billie_music_candidates)
  )

  billie_lyric_candidates$search_term <- rep(
    "lyric video",
    nrow(billie_lyric_candidates)
  )

  billie_audio_candidates$search_term <- rep(
    "official audio",
    nrow(billie_audio_candidates)
  )

  billie_candidates <- rbind(
    billie_music_candidates,
    billie_lyric_candidates,
    billie_audio_candidates
  )

  billie_candidates <- billie_candidates[
    !duplicated(billie_candidates$video_id),
  ]

  row.names(billie_candidates) <- NULL

  View(billie_candidates[, c(
    "video_id",
    "publishedAt",
    "title",
    "channelTitle",
    "search_term"
  )])

  table(billie_candidates$search_term)
  search_billie_live <- yt_search(
    term = "Billie Eilish live performance",
    channel_id = billie_channel_id,
    max_results = 20,
    published_after = "2024-01-01T00:00:00Z",
    published_before = "2026-08-23T00:00:00Z",
    relevance_language = "en"
  )

  billie_live_candidates <- search_billie_live

  billie_live_candidates$search_term <- rep(
    "live performance",
    nrow(billie_live_candidates)
  )

  billie_candidates <- rbind(
    billie_candidates,
    billie_live_candidates
  )

  billie_candidates <- billie_candidates[
    !duplicated(billie_candidates$video_id),
  ]

  row.names(billie_candidates) <- NULL

  View(billie_candidates[, c(
    "video_id",
    "publishedAt",
    "title",
    "channelTitle",
    "search_term"
  )])

  table(billie_candidates$search_term)
  billie_stats <- get_stats(
    video_ids = billie_candidates$video_id
  )

  billie_stats_small <- data.frame(
    video_id = billie_stats$id,
    view_count = as.numeric(billie_stats$statistics_viewCount),
    like_count = as.numeric(billie_stats$statistics_likeCount),
    comment_count = as.numeric(billie_stats$statistics_commentCount)
  )

  View(billie_stats_small[
    order(-billie_stats_small$comment_count),
  ])

  sum(
    billie_stats_small$comment_count >= 400,
    na.rm = TRUE
  )
  billie_candidates <- merge(
    billie_candidates,
    billie_stats_small,
    by = "video_id",
    all.x = TRUE,
    sort = FALSE
  )

  selected_columns <- c(
    "artist",
    "video_id",
    "publishedAt",
    "title",
    "channelTitle",
    "search_term",
    "view_count",
    "like_count",
    "comment_count",
    "target_comments"
  )

  billie_eligible <- billie_candidates[
    billie_candidates$comment_count >= 400 &
      !grepl(
        "trailer|behind|story of my song|on tour|album cover",
        billie_candidates$title,
        ignore.case = TRUE
      ),
  ]

  billie_eligible <- billie_eligible[
    order(-billie_eligible$comment_count),
  ]

  billie_selected <- head(
    billie_eligible,
    9
  )

  billie_selected$artist <- "Billie Eilish"
  billie_selected$target_comments <- 400

  row.names(billie_selected) <- NULL

  selected_videos <- rbind(
    taylor_selected[, selected_columns],
    ariana_selected[, selected_columns],
    billie_selected[, selected_columns]
  )

  row.names(selected_videos) <- NULL

  stopifnot(nrow(billie_selected) == 9)
  stopifnot(nrow(selected_videos) == 27)
  stopifnot(all(table(selected_videos$artist) == 9))

  write.csv(
    selected_videos,
    "03_Clean_Data/Q2_Selected_Videos.csv",
    row.names = FALSE
  )

  save(
    selected_videos,
    file = "04_RData/Q2_Selected_Videos.RData"
  )

  View(billie_selected[, c(
    "artist",
    "video_id",
    "title",
    "search_term",
    "comment_count",
    "target_comments"
  )])

  table(selected_videos$artist)
  comment_list <- vector(
    "list",
    nrow(selected_videos)
  )

  for (i in seq_len(nrow(selected_videos))) {
    message(
      "Collecting video ",
      i,
      " of ",
      nrow(selected_videos),
      ": ",
      selected_videos$title[i]
    )
    
    video_comments <- get_all_comments(
      video_id = selected_videos$video_id[i],
      max_results = selected_videos$target_comments[i]
    )
    
    if (nrow(video_comments) < selected_videos$target_comments[i]) {
      stop(
        paste(
          "Fewer than 400 comments returned for",
          selected_videos$title[i]
        )
      )
    }
    
    video_comments <- head(
      video_comments,
      selected_videos$target_comments[i]
    )
    
    video_comments$artist <- selected_videos$artist[i]
    video_comments$selected_video_id <- selected_videos$video_id[i]
    video_comments$video_title <- selected_videos$title[i]
    video_comments$search_term <- selected_videos$search_term[i]
    video_comments$source <- "YouTube"
    video_comments$collection_date <- as.character(Sys.Date())
    video_comments$collection_row <- seq_len(nrow(video_comments))
    
    comment_list[[i]] <- video_comments
    
    comments_checkpoint <- do.call(
      rbind,
      comment_list[seq_len(i)]
    )
    
    save(
      comments_checkpoint,
      file = "04_RData/Q2_Comments_Checkpoint.RData"
    )
    
    message(
      "Saved ",
      nrow(video_comments),
      " comments"
    )
  }

  youtube_comments_raw <- do.call(
    rbind,
    comment_list
  )

  row.names(youtube_comments_raw) <- NULL

  comment_summary <- aggregate(
    collection_row ~ artist + selected_video_id,
    data = youtube_comments_raw,
    FUN = length
  )

  names(comment_summary)[3] <- "data_points"

  stopifnot(nrow(youtube_comments_raw) == 10800)
  stopifnot(all(table(youtube_comments_raw$artist) == 3600))
  stopifnot(all(comment_summary$data_points == 400))

  write.csv(
    youtube_comments_raw,
    "02_Raw_Data/Q2_YouTube_Comments_Raw.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  save(
    youtube_comments_raw,
    comment_summary,
    selected_videos,
    file = "04_RData/Q2_YouTube_Data.RData"
  )

  table(youtube_comments_raw$artist)
  View(comment_summary)
  duplicate_comment_ids <- sum(
    duplicated(youtube_comments_raw$id)
  )

  missing_comment_text <- sum(
    is.na(youtube_comments_raw$textOriginal) |
      trimws(youtube_comments_raw$textOriginal) == ""
  )

  missing_actor_ids <- sum(
    is.na(youtube_comments_raw$authorChannelId.value) |
      trimws(youtube_comments_raw$authorChannelId.value) == ""
  )

  comments_per_artist <- as.data.frame(
    table(youtube_comments_raw$artist)
  )

  names(comments_per_artist) <- c(
    "artist",
    "data_points"
  )

  comments_per_video <- aggregate(
    id ~ artist + selected_video_id + video_title + search_term,
    data = youtube_comments_raw,
    FUN = length
  )

  names(comments_per_video)[5] <- "data_points"

  validation_summary <- data.frame(
    measure = c(
      "Total data points",
      "Unique comment IDs",
      "Duplicate comment IDs",
      "Missing comment text",
      "Missing actor IDs",
      "Number of artists",
      "Number of videos",
      "Minimum comments per video",
      "Maximum comments per video"
    ),
    result = c(
      nrow(youtube_comments_raw),
      length(unique(youtube_comments_raw$id)),
      duplicate_comment_ids,
      missing_comment_text,
      missing_actor_ids,
      length(unique(youtube_comments_raw$artist)),
      length(unique(youtube_comments_raw$selected_video_id)),
      min(comments_per_video$data_points),
      max(comments_per_video$data_points)
    )
  )

  stopifnot(nrow(youtube_comments_raw) == 10800)
  stopifnot(length(unique(youtube_comments_raw$id)) == 10800)
  stopifnot(duplicate_comment_ids == 0)
  stopifnot(all(comments_per_artist$data_points == 3600))
  stopifnot(all(comments_per_video$data_points == 400))

  write.csv(
    validation_summary,
    "03_Clean_Data/Q2_Validation_Summary.csv",
    row.names = FALSE
  )

  write.csv(
    comments_per_artist,
    "03_Clean_Data/Q2_Comments_Per_Artist.csv",
    row.names = FALSE
  )

  write.csv(
    comments_per_video,
    "03_Clean_Data/Q2_Comments_Per_Video.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  save(
    validation_summary,
    comments_per_artist,
    comments_per_video,
    file = "04_RData/Q2_Validation.RData"
  )

  validation_summary
  comments_per_artist
  # QUESTION 2: Repair blank-text batches before validating/exporting the clean data.
  # The earlier intermediate-clean assertion stopped before this repair. Final
  # 10,800-row validation now occurs only after the repair below.

  # Replace video batches containing blank comments

  blank_text_rows <- is.na(youtube_comments_raw$textOriginal) |
    trimws(youtube_comments_raw$textOriginal) == ""

  affected_video_ids <- unique(
    youtube_comments_raw$selected_video_id[blank_text_rows]
  )

  replacement_list <- vector(
    "list",
    length(affected_video_ids)
  )

  for (j in seq_along(affected_video_ids)) {
    current_video_id <- affected_video_ids[j]
    
    selected_row <- selected_videos[
      selected_videos$video_id == current_video_id,
    ]
    
    message(
      "Repairing video ",
      j,
      " of ",
      length(affected_video_ids),
      ": ",
      selected_row$title[1]
    )
    
    replacement_comments <- get_all_comments(
      video_id = current_video_id,
      max_results = 450
    )
    
    valid_text <- !is.na(replacement_comments$textOriginal) &
      trimws(replacement_comments$textOriginal) != ""
    
    replacement_comments <- replacement_comments[
      valid_text &
        !duplicated(replacement_comments$id),
    ]
    
    if (nrow(replacement_comments) < 400) {
      stop(
        paste(
          "Fewer than 400 valid comments returned for",
          selected_row$title[1]
        )
      )
    }
    
    replacement_comments <- head(
      replacement_comments,
      400
    )
    
    replacement_comments$artist <- selected_row$artist[1]
    replacement_comments$selected_video_id <- current_video_id
    replacement_comments$video_title <- selected_row$title[1]
    replacement_comments$search_term <- selected_row$search_term[1]
    replacement_comments$source <- "YouTube"
    replacement_comments$collection_date <- as.character(Sys.Date())
    replacement_comments$collection_row <- seq_len(400)
    
    replacement_list[[j]] <- replacement_comments
    
    message("Saved 400 valid replacement comments")
  }

  unaffected_comments <- youtube_comments_raw[
    !youtube_comments_raw$selected_video_id %in%
      affected_video_ids,
  ]

  youtube_comments_repaired <- do.call(
    rbind,
    c(
      list(unaffected_comments),
      replacement_list
    )
  )

  row.names(youtube_comments_repaired) <- NULL

  youtube_comments_clean <- data.frame(
    comment_id = youtube_comments_repaired$id,
    artist = youtube_comments_repaired$artist,
    video_id = youtube_comments_repaired$selected_video_id,
    video_title = youtube_comments_repaired$video_title,
    search_term = youtube_comments_repaired$search_term,
    comment_text = trimws(
      youtube_comments_repaired$textOriginal
    ),
    author_channel_id =
      youtube_comments_repaired$authorChannelId.value,
    like_count = as.numeric(
      youtube_comments_repaired$likeCount
    ),
    published_at = as.character(
      youtube_comments_repaired$publishedAt
    ),
    collection_date =
      youtube_comments_repaired$collection_date,
    source = youtube_comments_repaired$source,
    stringsAsFactors = FALSE
  )

  comments_per_video_clean <- aggregate(
    comment_id ~ artist + video_id,
    data = youtube_comments_clean,
    FUN = length
  )

  names(comments_per_video_clean)[3] <- "data_points"

  final_validation <- data.frame(
    measure = c(
      "Clean comments",
      "Unique comment IDs",
      "Blank comment text",
      "Artists",
      "Videos",
      "Minimum comments per video",
      "Maximum comments per video"
    ),
    result = c(
      nrow(youtube_comments_clean),
      length(unique(youtube_comments_clean$comment_id)),
      sum(
        is.na(youtube_comments_clean$comment_text) |
          youtube_comments_clean$comment_text == ""
      ),
      length(unique(youtube_comments_clean$artist)),
      length(unique(youtube_comments_clean$video_id)),
      min(comments_per_video_clean$data_points),
      max(comments_per_video_clean$data_points)
    )
  )

  stopifnot(nrow(youtube_comments_clean) == 10800)
  stopifnot(
    length(unique(youtube_comments_clean$comment_id)) == 10800
  )
  stopifnot(
    all(
      !is.na(youtube_comments_clean$comment_text) &
        youtube_comments_clean$comment_text != ""
    )
  )
  stopifnot(all(table(youtube_comments_clean$artist) == 3600))
  stopifnot(all(comments_per_video_clean$data_points == 400))

  write.csv(
    youtube_comments_clean,
    "03_Clean_Data/Q2_YouTube_Comments_Clean.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  write.csv(
    final_validation,
    "03_Clean_Data/Q2_Final_Validation.csv",
    row.names = FALSE
  )

  save(
    youtube_comments_clean,
    final_validation,
    comments_per_video_clean,
    file = "04_RData/Q2_YouTube_Comments_Clean.RData"
  )

  final_validation
  table(youtube_comments_clean$artist)
  # ============================================================
  # QUESTION 3: ACTOR NETWORKS AND PAGERANK
  # Step 1: Check the available network variables
  # ============================================================

  q3_column_check <- data.frame(
    column_number = seq_along(names(youtube_comments_clean)),
    column_name = names(youtube_comments_clean)
  )

  q3_possible_network_columns <- names(youtube_comments_clean)[
    grepl(
      paste(
        c(
          "author",
          "actor",
          "channel",
          "parent",
          "reply",
          "comment",
          "id"
        ),
        collapse = "|"
      ),
      names(youtube_comments_clean),
      ignore.case = TRUE
    )
  ]

  print(q3_column_check)
  print(q3_possible_network_columns)
  invisible(list(
    youtube_comments_raw = youtube_comments_raw,
    youtube_comments_clean = youtube_comments_clean,
    selected_videos = selected_videos,
    validation_summary = validation_summary,
    final_validation = final_validation,
    comments_per_artist = comments_per_artist,
    comments_per_video = comments_per_video,
    comments_per_video_clean = comments_per_video_clean
  ))
}

if (isTRUE(getOption("q2.recollect", FALSE))) {
  invisible(list2env(q2_collect_live(), envir = environment()))
} else {
  source("scripts/bootstrap_data.R", local = environment())
}
