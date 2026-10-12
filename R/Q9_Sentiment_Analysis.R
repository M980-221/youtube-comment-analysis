# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 9: SENTIMENT ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 1. Install and load packages
# ------------------------------------------------------------

required_packages <- c(
  "sentimentr",
  "syuzhet",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(sentimentr)
library(syuzhet)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 2. Load and validate the clean comment dataset
# ------------------------------------------------------------

if (!exists("youtube_comments_clean")) {
  load(
    "04_RData/Q2_YouTube_Comments_Clean.RData"
  )
}

stopifnot(exists("youtube_comments_clean"))
stopifnot(nrow(youtube_comments_clean) == 10800)

required_columns <- c(
  "comment_id",
  "artist",
  "video_id",
  "video_title",
  "comment_text"
)

stopifnot(
  all(required_columns %in% names(youtube_comments_clean))
)

dir.create(
  "03_Clean_Data",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "04_RData",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "05_Plots",
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# 3. Prepare original text for sentiment analysis
# ------------------------------------------------------------

q9_sentiment_data <- youtube_comments_clean[
  ,
  required_columns
]

q9_sentiment_data$comment_text[
  is.na(q9_sentiment_data$comment_text)
] <- ""

q9_sentiment_data$comment_text <- iconv(
  q9_sentiment_data$comment_text,
  from = "",
  to = "UTF-8",
  sub = " "
)

# ------------------------------------------------------------
# 4. Calculate negation-aware sentiment scores
# ------------------------------------------------------------

message(
  "Calculating comment sentiment scores..."
)

q9_sentiment_result <- sentiment_by(
  q9_sentiment_data$comment_text
)

q9_sentiment_data$sentiment_score <-
  q9_sentiment_result$ave_sentiment[
    match(
      seq_len(
        nrow(q9_sentiment_data)
      ),
      q9_sentiment_result$element_id
    )
  ]

q9_sentiment_data$sentiment_score[
  is.na(q9_sentiment_data$sentiment_score)
] <- 0

# A small neutral range prevents extremely weak scores from
# being incorrectly classified as clearly positive or negative.

q9_positive_threshold <- 0.05
q9_negative_threshold <- -0.05

q9_sentiment_data$sentiment_category <- ifelse(
  q9_sentiment_data$sentiment_score >
    q9_positive_threshold,
  "Positive",
  ifelse(
    q9_sentiment_data$sentiment_score <
      q9_negative_threshold,
    "Negative",
    "Neutral"
  )
)

q9_sentiment_data$sentiment_category <- factor(
  q9_sentiment_data$sentiment_category,
  levels = c(
    "Negative",
    "Neutral",
    "Positive"
  )
)

# ------------------------------------------------------------
# 5. Create sentiment distribution by artist
# ------------------------------------------------------------

q9_sentiment_distribution <- as.data.frame(
  table(
    artist = q9_sentiment_data$artist,
    sentiment_category =
      q9_sentiment_data$sentiment_category
  )
)

names(q9_sentiment_distribution)[3] <- "comments"

q9_sentiment_distribution$total_comments <-
  ave(
    q9_sentiment_distribution$comments,
    q9_sentiment_distribution$artist,
    FUN = sum
  )

q9_sentiment_distribution$percentage <- round(
  q9_sentiment_distribution$comments /
    q9_sentiment_distribution$total_comments *
    100,
  2
)

# ------------------------------------------------------------
# 6. Create detailed artist-level summary
# ------------------------------------------------------------

create_artist_sentiment_summary <- function(
    artist_name
) {
  
  artist_data <- q9_sentiment_data[
    q9_sentiment_data$artist == artist_name,
  ]
  
  data.frame(
    artist = artist_name,
    total_comments = nrow(
      artist_data
    ),
    mean_sentiment = mean(
      artist_data$sentiment_score
    ),
    median_sentiment = median(
      artist_data$sentiment_score
    ),
    standard_deviation = sd(
      artist_data$sentiment_score
    ),
    positive_comments = sum(
      artist_data$sentiment_category ==
        "Positive"
    ),
    neutral_comments = sum(
      artist_data$sentiment_category ==
        "Neutral"
    ),
    negative_comments = sum(
      artist_data$sentiment_category ==
        "Negative"
    ),
    positive_percentage = round(
      mean(
        artist_data$sentiment_category ==
          "Positive"
      ) *
        100,
      2
    ),
    neutral_percentage = round(
      mean(
        artist_data$sentiment_category ==
          "Neutral"
      ) *
        100,
      2
    ),
    negative_percentage = round(
      mean(
        artist_data$sentiment_category ==
          "Negative"
      ) *
        100,
      2
    )
  )
}

artist_names <- c(
  "Taylor Swift",
  "Ariana Grande",
  "Billie Eilish"
)

q9_artist_summary <- do.call(
  rbind,
  lapply(
    artist_names,
    create_artist_sentiment_summary
  )
)

row.names(q9_artist_summary) <- NULL

# ------------------------------------------------------------
# 7. Calculate sentiment for each of the 27 videos
# ------------------------------------------------------------

q9_video_groups <- split(
  q9_sentiment_data,
  list(
    q9_sentiment_data$artist,
    q9_sentiment_data$video_id
  ),
  drop = TRUE
)

q9_video_summary <- do.call(
  rbind,
  lapply(
    q9_video_groups,
    function(video_data) {
      
      data.frame(
        artist = video_data$artist[1],
        video_id = video_data$video_id[1],
        video_title = video_data$video_title[1],
        comments = nrow(
          video_data
        ),
        mean_sentiment = mean(
          video_data$sentiment_score
        ),
        positive_percentage = round(
          mean(
            video_data$sentiment_category ==
              "Positive"
          ) *
            100,
          2
        ),
        neutral_percentage = round(
          mean(
            video_data$sentiment_category ==
              "Neutral"
          ) *
            100,
          2
        ),
        negative_percentage = round(
          mean(
            video_data$sentiment_category ==
              "Negative"
          ) *
            100,
          2
        )
      )
    }
  )
)

row.names(q9_video_summary) <- NULL

q9_video_summary <- q9_video_summary[
  order(
    q9_video_summary$artist,
    -q9_video_summary$mean_sentiment
  ),
]

# ------------------------------------------------------------
# 8. Find the most positive and negative video per artist
# ------------------------------------------------------------

get_video_extremes <- function(
    artist_name
) {
  
  artist_videos <- q9_video_summary[
    q9_video_summary$artist == artist_name,
  ]
  
  most_positive <- artist_videos[
    which.max(
      artist_videos$mean_sentiment
    ),
  ]
  
  most_negative <- artist_videos[
    which.min(
      artist_videos$mean_sentiment
    ),
  ]
  
  rbind(
    data.frame(
      artist = artist_name,
      result = "Most positive video",
      video_id = most_positive$video_id,
      video_title = most_positive$video_title,
      mean_sentiment =
        most_positive$mean_sentiment,
      positive_percentage =
        most_positive$positive_percentage,
      negative_percentage =
        most_positive$negative_percentage
    ),
    data.frame(
      artist = artist_name,
      result = "Least positive video",
      video_id = most_negative$video_id,
      video_title = most_negative$video_title,
      mean_sentiment =
        most_negative$mean_sentiment,
      positive_percentage =
        most_negative$positive_percentage,
      negative_percentage =
        most_negative$negative_percentage
    )
  )
}

q9_video_extremes <- do.call(
  rbind,
  lapply(
    artist_names,
    get_video_extremes
  )
)

row.names(q9_video_extremes) <- NULL

# ------------------------------------------------------------
# 9. Calculate NRC emotions
# ------------------------------------------------------------

message(
  "Calculating NRC emotions..."
)

q9_nrc_scores <- get_nrc_sentiment(
  q9_sentiment_data$comment_text,
  language = "english"
)

stopifnot(
  nrow(q9_nrc_scores) ==
    nrow(q9_sentiment_data)
)

q9_nrc_scores$comment_id <-
  q9_sentiment_data$comment_id

q9_nrc_scores$artist <-
  q9_sentiment_data$artist

emotion_columns <- c(
  "anger",
  "anticipation",
  "disgust",
  "fear",
  "joy",
  "sadness",
  "surprise",
  "trust"
)

q9_emotion_summary <- aggregate(
  q9_nrc_scores[
    ,
    emotion_columns
  ],
  by = list(
    artist = q9_nrc_scores$artist
  ),
  FUN = sum
)

q9_emotion_long <- reshape(
  q9_emotion_summary,
  varying = emotion_columns,
  v.names = "emotion_count",
  timevar = "emotion",
  times = emotion_columns,
  direction = "long"
)

row.names(q9_emotion_long) <- NULL

q9_emotion_long$emotions_per_1000_comments <-
  round(
    q9_emotion_long$emotion_count /
      3600 *
      1000,
    2
  )

q9_emotion_long <- q9_emotion_long[
  order(
    q9_emotion_long$artist,
    -q9_emotion_long$emotion_count
  ),
]

row.names(q9_emotion_long) <- NULL

# ------------------------------------------------------------
# 10. Validate sentiment results
# ------------------------------------------------------------

stopifnot(
  nrow(q9_sentiment_data) == 10800
)

stopifnot(
  all(
    table(
      q9_sentiment_data$artist
    ) == 3600
  )
)

stopifnot(
  !any(
    is.na(
      q9_sentiment_data$sentiment_score
    )
  )
)

stopifnot(
  nrow(q9_video_summary) == 27
)

stopifnot(
  all(
    q9_video_summary$comments == 400
  )
)

stopifnot(
  all(
    abs(
      q9_sentiment_distribution$percentage[
        q9_sentiment_distribution$artist ==
          "Taylor Swift"
      ] |>
        sum() -
        100
    ) < 0.1
  )
)

# ------------------------------------------------------------
# 11. Plot sentiment distribution by artist
# ------------------------------------------------------------

q9_sentiment_distribution_plot <- ggplot(
  q9_sentiment_distribution,
  aes(
    x = artist,
    y = percentage,
    fill = sentiment_category
  )
) +
  geom_col(
    width = 0.68
  ) +
  geom_text(
    aes(
      label = ifelse(
        percentage >= 5,
        paste0(
          percentage,
          "%"
        ),
        ""
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    colour = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Negative" = "#E15759",
      "Neutral" = "#BAB0AC",
      "Positive" = "#59A14F"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      scale = 1
    ),
    limits = c(
      0,
      100
    )
  ) +
  labs(
    title = "YouTube Comment Sentiment by Artist",
    subtitle = "Based on 3,600 comments for each artist",
    x = "Artist",
    y = "Percentage of comments",
    fill = "Sentiment"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "05_Plots/Q9_Sentiment_Distribution.png",
  plot = q9_sentiment_distribution_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 12. Plot mean sentiment across all videos
# ------------------------------------------------------------

q9_video_plot_data <- q9_video_summary

q9_video_plot_data$short_title <- gsub(
  "^(Taylor Swift|Ariana Grande|Billie Eilish)[[:space:]]*[-–][[:space:]]*",
  "",
  q9_video_plot_data$video_title
)

q9_video_plot_data$short_title <- substr(
  q9_video_plot_data$short_title,
  1,
  48
)

q9_video_plot_data$video_label <- paste(
  q9_video_plot_data$artist,
  q9_video_plot_data$short_title,
  sep = " | "
)

q9_video_plot_data$video_label <- factor(
  q9_video_plot_data$video_label,
  levels = q9_video_plot_data$video_label[
    order(
      q9_video_plot_data$mean_sentiment
    )
  ]
)

q9_video_sentiment_plot <- ggplot(
  q9_video_plot_data,
  aes(
    x = video_label,
    y = mean_sentiment,
    fill = artist
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  coord_flip() +
  facet_wrap(
    ~ artist,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_discrete(
    labels = function(value) {
      sub(
        "^.*\\| ",
        "",
        value
      )
    }
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey30",
    linewidth = 0.4
  ) +
  labs(
    title = "Mean Audience Sentiment by Video",
    subtitle = "Higher scores indicate more positive reactions",
    x = "Selected YouTube video",
    y = "Mean sentiment score"
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = "05_Plots/Q9_Video_Sentiment.png",
  plot = q9_video_sentiment_plot,
  width = 12,
  height = 12,
  dpi = 300
)

# ------------------------------------------------------------
# 13. Plot NRC emotions
# ------------------------------------------------------------

q9_emotion_plot <- ggplot(
  q9_emotion_long,
  aes(
    x = emotion,
    y = emotions_per_1000_comments,
    fill = artist
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.78
    ),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
  ) +
  labs(
    title = "Emotional Reactions in YouTube Comments",
    subtitle = "NRC emotion occurrences per 1,000 comments",
    x = "Emotion",
    y = "Occurrences per 1,000 comments",
    fill = "Artist"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "05_Plots/Q9_NRC_Emotions.png",
  plot = q9_emotion_plot,
  width = 11,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 14. Export all Q9 evidence
# ------------------------------------------------------------

write.csv(
  q9_sentiment_data,
  "03_Clean_Data/Q9_Comment_Sentiment.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q9_artist_summary,
  "03_Clean_Data/Q9_Artist_Sentiment_Summary.csv",
  row.names = FALSE
)

write.csv(
  q9_sentiment_distribution,
  "03_Clean_Data/Q9_Sentiment_Distribution.csv",
  row.names = FALSE
)

write.csv(
  q9_video_summary,
  "03_Clean_Data/Q9_Video_Sentiment_Summary.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q9_video_extremes,
  "03_Clean_Data/Q9_Video_Sentiment_Extremes.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q9_emotion_long,
  "03_Clean_Data/Q9_NRC_Emotion_Summary.csv",
  row.names = FALSE
)

save(
  q9_sentiment_data,
  q9_artist_summary,
  q9_sentiment_distribution,
  q9_video_summary,
  q9_video_extremes,
  q9_nrc_scores,
  q9_emotion_summary,
  q9_emotion_long,
  q9_sentiment_distribution_plot,
  q9_video_sentiment_plot,
  q9_emotion_plot,
  file = "04_RData/Q9_Sentiment_Analysis.RData"
)

# ------------------------------------------------------------
# 15. Display final Q9 results
# ------------------------------------------------------------

print(q9_artist_summary)

print(q9_video_extremes)

print(q9_emotion_long)

print(q9_sentiment_distribution_plot)
# ============================================================
# QUESTION 9: CORRECT SENTIMENT DISTRIBUTION PLOT
# ============================================================

q9_sentiment_distribution_plot <- ggplot(
  q9_sentiment_distribution,
  aes(
    x = artist,
    y = percentage,
    fill = sentiment_category
  )
) +
  geom_col(
    width = 0.68
  ) +
  geom_text(
    aes(
      label = ifelse(
        percentage >= 5,
        paste0(
          percentage,
          "%"
        ),
        ""
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    colour = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Negative" = "#E15759",
      "Neutral" = "#BAB0AC",
      "Positive" = "#59A14F"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      scale = 1
    ),
    breaks = seq(
      0,
      100,
      25
    ),
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    ),
    clip = "off"
  ) +
  labs(
    title = "YouTube Comment Sentiment by Artist",
    subtitle = "Based on 3,600 comments for each artist",
    x = "Artist",
    y = "Percentage of comments",
    fill = "Sentiment"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "05_Plots/Q9_Sentiment_Distribution.png",
  plot = q9_sentiment_distribution_plot,
  width = 10,
  height = 6,
  dpi = 300
)

save(
  q9_sentiment_data,
  q9_artist_summary,
  q9_sentiment_distribution,
  q9_video_summary,
  q9_video_extremes,
  q9_nrc_scores,
  q9_emotion_summary,
  q9_emotion_long,
  q9_sentiment_distribution_plot,
  q9_video_sentiment_plot,
  q9_emotion_plot,
  file = "04_RData/Q9_Sentiment_Analysis.RData"
)

print(q9_artist_summary)

print(q9_video_extremes)

print(q9_emotion_long)

print(q9_sentiment_distribution_plot)