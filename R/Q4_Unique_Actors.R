# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 4: UNIQUE ACTORS
# ============================================================

# ------------------------------------------------------------
# 1. Load package and clean dataset
# ------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

if (!exists("youtube_comments_clean")) {
  load("04_RData/Q2_YouTube_Comments_Clean.RData")
}

stopifnot(exists("youtube_comments_clean"))
stopifnot(nrow(youtube_comments_clean) == 10800)

required_columns <- c(
  "artist",
  "comment_id",
  "author_channel_id"
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
# 2. Remove records without a valid actor ID
# ------------------------------------------------------------

q4_actor_data <- youtube_comments_clean[
  !is.na(youtube_comments_clean$author_channel_id) &
    trimws(youtube_comments_clean$author_channel_id) != "",
  required_columns
]

# ------------------------------------------------------------
# 3. Count comments made by each actor in each artist dataset
# ------------------------------------------------------------

q4_actor_activity <- aggregate(
  comment_id ~ artist + author_channel_id,
  data = q4_actor_data,
  FUN = length
)

names(q4_actor_activity)[3] <- "comments_made"

# ------------------------------------------------------------
# 4. Calculate unique and repeat actors
# ------------------------------------------------------------

artist_names <- c(
  "Taylor Swift",
  "Ariana Grande",
  "Billie Eilish"
)

calculate_actor_summary <- function(artist_name) {
  
  artist_comments <- q4_actor_data[
    q4_actor_data$artist == artist_name,
  ]
  
  artist_activity <- q4_actor_activity[
    q4_actor_activity$artist == artist_name,
  ]
  
  unique_actors <- length(
    unique(artist_comments$author_channel_id)
  )
  
  single_comment_actors <- sum(
    artist_activity$comments_made == 1
  )
  
  repeat_actors <- sum(
    artist_activity$comments_made > 1
  )
  
  data.frame(
    artist = artist_name,
    total_comments = nrow(artist_comments),
    unique_actors = unique_actors,
    single_comment_actors = single_comment_actors,
    repeat_actors = repeat_actors,
    repeat_actor_percentage = round(
      repeat_actors / unique_actors * 100,
      2
    ),
    comments_per_unique_actor = round(
      nrow(artist_comments) / unique_actors,
      2
    )
  )
}

q4_unique_actor_summary <- do.call(
  rbind,
  lapply(
    artist_names,
    calculate_actor_summary
  )
)

row.names(q4_unique_actor_summary) <- NULL

# ------------------------------------------------------------
# 5. Calculate cross-artist participation
# ------------------------------------------------------------

q4_cross_artist_activity <- aggregate(
  artist ~ author_channel_id,
  data = unique(
    q4_actor_data[, c(
      "author_channel_id",
      "artist"
    )]
  ),
  FUN = length
)

names(q4_cross_artist_activity)[2] <- "artists_commented_on"

q4_cross_artist_summary <- data.frame(
  measure = c(
    "Unique actors across all datasets",
    "Actors appearing in one artist dataset",
    "Actors appearing in two artist datasets",
    "Actors appearing in all three datasets"
  ),
  result = c(
    nrow(q4_cross_artist_activity),
    sum(
      q4_cross_artist_activity$artists_commented_on == 1
    ),
    sum(
      q4_cross_artist_activity$artists_commented_on == 2
    ),
    sum(
      q4_cross_artist_activity$artists_commented_on == 3
    )
  )
)

# ------------------------------------------------------------
# 6. Validate results
# ------------------------------------------------------------

stopifnot(
  all(q4_unique_actor_summary$total_comments == 3600)
)

stopifnot(
  all(
    q4_unique_actor_summary$unique_actors ==
      c(2657, 2932, 3174)
  )
)

stopifnot(
  all(
    q4_unique_actor_summary$single_comment_actors +
      q4_unique_actor_summary$repeat_actors ==
      q4_unique_actor_summary$unique_actors
  )
)

# ------------------------------------------------------------
# 7. Create unique-actor visualisation
# ------------------------------------------------------------

q4_unique_actor_plot <- ggplot(
  q4_unique_actor_summary,
  aes(
    x = reorder(
      artist,
      unique_actors
    ),
    y = unique_actors,
    fill = artist
  )
) +
  geom_col(
    width = 0.65,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = format(
        unique_actors,
        big.mark = ","
      )
    ),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(
        q4_unique_actor_summary$unique_actors
      ) * 1.12
    ),
    labels = scales::comma
  ) +
  labs(
    title = "Unique YouTube Actors by Artist",
    subtitle = "Based on 3,600 comments collected for each artist",
    x = "Artist",
    y = "Number of unique author channel IDs"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    axis.text.x = element_text(
      face = "bold"
    ),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = "05_Plots/Q4_Unique_Actors.png",
  plot = q4_unique_actor_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 8. Save Q4 evidence
# ------------------------------------------------------------

write.csv(
  q4_unique_actor_summary,
  "03_Clean_Data/Q4_Unique_Actor_Summary.csv",
  row.names = FALSE
)

write.csv(
  q4_cross_artist_summary,
  "03_Clean_Data/Q4_Cross_Artist_Actor_Summary.csv",
  row.names = FALSE
)

write.csv(
  q4_actor_activity,
  "03_Clean_Data/Q4_Actor_Activity.csv",
  row.names = FALSE
)

save(
  q4_actor_activity,
  q4_unique_actor_summary,
  q4_cross_artist_activity,
  q4_cross_artist_summary,
  q4_unique_actor_plot,
  file = "04_RData/Q4_Unique_Actors.RData"
)

# ------------------------------------------------------------
# 9. Display final Q4 results
# ------------------------------------------------------------

print(q4_unique_actor_summary)

print(q4_cross_artist_summary)

print(q4_unique_actor_plot)