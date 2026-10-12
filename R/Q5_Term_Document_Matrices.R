# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 5: TEXT PRE-PROCESSING, TERM-DOCUMENT MATRICES
# AND TOP 10 TERMS
# ============================================================

# ------------------------------------------------------------
# 1. Install and load required packages
# ------------------------------------------------------------

required_packages <- c(
  "tm",
  "slam",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tm)
library(slam)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 2. Load and validate the clean YouTube dataset
# ------------------------------------------------------------

if (!exists("youtube_comments_clean")) {
  load("04_RData/Q2_YouTube_Comments_Clean.RData")
}

stopifnot(exists("youtube_comments_clean"))
stopifnot(nrow(youtube_comments_clean) == 10800)

required_columns <- c(
  "comment_id",
  "artist",
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
# 3. Define text-cleaning functions
# ------------------------------------------------------------

remove_urls <- content_transformer(
  function(text) {
    gsub(
      "https?://\\S+|www\\.\\S+",
      " ",
      text
    )
  }
)

standardise_encoding <- content_transformer(
  function(text) {
    iconv(
      text,
      from = "",
      to = "ASCII",
      sub = " "
    )
  }
)

remove_repeated_spaces <- content_transformer(
  function(text) {
    gsub(
      "\\s+",
      " ",
      text
    )
  }
)

# Artist names and retrieval-related words are removed because
# they occur by design and would not provide useful insights.

q5_custom_stopwords <- c(
  "taylor",
  "swift",
  "ariana",
  "grande",
  "billie",
  "eilish",
  "official",
  "video",
  "videos",
  "music",
  "song",
  "songs",
  "lyric",
  "lyrics",
  "youtube",
  "http",
  "https",
  "www",
  "com",
  "amp",
  "quot"
)

# ------------------------------------------------------------
# 4. Function to preprocess one artist and create a TDM
# ------------------------------------------------------------

create_artist_tdm <- function(
    data,
    artist_name
) {
  
  artist_data <- data[
    data$artist == artist_name,
    c(
      "comment_id",
      "artist",
      "comment_text"
    )
  ]
  
  artist_data$comment_text[
    is.na(artist_data$comment_text)
  ] <- ""
  
  artist_corpus <- VCorpus(
    VectorSource(
      artist_data$comment_text
    )
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    standardise_encoding
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    remove_urls
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    content_transformer(tolower)
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    removePunctuation
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    removeNumbers
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    removeWords,
    stopwords("english")
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    removeWords,
    q5_custom_stopwords
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    remove_repeated_spaces
  )
  
  artist_corpus <- tm_map(
    artist_corpus,
    stripWhitespace
  )
  
  artist_tdm <- TermDocumentMatrix(
    artist_corpus,
    control = list(
      wordLengths = c(
        3,
        Inf
      )
    )
  )
  
  term_frequency <- row_sums(
    artist_tdm
  )
  
  term_frequency <- sort(
    term_frequency,
    decreasing = TRUE
  )
  
  top10 <- head(
    term_frequency,
    10
  )
  
  top10_table <- data.frame(
    artist = artist_name,
    rank = seq_along(top10),
    term = names(top10),
    frequency = as.numeric(top10),
    row.names = NULL
  )
  
  cleaned_text <- vapply(
    artist_corpus,
    content,
    character(1)
  )
  
  cleaned_comments <- data.frame(
    comment_id = artist_data$comment_id,
    artist = artist_name,
    cleaned_text = cleaned_text,
    stringsAsFactors = FALSE
  )
  
  list(
    corpus = artist_corpus,
    tdm = artist_tdm,
    term_frequency = term_frequency,
    top10 = top10_table,
    cleaned_comments = cleaned_comments
  )
}

# ------------------------------------------------------------
# 5. Create one corpus and TDM for each artist
# ------------------------------------------------------------

q5_taylor <- create_artist_tdm(
  youtube_comments_clean,
  "Taylor Swift"
)

q5_ariana <- create_artist_tdm(
  youtube_comments_clean,
  "Ariana Grande"
)

q5_billie <- create_artist_tdm(
  youtube_comments_clean,
  "Billie Eilish"
)

q5_top10_terms <- rbind(
  q5_taylor$top10,
  q5_ariana$top10,
  q5_billie$top10
)

row.names(q5_top10_terms) <- NULL

q5_cleaned_comments <- rbind(
  q5_taylor$cleaned_comments,
  q5_ariana$cleaned_comments,
  q5_billie$cleaned_comments
)

row.names(q5_cleaned_comments) <- NULL

# ------------------------------------------------------------
# 6. Produce a TDM summary
# ------------------------------------------------------------

q5_tdm_summary <- data.frame(
  artist = c(
    "Taylor Swift",
    "Ariana Grande",
    "Billie Eilish"
  ),
  documents = c(
    ncol(q5_taylor$tdm),
    ncol(q5_ariana$tdm),
    ncol(q5_billie$tdm)
  ),
  unique_terms = c(
    nrow(q5_taylor$tdm),
    nrow(q5_ariana$tdm),
    nrow(q5_billie$tdm)
  ),
  total_term_occurrences = c(
    sum(q5_taylor$term_frequency),
    sum(q5_ariana$term_frequency),
    sum(q5_billie$term_frequency)
  ),
  empty_after_cleaning = c(
    sum(
      trimws(q5_taylor$cleaned_comments$cleaned_text) == ""
    ),
    sum(
      trimws(q5_ariana$cleaned_comments$cleaned_text) == ""
    ),
    sum(
      trimws(q5_billie$cleaned_comments$cleaned_text) == ""
    )
  )
)

# ------------------------------------------------------------
# 7. Validate the matrices and top-term results
# ------------------------------------------------------------

stopifnot(
  all(q5_tdm_summary$documents == 3600)
)

stopifnot(
  nrow(q5_top10_terms) == 30
)

stopifnot(
  all(table(q5_top10_terms$artist) == 10)
)

stopifnot(
  !any(
    q5_top10_terms$term %in%
      c(
        stopwords("english"),
        q5_custom_stopwords
      )
  )
)

# ------------------------------------------------------------
# 8. Create the top-ten comparison visualisation
# ------------------------------------------------------------

q5_plot_data <- q5_top10_terms

q5_plot_data$term_label <- paste(
  q5_plot_data$artist,
  q5_plot_data$term,
  sep = " - "
)

q5_plot_data$term_label <- factor(
  q5_plot_data$term_label,
  levels = rev(
    q5_plot_data$term_label
  )
)

q5_top10_plot <- ggplot(
  q5_plot_data,
  aes(
    x = term_label,
    y = frequency,
    fill = artist
  )
) +
  geom_col(
    width = 0.72,
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
        "^.* - ",
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
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Ten Most Frequent Terms by Artist",
    subtitle = paste(
      "After removing URLs, punctuation, numbers,",
      "English stop words, artist names and search terms"
    ),
    x = "Term",
    y = "Frequency"
  ) +
  theme_minimal(
    base_size = 12
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
  filename = "05_Plots/Q5_Top10_Terms.png",
  plot = q5_top10_plot,
  width = 10,
  height = 11,
  dpi = 300
)

# ------------------------------------------------------------
# 9. Save all Q5 evidence
# ------------------------------------------------------------

write.csv(
  q5_cleaned_comments,
  "03_Clean_Data/Q5_Preprocessed_Comments.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q5_top10_terms,
  "03_Clean_Data/Q5_Top10_Terms.csv",
  row.names = FALSE
)

write.csv(
  q5_tdm_summary,
  "03_Clean_Data/Q5_TDM_Summary.csv",
  row.names = FALSE
)

save(
  q5_taylor,
  q5_ariana,
  q5_billie,
  q5_cleaned_comments,
  q5_top10_terms,
  q5_tdm_summary,
  q5_top10_plot,
  file = "04_RData/Q5_Term_Document_Matrices.RData"
)

# ------------------------------------------------------------
# 10. Display final Q5 results
# ------------------------------------------------------------

print(q5_tdm_summary)

print(
  q5_top10_terms,
  row.names = FALSE
)

print(q5_top10_plot)
# ============================================================
# QUESTION 5 CORRECTION:
# REMOVE INFORMAL AND CONVERSATIONAL FILLER WORDS
# ============================================================

q5_custom_stopwords <- c(
  # Artist names
  "taylor",
  "swift",
  "ariana",
  "grande",
  "billie",
  "eilish",
  
  # Search and platform terms
  "official",
  "video",
  "videos",
  "music",
  "song",
  "songs",
  "lyric",
  "lyrics",
  "youtube",
  "http",
  "https",
  "www",
  "com",
  "amp",
  "quot",
  
  # Informal contractions
  "im",
  "ive",
  "id",
  "ill",
  "youre",
  "youve",
  "youll",
  "theyre",
  "theyve",
  "thats",
  "theres",
  "isnt",
  "wasnt",
  "werent",
  "doesnt",
  "didnt",
  "dont",
  "wont",
  "cant",
  "couldnt",
  "wouldnt",
  "shouldnt",
  
  # Conversational filler terms
  "like",
  "just",
  "one",
  "much",
  "can",
  "will",
  "know",
  "want",
  "get",
  "got",
  "really",
  "thing",
  "things",
  "people",
  "even",
  "still",
  "way",
  "make",
  "made",
  "see",
  "say",
  "says",
  "think",
  "now",
  "also"
)

# Recreate all three corpora and matrices using the improved
# custom stop-word list.

q5_taylor <- create_artist_tdm(
  youtube_comments_clean,
  "Taylor Swift"
)

q5_ariana <- create_artist_tdm(
  youtube_comments_clean,
  "Ariana Grande"
)

q5_billie <- create_artist_tdm(
  youtube_comments_clean,
  "Billie Eilish"
)

q5_top10_terms <- rbind(
  q5_taylor$top10,
  q5_ariana$top10,
  q5_billie$top10
)

row.names(q5_top10_terms) <- NULL

q5_cleaned_comments <- rbind(
  q5_taylor$cleaned_comments,
  q5_ariana$cleaned_comments,
  q5_billie$cleaned_comments
)

row.names(q5_cleaned_comments) <- NULL

# Recreate the matrix summary.

q5_tdm_summary <- data.frame(
  artist = c(
    "Taylor Swift",
    "Ariana Grande",
    "Billie Eilish"
  ),
  documents = c(
    ncol(q5_taylor$tdm),
    ncol(q5_ariana$tdm),
    ncol(q5_billie$tdm)
  ),
  unique_terms = c(
    nrow(q5_taylor$tdm),
    nrow(q5_ariana$tdm),
    nrow(q5_billie$tdm)
  ),
  total_term_occurrences = c(
    sum(q5_taylor$term_frequency),
    sum(q5_ariana$term_frequency),
    sum(q5_billie$term_frequency)
  ),
  empty_after_cleaning = c(
    sum(
      trimws(
        q5_taylor$cleaned_comments$cleaned_text
      ) == ""
    ),
    sum(
      trimws(
        q5_ariana$cleaned_comments$cleaned_text
      ) == ""
    ),
    sum(
      trimws(
        q5_billie$cleaned_comments$cleaned_text
      ) == ""
    )
  )
)

# Validate the corrected results.

stopifnot(
  all(q5_tdm_summary$documents == 3600)
)

stopifnot(
  nrow(q5_top10_terms) == 30
)

stopifnot(
  all(table(q5_top10_terms$artist) == 10)
)

stopifnot(
  !any(
    q5_top10_terms$term %in%
      c(
        stopwords("english"),
        q5_custom_stopwords
      )
  )
)

# Recreate the corrected plot.

q5_plot_data <- q5_top10_terms

q5_plot_data$term_label <- paste(
  q5_plot_data$artist,
  q5_plot_data$term,
  sep = " - "
)

q5_plot_data$term_label <- factor(
  q5_plot_data$term_label,
  levels = rev(
    q5_plot_data$term_label
  )
)

q5_top10_plot <- ggplot(
  q5_plot_data,
  aes(
    x = term_label,
    y = frequency,
    fill = artist
  )
) +
  geom_col(
    width = 0.72,
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
        "^.* - ",
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
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Ten Most Frequent Meaningful Terms by Artist",
    subtitle = paste(
      "Results after removing English stop words,",
      "artist names, search terms and conversational filler"
    ),
    x = "Term",
    y = "Frequency"
  ) +
  theme_minimal(
    base_size = 12
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

# Overwrite the earlier files with corrected outputs.

ggsave(
  filename = "05_Plots/Q5_Top10_Terms.png",
  plot = q5_top10_plot,
  width = 10,
  height = 11,
  dpi = 300
)

write.csv(
  q5_cleaned_comments,
  "03_Clean_Data/Q5_Preprocessed_Comments.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q5_top10_terms,
  "03_Clean_Data/Q5_Top10_Terms.csv",
  row.names = FALSE
)

write.csv(
  q5_tdm_summary,
  "03_Clean_Data/Q5_TDM_Summary.csv",
  row.names = FALSE
)

save(
  q5_taylor,
  q5_ariana,
  q5_billie,
  q5_cleaned_comments,
  q5_top10_terms,
  q5_tdm_summary,
  q5_top10_plot,
  file = "04_RData/Q5_Term_Document_Matrices.RData"
)

print(q5_tdm_summary)

print(
  q5_top10_terms,
  row.names = FALSE
)

print(q5_top10_plot)