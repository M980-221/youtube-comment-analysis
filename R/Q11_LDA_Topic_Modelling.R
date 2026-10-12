# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 11: LDA TOPIC MODELLING
# ============================================================

# ------------------------------------------------------------
# 1. Install and load packages
# ------------------------------------------------------------

required_packages <- c(
  "tm",
  "slam",
  "topicmodels",
  "tidytext",
  "dplyr",
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
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 2. Load Q5 preprocessed comments
# ------------------------------------------------------------

if (!exists("q5_cleaned_comments")) {
  load(
    "04_RData/Q5_Term_Document_Matrices.RData"
  )
}

stopifnot(exists("q5_cleaned_comments"))
stopifnot(nrow(q5_cleaned_comments) == 10800)

required_columns <- c(
  "comment_id",
  "artist",
  "cleaned_text"
)

stopifnot(
  all(required_columns %in% names(q5_cleaned_comments))
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
# 3. LDA function for one artist
# ------------------------------------------------------------

create_artist_lda <- function(
    data,
    artist_name,
    number_of_topics = 3
) {
  
  artist_data <- data[
    data$artist == artist_name,
    c(
      "comment_id",
      "artist",
      "cleaned_text"
    )
  ]
  
  artist_data$cleaned_text[
    is.na(artist_data$cleaned_text)
  ] <- ""
  
  artist_corpus <- VCorpus(
    VectorSource(
      artist_data$cleaned_text
    )
  )
  
  artist_dtm <- DocumentTermMatrix(
    artist_corpus,
    control = list(
      wordLengths = c(
        3,
        Inf
      )
    )
  )

  # Keep the original document labels before empty rows are removed.
  # A retained document label is not its position in the filtered matrix.
  document_lookup <- data.frame(
    document = as.character(rownames(artist_dtm)),
    comment_id = as.character(artist_data$comment_id),
    stringsAsFactors = FALSE
  )
  stopifnot(
    nrow(document_lookup) == nrow(artist_data),
    !anyNA(document_lookup),
    all(nzchar(document_lookup$document)),
    all(nzchar(document_lookup$comment_id)),
    !anyDuplicated(document_lookup$document),
    !anyDuplicated(document_lookup$comment_id)
  )
  
  # Remove terms occurring fewer than five times.
  # This reduces rare-word noise and improves topic stability.
  
  artist_term_frequency <- col_sums(
    artist_dtm
  )
  
  retained_terms <- names(
    artist_term_frequency[
      artist_term_frequency >= 5
    ]
  )
  
  artist_dtm <- artist_dtm[
    ,
    retained_terms
  ]
  
  # Remove documents that became empty after preprocessing.
  
  nonempty_documents <- row_sums(
    artist_dtm
  ) > 0
  
  lda_dtm <- artist_dtm[
    nonempty_documents,
  ]
  
  retained_lookup <- document_lookup[
    match(as.character(rownames(lda_dtm)), document_lookup$document),
    , drop = FALSE
  ]
  stopifnot(
    !anyNA(retained_lookup),
    nrow(retained_lookup) == nrow(lda_dtm)
  )
  
  stopifnot(
    nrow(lda_dtm) > 0
  )
  
  stopifnot(
    ncol(lda_dtm) > number_of_topics
  )
  
  set.seed(3032)
  
  lda_model <- LDA(
    lda_dtm,
    k = number_of_topics,
    method = "Gibbs",
    control = list(
      seed = 3032,
      burnin = 500,
      iter = 1200,
      thin = 50
    )
  )
  
  beta_table <- tidy(
    lda_model,
    matrix = "beta"
  )
  
  gamma_table <- tidy(
    lda_model,
    matrix = "gamma"
  )
  
  gamma_table$comment_id <- retained_lookup$comment_id[
    match(as.character(gamma_table$document), retained_lookup$document)
  ]

  # Every document must map to exactly one original comment, with no gaps.
  gamma_mapping <- unique(as.data.frame(
    gamma_table[, c("document", "comment_id")]
  ))
  expected_comment_ids <- document_lookup$comment_id[
    match(as.character(gamma_table$document), document_lookup$document)
  ]
  stopifnot(
    !anyNA(gamma_table$comment_id),
    all(nzchar(gamma_table$comment_id)),
    nrow(gamma_mapping) == nrow(lda_dtm),
    !anyDuplicated(gamma_mapping$document),
    !anyDuplicated(gamma_mapping$comment_id),
    identical(as.character(gamma_table$comment_id), expected_comment_ids)
  )
  
  gamma_table$artist <- artist_name
  
  top_terms <- beta_table |>
    group_by(topic) |>
    slice_max(
      order_by = beta,
      n = 10,
      with_ties = FALSE
    ) |>
    arrange(
      topic,
      desc(beta)
    ) |>
    ungroup()
  
  top_terms$artist <- artist_name
  
  dominant_topics <- gamma_table |>
    group_by(document) |>
    slice_max(
      order_by = gamma,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup()
  
  dominant_counts <- dominant_topics |>
    count(
      topic,
      name = "dominant_documents"
    )
  
  topic_prevalence <- gamma_table |>
    group_by(topic) |>
    summarise(
      mean_topic_probability = mean(gamma),
      median_topic_probability = median(gamma),
      .groups = "drop"
    ) |>
    left_join(
      dominant_counts,
      by = "topic"
    )
  
  topic_prevalence$artist <- artist_name
  
  topic_term_labels <- top_terms |>
    group_by(topic) |>
    summarise(
      top_terms = paste(
        term,
        collapse = ", "
      ),
      .groups = "drop"
    )
  
  topic_summary <- topic_prevalence |>
    left_join(
      topic_term_labels,
      by = "topic"
    )
  
  model_summary <- data.frame(
    artist = artist_name,
    original_documents = nrow(
      artist_data
    ),
    documents_used = nrow(
      lda_dtm
    ),
    empty_documents_removed =
      nrow(artist_data) -
      nrow(lda_dtm),
    retained_terms = ncol(
      lda_dtm
    ),
    retained_tokens = sum(
      lda_dtm
    ),
    topics = number_of_topics,
    log_likelihood = as.numeric(
      logLik(
        lda_model
      )
    ),
    perplexity = perplexity(
      lda_model,
      newdata = lda_dtm
    )
  )
  
  list(
    model = lda_model,
    dtm = lda_dtm,
    beta = beta_table,
    gamma = gamma_table,
    top_terms = top_terms,
    dominant_topics = dominant_topics,
    topic_prevalence = topic_prevalence,
    topic_summary = topic_summary,
    model_summary = model_summary
  )
}

# ------------------------------------------------------------
# 4. Run three-topic LDA for all artists
# ------------------------------------------------------------

message(
  "Running Taylor Swift LDA model..."
)

q11_taylor <- create_artist_lda(
  q5_cleaned_comments,
  "Taylor Swift",
  number_of_topics = 3
)

message(
  "Running Ariana Grande LDA model..."
)

q11_ariana <- create_artist_lda(
  q5_cleaned_comments,
  "Ariana Grande",
  number_of_topics = 3
)

message(
  "Running Billie Eilish LDA model..."
)

q11_billie <- create_artist_lda(
  q5_cleaned_comments,
  "Billie Eilish",
  number_of_topics = 3
)

# ------------------------------------------------------------
# 5. Combine all model results
# ------------------------------------------------------------

q11_model_summary <- rbind(
  q11_taylor$model_summary,
  q11_ariana$model_summary,
  q11_billie$model_summary
)

row.names(q11_model_summary) <- NULL

q11_top_terms <- rbind(
  q11_taylor$top_terms,
  q11_ariana$top_terms,
  q11_billie$top_terms
)

row.names(q11_top_terms) <- NULL

q11_topic_prevalence <- rbind(
  q11_taylor$topic_prevalence,
  q11_ariana$topic_prevalence,
  q11_billie$topic_prevalence
)

row.names(q11_topic_prevalence) <- NULL

q11_topic_summary <- rbind(
  q11_taylor$topic_summary,
  q11_ariana$topic_summary,
  q11_billie$topic_summary
)

row.names(q11_topic_summary) <- NULL

q11_document_topics <- rbind(
  q11_taylor$gamma,
  q11_ariana$gamma,
  q11_billie$gamma
)

row.names(q11_document_topics) <- NULL

q11_dominant_topics <- rbind(
  q11_taylor$dominant_topics,
  q11_ariana$dominant_topics,
  q11_billie$dominant_topics
)

row.names(q11_dominant_topics) <- NULL

# ------------------------------------------------------------
# 6. Create topic labels
# ------------------------------------------------------------

q11_topic_summary$topic_label <- paste(
  q11_topic_summary$artist,
  paste0(
    "Topic ",
    q11_topic_summary$topic
  ),
  sep = " - "
)

q11_topic_prevalence$topic_label <- paste(
  q11_topic_prevalence$artist,
  paste0(
    "Topic ",
    q11_topic_prevalence$topic
  ),
  sep = " - "
)

q11_top_terms$topic_label <- paste(
  q11_top_terms$artist,
  paste0(
    "Topic ",
    q11_top_terms$topic
  ),
  sep = " - "
)

# ------------------------------------------------------------
# 7. Compare important terms between artists
# ------------------------------------------------------------

get_artist_topic_terms <- function(
    artist_name
) {
  
  unique(
    q11_top_terms$term[
      q11_top_terms$artist == artist_name
    ]
  )
}

q11_taylor_terms <- get_artist_topic_terms(
  "Taylor Swift"
)

q11_ariana_terms <- get_artist_topic_terms(
  "Ariana Grande"
)

q11_billie_terms <- get_artist_topic_terms(
  "Billie Eilish"
)

create_term_overlap <- function(
    first_artist,
    second_artist,
    first_terms,
    second_terms
) {
  
  shared_terms <- intersect(
    first_terms,
    second_terms
  )
  
  data.frame(
    comparison = paste(
      first_artist,
      "vs",
      second_artist
    ),
    shared_terms = length(
      shared_terms
    ),
    shared_term_names = ifelse(
      length(shared_terms) == 0,
      "None",
      paste(
        shared_terms,
        collapse = ", "
      )
    )
  )
}

q11_term_overlap <- rbind(
  create_term_overlap(
    "Taylor Swift",
    "Ariana Grande",
    q11_taylor_terms,
    q11_ariana_terms
  ),
  create_term_overlap(
    "Taylor Swift",
    "Billie Eilish",
    q11_taylor_terms,
    q11_billie_terms
  ),
  create_term_overlap(
    "Ariana Grande",
    "Billie Eilish",
    q11_ariana_terms,
    q11_billie_terms
  )
)

row.names(q11_term_overlap) <- NULL

# ------------------------------------------------------------
# 8. Validate results
# ------------------------------------------------------------

stopifnot(
  nrow(q11_model_summary) == 3
)

stopifnot(
  all(
    q11_model_summary$topics == 3
  )
)

stopifnot(
  nrow(q11_topic_summary) == 9
)

stopifnot(
  nrow(q11_top_terms) == 90
)

stopifnot(
  all(
    table(
      q11_top_terms$artist,
      q11_top_terms$topic
    ) == 10
  )
)

stopifnot(
  all(
    q11_topic_prevalence$mean_topic_probability >
      0
  )
)

stopifnot(
  all(
    q11_model_summary$documents_used > 0
  )
)

# ------------------------------------------------------------
# 9. Plot the ten most important terms per topic
# ------------------------------------------------------------

q11_term_plot_data <- q11_top_terms

q11_term_plot_data$term_key <- paste(
  q11_term_plot_data$topic_label,
  q11_term_plot_data$term,
  sep = " | "
)

q11_term_plot_data <- q11_term_plot_data[
  order(
    q11_term_plot_data$topic_label,
    q11_term_plot_data$beta
  ),
]

q11_term_plot_data$term_key <- factor(
  q11_term_plot_data$term_key,
  levels = q11_term_plot_data$term_key
)

q11_topic_terms_plot <- ggplot(
  q11_term_plot_data,
  aes(
    x = term_key,
    y = beta,
    fill = artist
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  coord_flip() +
  facet_wrap(
    ~ topic_label,
    scales = "free",
    ncol = 3
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
  labs(
    title = "Top Ten Terms in Each LDA Topic",
    subtitle = "Three topics identified for each artist dataset",
    x = "Term",
    y = "Term probability within topic"
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
      face = "bold",
      size = 9
    ),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(
      1,
      "lines"
    )
  )

ggsave(
  filename = "05_Plots/Q11_LDA_Top_Terms.png",
  plot = q11_topic_terms_plot,
  width = 16,
  height = 13,
  dpi = 300
)

# ------------------------------------------------------------
# 10. Plot topic prevalence
# ------------------------------------------------------------

q11_topic_prevalence_plot <- ggplot(
  q11_topic_prevalence,
  aes(
    x = factor(
      topic
    ),
    y = mean_topic_probability,
    fill = artist
  )
) +
  geom_col(
    width = 0.68,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = percent(
        mean_topic_probability,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  facet_wrap(
    ~ artist,
    nrow = 1
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    title = "Average Topic Prevalence by Artist",
    subtitle = "Average document-topic probability",
    x = "LDA topic",
    y = "Average topic probability"
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
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = "05_Plots/Q11_LDA_Topic_Prevalence.png",
  plot = q11_topic_prevalence_plot,
  width = 12,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 11. Export all Q11 evidence
# ------------------------------------------------------------

write.csv(
  q11_model_summary,
  "03_Clean_Data/Q11_LDA_Model_Summary.csv",
  row.names = FALSE
)

write.csv(
  q11_top_terms,
  "03_Clean_Data/Q11_LDA_Top_Terms.csv",
  row.names = FALSE
)

write.csv(
  q11_topic_prevalence,
  "03_Clean_Data/Q11_LDA_Topic_Prevalence.csv",
  row.names = FALSE
)

write.csv(
  q11_topic_summary,
  "03_Clean_Data/Q11_LDA_Topic_Summary.csv",
  row.names = FALSE
)

write.csv(
  q11_document_topics,
  "03_Clean_Data/Q11_LDA_Document_Topics.csv",
  row.names = FALSE
)

write.csv(
  q11_dominant_topics,
  "03_Clean_Data/Q11_LDA_Dominant_Topics.csv",
  row.names = FALSE
)

write.csv(
  q11_term_overlap,
  "03_Clean_Data/Q11_LDA_Term_Overlap.csv",
  row.names = FALSE
)

save(
  q11_taylor,
  q11_ariana,
  q11_billie,
  q11_model_summary,
  q11_top_terms,
  q11_topic_prevalence,
  q11_topic_summary,
  q11_document_topics,
  q11_dominant_topics,
  q11_term_overlap,
  q11_topic_terms_plot,
  q11_topic_prevalence_plot,
  file = "04_RData/Q11_LDA_Topic_Modelling.RData"
)

# ------------------------------------------------------------
# 12. Display final results
# ------------------------------------------------------------

print(q11_model_summary)

print(
  q11_topic_summary[, c(
    "artist",
    "topic",
    "mean_topic_probability",
    "dominant_documents",
    "top_terms"
  )],
  row.names = FALSE
)

print(q11_term_overlap)

print(q11_topic_terms_plot)
