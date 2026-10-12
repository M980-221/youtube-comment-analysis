# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 6: SEMANTIC BIGRAM NETWORKS AND PAGERANK
# ============================================================

# ------------------------------------------------------------
# 1. Install and load required packages
# ------------------------------------------------------------

required_packages <- c(
  "tidytext",
  "dplyr",
  "tidyr",
  "igraph",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 2. Load the preprocessed Q5 data
# ------------------------------------------------------------

if (
  !exists("q5_cleaned_comments") ||
  !exists("q5_top10_terms")
) {
  load(
    "04_RData/Q5_Term_Document_Matrices.RData"
  )
}

stopifnot(exists("q5_cleaned_comments"))
stopifnot(exists("q5_top10_terms"))
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
# 3. Extract meaningful bigrams
# ------------------------------------------------------------

q6_bigram_source <- q5_cleaned_comments[
  !is.na(q5_cleaned_comments$cleaned_text) &
    trimws(q5_cleaned_comments$cleaned_text) != "",
  required_columns
]

q6_bigram_tokens <- q6_bigram_source %>%
  unnest_tokens(
    output = bigram,
    input = cleaned_text,
    token = "ngrams",
    n = 2
  ) %>%
  separate(
    bigram,
    into = c(
      "word1",
      "word2"
    ),
    sep = " ",
    remove = TRUE
  ) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    word1 != "",
    word2 != "",
    word1 != word2,
    
    # Remove single-letter and two-letter fragments that do not
    # provide meaningful semantic information.
    nchar(word1) >= 3,
    nchar(word2) >= 3,
    
    # Keep alphabetic terms only.
    grepl(
      "^[a-z]+$",
      word1
    ),
    grepl(
      "^[a-z]+$",
      word2
    )
  )

q6_bigram_tokens$occurrence <- 1

q6_bigram_edges_all <- aggregate(
  occurrence ~ artist + word1 + word2,
  data = q6_bigram_tokens,
  FUN = sum
)

names(q6_bigram_edges_all)[4] <- "weight"

# Remove one-off bigrams because they can be accidental
# combinations and create a sparse, noisy network.

q6_min_bigram_frequency <- 2

q6_bigram_edges <- q6_bigram_edges_all[
  q6_bigram_edges_all$weight >=
    q6_min_bigram_frequency,
]

row.names(q6_bigram_edges) <- NULL

# ------------------------------------------------------------
# 4. Function to create a directed semantic network
# ------------------------------------------------------------

create_semantic_network <- function(artist_name) {
  
  artist_edges <- q6_bigram_edges[
    q6_bigram_edges$artist == artist_name,
  ]
  
  semantic_edges <- data.frame(
    from = artist_edges$word1,
    to = artist_edges$word2,
    weight = artist_edges$weight
  )
  
  semantic_network <- graph_from_data_frame(
    semantic_edges,
    directed = TRUE
  )
  
  semantic_network <- simplify(
    semantic_network,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(
      weight = "sum"
    )
  )
  
  pagerank_scores <- page_rank(
    semantic_network,
    directed = TRUE,
    weights = E(semantic_network)$weight
  )$vector
  
  term_ranking <- data.frame(
    artist = artist_name,
    term = V(semantic_network)$name,
    page_rank = as.numeric(
      pagerank_scores[
        V(semantic_network)$name
      ]
    ),
    weighted_degree = as.numeric(
      strength(
        semantic_network,
        mode = "all",
        weights = E(semantic_network)$weight
      )
    ),
    in_degree = as.numeric(
      degree(
        semantic_network,
        mode = "in"
      )
    ),
    out_degree = as.numeric(
      degree(
        semantic_network,
        mode = "out"
      )
    )
  )
  
  term_ranking <- term_ranking[
    order(
      -term_ranking$page_rank,
      -term_ranking$weighted_degree,
      term_ranking$term
    ),
  ]
  
  row.names(term_ranking) <- NULL
  term_ranking$rank <- seq_len(
    nrow(term_ranking)
  )
  
  list(
    network = semantic_network,
    edges = artist_edges,
    ranking = term_ranking
  )
}

# ------------------------------------------------------------
# 5. Create networks for all three artists
# ------------------------------------------------------------

q6_taylor <- create_semantic_network(
  "Taylor Swift"
)

q6_ariana <- create_semantic_network(
  "Ariana Grande"
)

q6_billie <- create_semantic_network(
  "Billie Eilish"
)

q6_top10_pagerank <- rbind(
  head(q6_taylor$ranking, 10),
  head(q6_ariana$ranking, 10),
  head(q6_billie$ranking, 10)
)

q6_top10_pagerank <- q6_top10_pagerank[
  order(
    q6_top10_pagerank$artist,
    q6_top10_pagerank$rank
  ),
]

row.names(q6_top10_pagerank) <- NULL

# ------------------------------------------------------------
# 6. Summarise the three semantic networks
# ------------------------------------------------------------

q6_network_summary <- data.frame(
  artist = c(
    "Taylor Swift",
    "Ariana Grande",
    "Billie Eilish"
  ),
  terms_nodes = c(
    vcount(q6_taylor$network),
    vcount(q6_ariana$network),
    vcount(q6_billie$network)
  ),
  bigram_edges = c(
    ecount(q6_taylor$network),
    ecount(q6_ariana$network),
    ecount(q6_billie$network)
  ),
  retained_bigram_occurrences = c(
    sum(E(q6_taylor$network)$weight),
    sum(E(q6_ariana$network)$weight),
    sum(E(q6_billie$network)$weight)
  ),
  density = round(
    c(
      edge_density(q6_taylor$network),
      edge_density(q6_ariana$network),
      edge_density(q6_billie$network)
    ),
    6
  )
)

# ------------------------------------------------------------
# 7. Compare Q5 frequency rankings with Q6 PageRank rankings
# ------------------------------------------------------------

q6_frequency_ranks <- q5_top10_terms[
  ,
  c(
    "artist",
    "term",
    "rank",
    "frequency"
  )
]

names(q6_frequency_ranks)[3] <- "frequency_rank"

q6_pagerank_ranks <- q6_top10_pagerank[
  ,
  c(
    "artist",
    "term",
    "rank",
    "page_rank"
  )
]

names(q6_pagerank_ranks)[3] <- "pagerank_rank"

q6_rank_comparison <- merge(
  q6_frequency_ranks,
  q6_pagerank_ranks,
  by = c(
    "artist",
    "term"
  ),
  all = TRUE,
  sort = FALSE
)

q6_rank_comparison$in_frequency_top10 <-
  !is.na(q6_rank_comparison$frequency_rank)

q6_rank_comparison$in_pagerank_top10 <-
  !is.na(q6_rank_comparison$pagerank_rank)

q6_rank_comparison$in_both_top10 <-
  q6_rank_comparison$in_frequency_top10 &
  q6_rank_comparison$in_pagerank_top10

calculate_overlap <- function(artist_name) {
  
  frequency_terms <- q6_frequency_ranks$term[
    q6_frequency_ranks$artist == artist_name
  ]
  
  pagerank_terms <- q6_pagerank_ranks$term[
    q6_pagerank_ranks$artist == artist_name
  ]
  
  shared_terms <- intersect(
    frequency_terms,
    pagerank_terms
  )
  
  data.frame(
    artist = artist_name,
    shared_top10_terms = length(shared_terms),
    frequency_only_terms = 10 - length(shared_terms),
    pagerank_only_terms = 10 - length(shared_terms),
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

q6_overlap_summary <- do.call(
  rbind,
  lapply(
    c(
      "Taylor Swift",
      "Ariana Grande",
      "Billie Eilish"
    ),
    calculate_overlap
  )
)

row.names(q6_overlap_summary) <- NULL

# ------------------------------------------------------------
# 8. Validate the PageRank results
# ------------------------------------------------------------

stopifnot(
  nrow(q6_top10_pagerank) == 30
)

stopifnot(
  all(table(q6_top10_pagerank$artist) == 10)
)

stopifnot(
  all(q6_top10_pagerank$page_rank > 0)
)

stopifnot(
  all(
    q6_network_summary$terms_nodes > 10
  )
)

stopifnot(
  all(
    q6_network_summary$bigram_edges > 10
  )
)

stopifnot(
  all(
    q6_overlap_summary$shared_top10_terms >= 0 &
      q6_overlap_summary$shared_top10_terms <= 10
  )
)

# ------------------------------------------------------------
# 9. Create top-ten PageRank plot
# ------------------------------------------------------------

q6_pagerank_plot_data <- q6_top10_pagerank

q6_pagerank_plot_data$term_label <- paste(
  q6_pagerank_plot_data$artist,
  q6_pagerank_plot_data$term,
  sep = " - "
)

q6_pagerank_plot_data$term_label <- factor(
  q6_pagerank_plot_data$term_label,
  levels = rev(
    q6_pagerank_plot_data$term_label
  )
)

q6_pagerank_plot <- ggplot(
  q6_pagerank_plot_data,
  aes(
    x = term_label,
    y = page_rank,
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
  labs(
    title = "Top Ten Semantic Terms by PageRank",
    subtitle = paste(
      "Directed weighted bigram networks;",
      "one-off bigrams excluded"
    ),
    x = "Term",
    y = "PageRank score"
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
  filename = "05_Plots/Q6_Top10_Terms_PageRank.png",
  plot = q6_pagerank_plot,
  width = 10,
  height = 11,
  dpi = 300
)

# ------------------------------------------------------------
# 10. Create Q5 frequency versus Q6 PageRank rank plot
# ------------------------------------------------------------

q6_frequency_plot_data <- data.frame(
  artist = q5_top10_terms$artist,
  term = q5_top10_terms$term,
  rank = q5_top10_terms$rank,
  method = "Q5: Term frequency"
)

q6_pagerank_comparison_data <- data.frame(
  artist = q6_top10_pagerank$artist,
  term = q6_top10_pagerank$term,
  rank = q6_top10_pagerank$rank,
  method = "Q6: Semantic PageRank"
)

q6_method_comparison_data <- rbind(
  q6_frequency_plot_data,
  q6_pagerank_comparison_data
)

q6_method_comparison_data$importance_score <-
  11 - q6_method_comparison_data$rank

q6_method_comparison_data$term_label <- paste(
  q6_method_comparison_data$artist,
  q6_method_comparison_data$method,
  q6_method_comparison_data$term,
  sep = " - "
)

q6_method_comparison_data$term_label <- factor(
  q6_method_comparison_data$term_label,
  levels = rev(
    q6_method_comparison_data$term_label
  )
)

q6_method_comparison_plot <- ggplot(
  q6_method_comparison_data,
  aes(
    x = term_label,
    y = importance_score,
    fill = method
  )
) +
  geom_col(
    width = 0.72
  ) +
  coord_flip() +
  facet_grid(
    artist ~ method,
    scales = "free_y",
    space = "free_y"
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
      "Q5: Term frequency" = "#F28E2B",
      "Q6: Semantic PageRank" = "#4E79A7"
    )
  ) +
  scale_y_continuous(
    breaks = 1:10,
    labels = 10:1
  ) +
  labs(
    title = "Comparison of Frequency and Semantic Importance",
    subtitle = "A value of 1 indicates the highest-ranked term",
    x = "Term",
    y = "Rank",
    fill = "Ranking method"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "05_Plots/Q6_Frequency_vs_PageRank.png",
  plot = q6_method_comparison_plot,
  width = 14,
  height = 12,
  dpi = 300
)

# ------------------------------------------------------------
# 11. Function to save readable semantic-network plots
# ------------------------------------------------------------

save_semantic_network_plot <- function(
    network_result,
    artist_name,
    file_name,
    main_colour
) {
  
  top_terms <- head(
    network_result$ranking$term,
    10
  )
  
  visual_edges <- network_result$edges[
    network_result$edges$word1 %in% top_terms |
      network_result$edges$word2 %in% top_terms,
  ]
  
  visual_edges <- visual_edges[
    order(
      -visual_edges$weight,
      visual_edges$word1,
      visual_edges$word2
    ),
  ]
  
  visual_edges <- head(
    visual_edges,
    80
  )
  
  visual_network <- graph_from_data_frame(
    data.frame(
      from = visual_edges$word1,
      to = visual_edges$word2,
      weight = visual_edges$weight
    ),
    directed = TRUE
  )
  
  visual_scores <- network_result$ranking$page_rank[
    match(
      V(visual_network)$name,
      network_result$ranking$term
    )
  ]
  
  visual_ranks <- network_result$ranking$rank[
    match(
      V(visual_network)$name,
      network_result$ranking$term
    )
  ]
  
  V(visual_network)$color <- ifelse(
    V(visual_network)$name %in% top_terms,
    "#E15759",
    main_colour
  )
  
  V(visual_network)$size <- 6 +
    18 * (
      visual_scores /
        max(
          visual_scores,
          na.rm = TRUE
        )
    )
  
  V(visual_network)$label <- ifelse(
    V(visual_network)$name %in% top_terms |
      visual_ranks <= 20,
    V(visual_network)$name,
    NA
  )
  
  png(
    filename = file_name,
    width = 2600,
    height = 2000,
    res = 250
  )
  
  set.seed(3032)
  
  plot(
    visual_network,
    layout = layout_with_fr(
      visual_network,
      weights = E(visual_network)$weight
    ),
    main = paste(
      artist_name,
      "Semantic Bigram Network"
    ),
    vertex.label.cex = 0.82,
    vertex.label.color = "black",
    vertex.frame.color = "white",
    edge.color = adjustcolor(
      "grey45",
      alpha.f = 0.55
    ),
    edge.width = pmax(
      1,
      log1p(E(visual_network)$weight)
    ),
    edge.arrow.size = 0.35,
    edge.curved = 0.08,
    margin = 0.08
  )
  
  legend(
    "topleft",
    legend = c(
      "Top 10 PageRank term",
      "Connected term"
    ),
    col = c(
      "#E15759",
      main_colour
    ),
    pch = 19,
    pt.cex = 1.6,
    bty = "n"
  )
  
  dev.off()
}

# ------------------------------------------------------------
# 12. Save three semantic-network visualisations
# ------------------------------------------------------------

save_semantic_network_plot(
  q6_taylor,
  "Taylor Swift",
  "05_Plots/Q6_Taylor_Semantic_Network.png",
  "#4E79A7"
)

save_semantic_network_plot(
  q6_ariana,
  "Ariana Grande",
  "05_Plots/Q6_Ariana_Semantic_Network.png",
  "#B07AA1"
)

save_semantic_network_plot(
  q6_billie,
  "Billie Eilish",
  "05_Plots/Q6_Billie_Semantic_Network.png",
  "#59A14F"
)

# ------------------------------------------------------------
# 13. Export Q6 evidence
# ------------------------------------------------------------

write.csv(
  q6_bigram_edges,
  "03_Clean_Data/Q6_Semantic_Bigram_Edges.csv",
  row.names = FALSE
)

write.csv(
  q6_top10_pagerank,
  "03_Clean_Data/Q6_Top10_Terms_PageRank.csv",
  row.names = FALSE
)

write.csv(
  q6_network_summary,
  "03_Clean_Data/Q6_Semantic_Network_Summary.csv",
  row.names = FALSE
)

write.csv(
  q6_rank_comparison,
  "03_Clean_Data/Q6_Frequency_PageRank_Comparison.csv",
  row.names = FALSE
)

write.csv(
  q6_overlap_summary,
  "03_Clean_Data/Q6_Top10_Overlap_Summary.csv",
  row.names = FALSE
)

save(
  q6_taylor,
  q6_ariana,
  q6_billie,
  q6_bigram_edges,
  q6_top10_pagerank,
  q6_network_summary,
  q6_rank_comparison,
  q6_overlap_summary,
  q6_pagerank_plot,
  q6_method_comparison_plot,
  file = "04_RData/Q6_Semantic_Bigram_Networks.RData"
)

# ------------------------------------------------------------
# 14. Display final results
# ------------------------------------------------------------

print(q6_network_summary)

print(
  q6_top10_pagerank[, c(
    "artist",
    "rank",
    "term",
    "page_rank",
    "weighted_degree",
    "in_degree",
    "out_degree"
  )],
  row.names = FALSE
)

print(q6_overlap_summary)

print(q6_pagerank_plot)
# ============================================================
# QUESTION 6: TOP BIGRAM CONNECTIONS FOR INTERPRETATION
# ============================================================

get_top_bigram_connections <- function(
    artist_name,
    number_of_bigrams = 15
) {
  
  artist_bigrams <- q6_bigram_edges[
    q6_bigram_edges$artist == artist_name,
  ]
  
  artist_bigrams <- artist_bigrams[
    order(
      -artist_bigrams$weight,
      artist_bigrams$word1,
      artist_bigrams$word2
    ),
  ]
  
  head(
    artist_bigrams,
    number_of_bigrams
  )
}

q6_top_bigram_connections <- rbind(
  get_top_bigram_connections(
    "Taylor Swift"
  ),
  get_top_bigram_connections(
    "Ariana Grande"
  ),
  get_top_bigram_connections(
    "Billie Eilish"
  )
)

row.names(q6_top_bigram_connections) <- NULL

write.csv(
  q6_top_bigram_connections,
  "03_Clean_Data/Q6_Top_Bigram_Connections.csv",
  row.names = FALSE
)

save(
  q6_top_bigram_connections,
  file = "04_RData/Q6_Top_Bigram_Connections.RData"
)

print(
  q6_top_bigram_connections,
  row.names = FALSE
)