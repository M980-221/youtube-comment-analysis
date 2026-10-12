# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 7: SOCIAL NETWORK CENTRALITY ANALYSIS
# Degree, betweenness and closeness centrality
# ============================================================

# ------------------------------------------------------------
# 1. Install and load required packages
# ------------------------------------------------------------

required_packages <- c(
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

library(igraph)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 2. Load the three Q3 actor networks
# ------------------------------------------------------------

if (
  !exists("q3_taylor") ||
  !exists("q3_ariana") ||
  !exists("q3_billie")
) {
  load(
    "04_RData/Q3_Actor_Networks.RData"
  )
}

stopifnot(exists("q3_taylor"))
stopifnot(exists("q3_ariana"))
stopifnot(exists("q3_billie"))

stopifnot(
  inherits(
    q3_taylor$network,
    "igraph"
  )
)

stopifnot(
  inherits(
    q3_ariana$network,
    "igraph"
  )
)

stopifnot(
  inherits(
    q3_billie$network,
    "igraph"
  )
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
# 3. Centrality-analysis function
# ------------------------------------------------------------

calculate_actor_centrality <- function(
    network_result,
    artist_name
) {
  
  full_network <- network_result$network
  
  # Confirm that edge weights are available.
  
  if (is.null(E(full_network)$weight)) {
    E(full_network)$weight <- 1
  }
  
  # Identify connected components.
  
  network_components <- components(
    full_network,
    mode = "weak"
  )
  
  largest_component_id <- which.max(
    network_components$csize
  )
  
  largest_component_nodes <- V(full_network)$name[
    network_components$membership ==
      largest_component_id
  ]
  
  giant_network <- induced_subgraph(
    full_network,
    vids = largest_component_nodes
  )
  
  # Stronger connections should represent shorter distances.
  # Therefore, distance is the inverse of comment frequency.
  
  E(giant_network)$distance <-
    1 / E(giant_network)$weight
  
  actor_nodes <- V(giant_network)$name[
    V(giant_network)$node_type == "actor"
  ]
  
  # Unweighted degree represents the number of different
  # selected videos on which an actor commented.
  
  degree_raw <- degree(
    giant_network,
    v = actor_nodes,
    mode = "all",
    loops = FALSE
  )
  
  degree_normalised <- degree_raw /
    (
      vcount(giant_network) - 1
    )
  
  # Weighted degree represents total comment activity.
  
  weighted_degree <- strength(
    giant_network,
    vids = actor_nodes,
    mode = "all",
    loops = FALSE,
    weights = E(giant_network)$weight
  )
  
  # Normalised betweenness measures bridging influence.
  
  betweenness_normalised <- betweenness(
    giant_network,
    v = actor_nodes,
    directed = FALSE,
    weights = E(giant_network)$distance,
    normalized = TRUE
  )
  
  # Normalised closeness measures average network proximity.
  # It is calculated on the largest connected component.
  
  closeness_normalised <- closeness(
    giant_network,
    vids = actor_nodes,
    mode = "all",
    weights = E(giant_network)$distance,
    normalized = TRUE
  )
  
  actor_centrality <- data.frame(
    artist = artist_name,
    author_channel_id = sub(
      "^actor::",
      "",
      actor_nodes
    ),
    degree = as.numeric(
      degree_raw
    ),
    degree_normalised = as.numeric(
      degree_normalised
    ),
    weighted_degree = as.numeric(
      weighted_degree
    ),
    betweenness_normalised = as.numeric(
      betweenness_normalised
    ),
    closeness_normalised = as.numeric(
      closeness_normalised
    )
  )
  
  actor_centrality <- actor_centrality[
    !is.na(actor_centrality$degree_normalised) &
      !is.na(actor_centrality$betweenness_normalised) &
      !is.na(actor_centrality$closeness_normalised),
  ]
  
  row.names(actor_centrality) <- NULL
  
  network_summary <- data.frame(
    artist = artist_name,
    full_network_vertices = vcount(
      full_network
    ),
    full_network_edges = ecount(
      full_network
    ),
    connected_components = network_components$no,
    giant_component_vertices = vcount(
      giant_network
    ),
    actors_in_giant_component = nrow(
      actor_centrality
    ),
    giant_component_percentage = round(
      vcount(giant_network) /
        vcount(full_network) *
        100,
      2
    ),
    maximum_degree = max(
      actor_centrality$degree
    ),
    maximum_weighted_degree = max(
      actor_centrality$weighted_degree
    ),
    maximum_betweenness = max(
      actor_centrality$betweenness_normalised
    ),
    maximum_closeness = max(
      actor_centrality$closeness_normalised
    )
  )
  
  list(
    full_network = full_network,
    giant_network = giant_network,
    centrality = actor_centrality,
    summary = network_summary
  )
}

# ------------------------------------------------------------
# 4. Calculate centrality for all three artist networks
# ------------------------------------------------------------

q7_taylor <- calculate_actor_centrality(
  q3_taylor,
  "Taylor Swift"
)

q7_ariana <- calculate_actor_centrality(
  q3_ariana,
  "Ariana Grande"
)

q7_billie <- calculate_actor_centrality(
  q3_billie,
  "Billie Eilish"
)

q7_all_centrality <- rbind(
  q7_taylor$centrality,
  q7_ariana$centrality,
  q7_billie$centrality
)

row.names(q7_all_centrality) <- NULL

q7_network_summary <- rbind(
  q7_taylor$summary,
  q7_ariana$summary,
  q7_billie$summary
)

row.names(q7_network_summary) <- NULL

# ------------------------------------------------------------
# 5. Create top-five tables for each centrality measure
# ------------------------------------------------------------

get_top_centrality <- function(
    centrality_data,
    artist_name,
    score_column,
    measure_name
) {
  
  artist_results <- centrality_data[
    centrality_data$artist == artist_name,
  ]
  
  artist_results <- artist_results[
    order(
      -artist_results[[score_column]],
      -artist_results$weighted_degree,
      artist_results$author_channel_id
    ),
  ]
  
  artist_results <- head(
    artist_results,
    5
  )
  
  data.frame(
    artist = artist_name,
    measure = measure_name,
    rank = seq_len(
      nrow(artist_results)
    ),
    author_channel_id =
      artist_results$author_channel_id,
    score =
      artist_results[[score_column]],
    degree =
      artist_results$degree,
    weighted_degree =
      artist_results$weighted_degree,
    betweenness_normalised =
      artist_results$betweenness_normalised,
    closeness_normalised =
      artist_results$closeness_normalised
  )
}

artist_names <- c(
  "Taylor Swift",
  "Ariana Grande",
  "Billie Eilish"
)

q7_top5_list <- list()
q7_list_position <- 1

for (artist_name in artist_names) {
  
  q7_top5_list[[q7_list_position]] <-
    get_top_centrality(
      q7_all_centrality,
      artist_name,
      "degree_normalised",
      "Degree"
    )
  
  q7_list_position <- q7_list_position + 1
  
  q7_top5_list[[q7_list_position]] <-
    get_top_centrality(
      q7_all_centrality,
      artist_name,
      "betweenness_normalised",
      "Betweenness"
    )
  
  q7_list_position <- q7_list_position + 1
  
  q7_top5_list[[q7_list_position]] <-
    get_top_centrality(
      q7_all_centrality,
      artist_name,
      "closeness_normalised",
      "Closeness"
    )
  
  q7_list_position <- q7_list_position + 1
}

q7_top5_centrality <- do.call(
  rbind,
  q7_top5_list
)

row.names(q7_top5_centrality) <- NULL

q7_top5_centrality <- q7_top5_centrality[
  order(
    q7_top5_centrality$artist,
    q7_top5_centrality$measure,
    q7_top5_centrality$rank
  ),
]

row.names(q7_top5_centrality) <- NULL

# ------------------------------------------------------------
# 6. Produce cross-artist centrality comparison
# ------------------------------------------------------------

calculate_centrality_comparison <- function(
    artist_name
) {
  
  artist_data <- q7_all_centrality[
    q7_all_centrality$artist == artist_name,
  ]
  
  data.frame(
    artist = artist_name,
    mean_degree = mean(
      artist_data$degree
    ),
    maximum_degree = max(
      artist_data$degree
    ),
    mean_weighted_degree = mean(
      artist_data$weighted_degree
    ),
    maximum_weighted_degree = max(
      artist_data$weighted_degree
    ),
    mean_betweenness = mean(
      artist_data$betweenness_normalised
    ),
    maximum_betweenness = max(
      artist_data$betweenness_normalised
    ),
    mean_closeness = mean(
      artist_data$closeness_normalised
    ),
    maximum_closeness = max(
      artist_data$closeness_normalised
    )
  )
}

q7_centrality_comparison <- do.call(
  rbind,
  lapply(
    artist_names,
    calculate_centrality_comparison
  )
)

row.names(q7_centrality_comparison) <- NULL

# ------------------------------------------------------------
# 7. Validate all centrality results
# ------------------------------------------------------------

stopifnot(
  nrow(q7_top5_centrality) == 45
)

stopifnot(
  all(
    table(
      q7_top5_centrality$artist,
      q7_top5_centrality$measure
    ) == 5
  )
)

stopifnot(
  all(
    q7_top5_centrality$score >= 0
  )
)

stopifnot(
  all(
    q7_all_centrality$degree >= 1
  )
)

stopifnot(
  all(
    q7_network_summary$giant_component_vertices > 0
  )
)

# ------------------------------------------------------------
# 8. Plot the top-five actors for every centrality measure
# ------------------------------------------------------------

q7_plot_data <- q7_top5_centrality

q7_plot_data$actor_label <- paste0(
  substr(
    q7_plot_data$author_channel_id,
    1,
    8
  ),
  "..."
)

q7_plot_data$plot_label <- paste(
  q7_plot_data$artist,
  q7_plot_data$measure,
  q7_plot_data$actor_label,
  sep = " - "
)

q7_plot_data$plot_label <- factor(
  q7_plot_data$plot_label,
  levels = rev(
    q7_plot_data$plot_label
  )
)

q7_top5_plot <- ggplot(
  q7_plot_data,
  aes(
    x = plot_label,
    y = score,
    fill = artist
  )
) +
  geom_col(
    width = 0.72,
    show.legend = FALSE
  ) +
  coord_flip() +
  facet_grid(
    artist ~ measure,
    scales = "free",
    space = "free"
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
    labels = label_number(
      accuracy = 0.0001
    )
  ) +
  labs(
    title = "Top Five Actors by Centrality Measure",
    subtitle = paste(
      "Normalised degree, betweenness and closeness",
      "within each artist network"
    ),
    x = "YouTube author channel ID",
    y = "Normalised centrality score"
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
  filename = "05_Plots/Q7_Top5_Centrality.png",
  plot = q7_top5_plot,
  width = 16,
  height = 13,
  dpi = 300
)

# ------------------------------------------------------------
# 9. Create cross-artist maximum-centrality comparison plot
# ------------------------------------------------------------

q7_maximum_comparison_plot_data <- rbind(
  data.frame(
    artist = q7_centrality_comparison$artist,
    measure = "Degree",
    maximum_score =
      q7_centrality_comparison$maximum_degree
  ),
  data.frame(
    artist = q7_centrality_comparison$artist,
    measure = "Betweenness",
    maximum_score =
      q7_centrality_comparison$maximum_betweenness
  ),
  data.frame(
    artist = q7_centrality_comparison$artist,
    measure = "Closeness",
    maximum_score =
      q7_centrality_comparison$maximum_closeness
  )
)

q7_maximum_comparison_plot <- ggplot(
  q7_maximum_comparison_plot_data,
  aes(
    x = artist,
    y = maximum_score,
    fill = artist
  )
) +
  geom_col(
    width = 0.65,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ measure,
    scales = "free_y"
  ) +
  geom_text(
    aes(
      label = signif(
        maximum_score,
        4
      )
    ),
    vjust = -0.5,
    size = 4,
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
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    title = "Maximum Centrality Comparison Across Artists",
    subtitle = paste(
      "Higher values identify the most structurally",
      "central actor in each dataset"
    ),
    x = "Artist",
    y = "Maximum centrality score"
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
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = "05_Plots/Q7_Centrality_Artist_Comparison.png",
  plot = q7_maximum_comparison_plot,
  width = 12,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 10. Export all Q7 evidence
# ------------------------------------------------------------

write.csv(
  q7_all_centrality,
  "03_Clean_Data/Q7_All_Actor_Centrality_Scores.csv",
  row.names = FALSE
)

write.csv(
  q7_top5_centrality,
  "03_Clean_Data/Q7_Top5_Centrality.csv",
  row.names = FALSE
)

write.csv(
  q7_network_summary,
  "03_Clean_Data/Q7_Centrality_Network_Summary.csv",
  row.names = FALSE
)

write.csv(
  q7_centrality_comparison,
  "03_Clean_Data/Q7_Centrality_Artist_Comparison.csv",
  row.names = FALSE
)

save(
  q7_taylor,
  q7_ariana,
  q7_billie,
  q7_all_centrality,
  q7_top5_centrality,
  q7_network_summary,
  q7_centrality_comparison,
  q7_top5_plot,
  q7_maximum_comparison_plot,
  file = "04_RData/Q7_Centrality_Analysis.RData"
)

# ------------------------------------------------------------
# 11. Display final Q7 results
# ------------------------------------------------------------

print(q7_network_summary)

print(q7_centrality_comparison)

print(
  q7_top5_centrality[, c(
    "artist",
    "measure",
    "rank",
    "author_channel_id",
    "score",
    "degree",
    "weighted_degree"
  )],
  row.names = FALSE
)

print(q7_top5_plot)
# ============================================================
# QUESTION 7: CORRECTED TOP-FIVE CENTRALITY VISUALISATION
# ============================================================

q7_plot_data <- q7_top5_centrality

q7_plot_data$actor_label <- paste0(
  "#",
  q7_plot_data$rank,
  " ",
  substr(
    q7_plot_data$author_channel_id,
    1,
    8
  ),
  "..."
)

q7_plot_data$panel <- paste(
  q7_plot_data$artist,
  q7_plot_data$measure,
  sep = " - "
)

q7_plot_data$plot_label <- paste(
  q7_plot_data$panel,
  q7_plot_data$actor_label,
  sep = " | "
)

q7_plot_data$plot_label <- factor(
  q7_plot_data$plot_label,
  levels = rev(
    q7_plot_data$plot_label
  )
)

q7_top5_plot <- ggplot(
  q7_plot_data,
  aes(
    x = plot_label,
    y = score,
    fill = artist
  )
) +
  geom_col(
    width = 0.68,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = signif(
        score,
        3
      )
    ),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  facet_wrap(
    ~ panel,
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
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.22
      )
    )
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
  ) +
  labs(
    title = "Top Five Actors by Centrality Measure",
    subtitle = paste(
      "Degree, betweenness and closeness",
      "across three YouTube actor networks"
    ),
    x = "Rank and author channel ID",
    y = "Normalised centrality score"
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
  filename = "05_Plots/Q7_Top5_Centrality.png",
  plot = q7_top5_plot,
  width = 16,
  height = 12,
  dpi = 300
)

save(
  q7_taylor,
  q7_ariana,
  q7_billie,
  q7_all_centrality,
  q7_top5_centrality,
  q7_network_summary,
  q7_centrality_comparison,
  q7_top5_plot,
  q7_maximum_comparison_plot,
  file = "04_RData/Q7_Centrality_Analysis.RData"
)

print(q7_network_summary)

print(q7_top5_plot)