# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 8: COMMUNITY ANALYSIS
# GIRVAN-NEWMAN AND LOUVAIN
# ============================================================

# ------------------------------------------------------------
# 1. Install and load packages
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
# 2. Load actor networks and video information
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

if (!exists("selected_videos")) {
  load(
    "04_RData/Q2_Selected_Videos.RData"
  )
}

stopifnot(exists("q3_taylor"))
stopifnot(exists("q3_ariana"))
stopifnot(exists("q3_billie"))
stopifnot(exists("selected_videos"))

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
# 3. Create a video-title lookup table
# ------------------------------------------------------------

video_id_column <- if (
  "video_id" %in% names(selected_videos)
) {
  "video_id"
} else {
  "selected_video_id"
}

video_title_column <- if (
  "video_title" %in% names(selected_videos)
) {
  "video_title"
} else {
  "title"
}

q8_video_lookup <- data.frame(
  artist = selected_videos$artist,
  video_id = selected_videos[[video_id_column]],
  video_title = selected_videos[[video_title_column]],
  stringsAsFactors = FALSE
)

q8_video_lookup <- q8_video_lookup[
  !duplicated(
    q8_video_lookup[, c(
      "artist",
      "video_id"
    )]
  ),
]

row.names(q8_video_lookup) <- NULL

# ------------------------------------------------------------
# 4. Function to calculate community composition
# ------------------------------------------------------------

create_community_composition <- function(
    membership_table,
    membership_column,
    artist_name,
    algorithm_name
) {
  
  community_values <-
    membership_table[[membership_column]]
  
  composition_table <- as.data.frame.matrix(
    table(
      community = community_values,
      node_type = membership_table$node_type
    )
  )
  
  if (!"actor" %in% names(composition_table)) {
    composition_table$actor <- 0
  }
  
  if (!"video" %in% names(composition_table)) {
    composition_table$video <- 0
  }
  
  composition_table$community <-
    as.numeric(
      row.names(composition_table)
    )
  
  composition_table$artist <- artist_name
  composition_table$algorithm <- algorithm_name
  
  composition_table$total_nodes <-
    composition_table$actor +
    composition_table$video
  
  composition_table <- composition_table[
    ,
    c(
      "artist",
      "algorithm",
      "community",
      "actor",
      "video",
      "total_nodes"
    )
  ]
  
  row.names(composition_table) <- NULL
  
  composition_table
}

# ------------------------------------------------------------
# 5. Function to analyse one artist network
# ------------------------------------------------------------

analyse_artist_communities <- function(
    network_result,
    artist_name
) {
  
  full_network <- network_result$network
  
  if (is.null(E(full_network)$weight)) {
    E(full_network)$weight <- 1
  }
  
  actor_nodes <- V(full_network)$name[
    V(full_network)$node_type == "actor"
  ]
  
  actor_video_degree <- degree(
    full_network,
    v = actor_nodes,
    mode = "all"
  )
  
  # Keep actors connected to at least two selected videos.
  # Single-video actors are pendant nodes and would produce
  # communities based almost entirely on individual videos.
  # Cross-video actors reveal genuine audience connections.
  
  cross_video_actors <- names(
    actor_video_degree[
      actor_video_degree >= 2
    ]
  )
  
  connected_videos <- unique(
    unlist(
      lapply(
        adjacent_vertices(
          full_network,
          cross_video_actors,
          mode = "all"
        ),
        names
      )
    )
  )
  
  connected_videos <- connected_videos[
    grepl(
      "^video::",
      connected_videos
    )
  ]
  
  selected_nodes <- unique(
    c(
      cross_video_actors,
      connected_videos
    )
  )
  
  active_network <- induced_subgraph(
    full_network,
    vids = selected_nodes
  )
  
  active_network <- simplify(
    active_network,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(
      weight = "sum"
    )
  )
  
  E(active_network)$distance <-
    1 / E(active_network)$weight
  
  # ----------------------------------------------------------
  # Girvan-Newman community detection
  # ----------------------------------------------------------
  
  girvan_newman_result <-
    cluster_edge_betweenness(
      active_network,
      directed = FALSE,
      weights = E(active_network)$distance,
      modularity = TRUE
    )
  
  girvan_membership <- membership(
    girvan_newman_result
  )
  
  girvan_modularity <- modularity(
    active_network,
    girvan_membership,
    weights = E(active_network)$weight
  )
  
  # ----------------------------------------------------------
  # Louvain community detection
  # ----------------------------------------------------------
  
  set.seed(3032)
  
  louvain_result <- cluster_louvain(
    active_network,
    weights = E(active_network)$weight,
    resolution = 1
  )
  
  louvain_membership <- membership(
    louvain_result
  )
  
  louvain_modularity <- modularity(
    active_network,
    louvain_membership,
    weights = E(active_network)$weight
  )
  
  # ----------------------------------------------------------
  # Compare the two community solutions
  # ----------------------------------------------------------
  
  adjusted_rand_agreement <- compare(
    girvan_membership,
    louvain_membership,
    method = "adjusted.rand"
  )
  
  membership_table <- data.frame(
    artist = artist_name,
    node = V(active_network)$name,
    node_type = V(active_network)$node_type,
    node_id = sub(
      "^(actor|video)::",
      "",
      V(active_network)$name
    ),
    girvan_newman_community =
      as.numeric(
        girvan_membership[
          V(active_network)$name
        ]
      ),
    louvain_community =
      as.numeric(
        louvain_membership[
          V(active_network)$name
        ]
      ),
    stringsAsFactors = FALSE
  )
  
  membership_table$video_title <- NA_character_
  
  video_rows <- membership_table$node_type == "video"
  
  membership_table$video_title[video_rows] <-
    q8_video_lookup$video_title[
      match(
        paste(
          artist_name,
          membership_table$node_id[video_rows]
        ),
        paste(
          q8_video_lookup$artist,
          q8_video_lookup$video_id
        )
      )
    ]
  
  girvan_composition <-
    create_community_composition(
      membership_table,
      "girvan_newman_community",
      artist_name,
      "Girvan-Newman"
    )
  
  louvain_composition <-
    create_community_composition(
      membership_table,
      "louvain_community",
      artist_name,
      "Louvain"
    )
  
  girvan_sizes <- sizes(
    girvan_newman_result
  )
  
  louvain_sizes <- sizes(
    louvain_result
  )
  
  summary_table <- rbind(
    data.frame(
      artist = artist_name,
      algorithm = "Girvan-Newman",
      active_actor_nodes =
        sum(
          V(active_network)$node_type == "actor"
        ),
      video_nodes =
        sum(
          V(active_network)$node_type == "video"
        ),
      network_edges = ecount(
        active_network
      ),
      communities = length(
        unique(
          girvan_membership
        )
      ),
      modularity = girvan_modularity,
      largest_community_nodes = max(
        girvan_sizes
      ),
      largest_community_percentage =
        round(
          max(girvan_sizes) /
            vcount(active_network) *
            100,
          2
        ),
      adjusted_rand_agreement =
        adjusted_rand_agreement
    ),
    data.frame(
      artist = artist_name,
      algorithm = "Louvain",
      active_actor_nodes =
        sum(
          V(active_network)$node_type == "actor"
        ),
      video_nodes =
        sum(
          V(active_network)$node_type == "video"
        ),
      network_edges = ecount(
        active_network
      ),
      communities = length(
        unique(
          louvain_membership
        )
      ),
      modularity = louvain_modularity,
      largest_community_nodes = max(
        louvain_sizes
      ),
      largest_community_percentage =
        round(
          max(louvain_sizes) /
            vcount(active_network) *
            100,
          2
        ),
      adjusted_rand_agreement =
        adjusted_rand_agreement
    )
  )
  
  list(
    full_network = full_network,
    active_network = active_network,
    actor_video_degree = actor_video_degree,
    girvan_newman = girvan_newman_result,
    louvain = louvain_result,
    girvan_membership = girvan_membership,
    louvain_membership = louvain_membership,
    membership_table = membership_table,
    girvan_composition = girvan_composition,
    louvain_composition = louvain_composition,
    summary = summary_table
  )
}

# ------------------------------------------------------------
# 6. Run both algorithms for all three artists
# ------------------------------------------------------------

message(
  "Running Taylor Swift community analysis..."
)

q8_taylor <- analyse_artist_communities(
  q3_taylor,
  "Taylor Swift"
)

message(
  "Running Ariana Grande community analysis..."
)

q8_ariana <- analyse_artist_communities(
  q3_ariana,
  "Ariana Grande"
)

message(
  "Running Billie Eilish community analysis..."
)

q8_billie <- analyse_artist_communities(
  q3_billie,
  "Billie Eilish"
)

# ------------------------------------------------------------
# 7. Combine summaries and memberships
# ------------------------------------------------------------

q8_community_summary <- rbind(
  q8_taylor$summary,
  q8_ariana$summary,
  q8_billie$summary
)

row.names(q8_community_summary) <- NULL

q8_membership_table <- rbind(
  q8_taylor$membership_table,
  q8_ariana$membership_table,
  q8_billie$membership_table
)

row.names(q8_membership_table) <- NULL

q8_community_composition <- rbind(
  q8_taylor$girvan_composition,
  q8_taylor$louvain_composition,
  q8_ariana$girvan_composition,
  q8_ariana$louvain_composition,
  q8_billie$girvan_composition,
  q8_billie$louvain_composition
)

row.names(q8_community_composition) <- NULL

# ------------------------------------------------------------
# 8. Create video-community comparison
# ------------------------------------------------------------

q8_video_communities <- q8_membership_table[
  q8_membership_table$node_type == "video",
  c(
    "artist",
    "node_id",
    "video_title",
    "girvan_newman_community",
    "louvain_community"
  )
]

names(q8_video_communities)[2] <- "video_id"

q8_video_communities <- q8_video_communities[
  order(
    q8_video_communities$artist,
    q8_video_communities$girvan_newman_community,
    q8_video_communities$louvain_community
  ),
]

row.names(q8_video_communities) <- NULL

# ------------------------------------------------------------
# 9. Validate the community results
# ------------------------------------------------------------

stopifnot(
  nrow(q8_community_summary) == 6
)

stopifnot(
  all(
    table(
      q8_community_summary$artist
    ) == 2
  )
)

stopifnot(
  all(
    table(
      q8_community_summary$algorithm
    ) == 3
  )
)

stopifnot(
  all(
    q8_community_summary$communities >= 2
  )
)

stopifnot(
  all(
    is.finite(
      q8_community_summary$modularity
    )
  )
)

stopifnot(
  all(
    q8_community_summary$active_actor_nodes > 0
  )
)

stopifnot(
  length(
    q8_taylor$girvan_membership
  ) ==
    vcount(q8_taylor$active_network)
)

stopifnot(
  length(
    q8_ariana$louvain_membership
  ) ==
    vcount(q8_ariana$active_network)
)

stopifnot(
  length(
    q8_billie$louvain_membership
  ) ==
    vcount(q8_billie$active_network)
)

# ------------------------------------------------------------
# 10. Community-count comparison plot
# ------------------------------------------------------------

q8_community_count_plot <- ggplot(
  q8_community_summary,
  aes(
    x = artist,
    y = communities,
    fill = algorithm
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = communities
    ),
    position = position_dodge(
      width = 0.75
    ),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Girvan-Newman" = "#E15759",
      "Louvain" = "#4E79A7"
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    title = "Number of Detected Audience Communities",
    subtitle = "Comparison of Girvan-Newman and Louvain",
    x = "Artist",
    y = "Number of communities",
    fill = "Algorithm"
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
  filename = "05_Plots/Q8_Community_Count_Comparison.png",
  plot = q8_community_count_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 11. Modularity comparison plot
# ------------------------------------------------------------

q8_modularity_plot <- ggplot(
  q8_community_summary,
  aes(
    x = artist,
    y = modularity,
    fill = algorithm
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = round(
        modularity,
        3
      )
    ),
    position = position_dodge(
      width = 0.75
    ),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Girvan-Newman" = "#E15759",
      "Louvain" = "#4E79A7"
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
    title = "Community Modularity Across Artist Networks",
    subtitle = "Higher modularity indicates clearer community separation",
    x = "Artist",
    y = "Weighted modularity",
    fill = "Algorithm"
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
  filename = "05_Plots/Q8_Modularity_Comparison.png",
  plot = q8_modularity_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 12. Function to create readable community-network plots
# ------------------------------------------------------------

save_community_network_plot <- function(
    community_result,
    artist_name,
    algorithm_name,
    membership_vector,
    file_name
) {
  
  network <- community_result$active_network
  
  membership_values <- as.numeric(
    membership_vector[
      V(network)$name
    ]
  )
  
  number_of_communities <- length(
    unique(
      membership_values
    )
  )
  
  community_colours <- hcl.colors(
    number_of_communities,
    palette = "Dynamic"
  )
  
  colour_index <- match(
    membership_values,
    sort(
      unique(
        membership_values
      )
    )
  )
  
  vertex_colours <- community_colours[
    colour_index
  ]
  
  actor_degree <- degree(
    network,
    mode = "all"
  )
  
  actor_rank <- rank(
    -actor_degree,
    ties.method = "first"
  )
  
  video_nodes <- V(network)$node_type == "video"
  
  video_sequence <- match(
    V(network)$name,
    V(network)$name[
      video_nodes
    ]
  )
  
  vertex_labels <- ifelse(
    video_nodes,
    paste0(
      "V",
      video_sequence
    ),
    ifelse(
      actor_rank <= 5,
      paste0(
        substr(
          sub(
            "^actor::",
            "",
            V(network)$name
          ),
          1,
          6
        ),
        "..."
      ),
      NA
    )
  )
  
  vertex_shapes <- ifelse(
    video_nodes,
    "square",
    "circle"
  )
  
  vertex_sizes <- ifelse(
    video_nodes,
    14,
    6
  )
  
  set.seed(3032)
  
  network_layout <- layout_with_fr(
    network,
    weights = E(network)$weight,
    niter = 1200
  )
  
  png(
    filename = file_name,
    width = 2600,
    height = 2000,
    res = 250
  )
  
  plot(
    network,
    layout = network_layout,
    main = paste(
      artist_name,
      "-",
      algorithm_name,
      "Communities"
    ),
    vertex.color = vertex_colours,
    vertex.shape = vertex_shapes,
    vertex.size = vertex_sizes,
    vertex.label = vertex_labels,
    vertex.label.cex = 0.72,
    vertex.label.color = "black",
    vertex.frame.color = "white",
    edge.color = adjustcolor(
      "grey55",
      alpha.f = 0.45
    ),
    edge.width = pmax(
      0.7,
      log1p(
        E(network)$weight
      )
    ),
    margin = 0.08
  )
  
  legend(
    "topleft",
    legend = paste(
      "Community",
      sort(
        unique(
          membership_values
        )
      )
    ),
    col = community_colours,
    pch = 19,
    pt.cex = 1.4,
    bty = "n",
    cex = 0.8
  )
  
  legend(
    "bottomleft",
    legend = c(
      "Cross-video actor",
      "Selected video"
    ),
    pch = c(
      21,
      22
    ),
    pt.bg = c(
      "grey70",
      "grey70"
    ),
    pt.cex = 1.4,
    bty = "n",
    cex = 0.8
  )
  
  dev.off()
}

# ------------------------------------------------------------
# 13. Save six community-network visualisations
# ------------------------------------------------------------

save_community_network_plot(
  q8_taylor,
  "Taylor Swift",
  "Girvan-Newman",
  q8_taylor$girvan_membership,
  "05_Plots/Q8_Taylor_Girvan_Newman.png"
)

save_community_network_plot(
  q8_taylor,
  "Taylor Swift",
  "Louvain",
  q8_taylor$louvain_membership,
  "05_Plots/Q8_Taylor_Louvain.png"
)

save_community_network_plot(
  q8_ariana,
  "Ariana Grande",
  "Girvan-Newman",
  q8_ariana$girvan_membership,
  "05_Plots/Q8_Ariana_Girvan_Newman.png"
)

save_community_network_plot(
  q8_ariana,
  "Ariana Grande",
  "Louvain",
  q8_ariana$louvain_membership,
  "05_Plots/Q8_Ariana_Louvain.png"
)

save_community_network_plot(
  q8_billie,
  "Billie Eilish",
  "Girvan-Newman",
  q8_billie$girvan_membership,
  "05_Plots/Q8_Billie_Girvan_Newman.png"
)

save_community_network_plot(
  q8_billie,
  "Billie Eilish",
  "Louvain",
  q8_billie$louvain_membership,
  "05_Plots/Q8_Billie_Louvain.png"
)

# ------------------------------------------------------------
# 14. Export all Q8 evidence
# ------------------------------------------------------------

write.csv(
  q8_community_summary,
  "03_Clean_Data/Q8_Community_Analysis_Summary.csv",
  row.names = FALSE
)

write.csv(
  q8_membership_table,
  "03_Clean_Data/Q8_Community_Memberships.csv",
  row.names = FALSE
)

write.csv(
  q8_community_composition,
  "03_Clean_Data/Q8_Community_Composition.csv",
  row.names = FALSE
)

write.csv(
  q8_video_communities,
  "03_Clean_Data/Q8_Video_Community_Comparison.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  q8_video_lookup,
  "03_Clean_Data/Q8_Video_Label_Lookup.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

save(
  q8_taylor,
  q8_ariana,
  q8_billie,
  q8_community_summary,
  q8_membership_table,
  q8_community_composition,
  q8_video_communities,
  q8_community_count_plot,
  q8_modularity_plot,
  file = "04_RData/Q8_Community_Analysis.RData"
)

# ------------------------------------------------------------
# 15. Display final Q8 results
# ------------------------------------------------------------

print(q8_community_summary)

print(q8_video_communities)

print(q8_community_count_plot)

print(q8_modularity_plot)
# ============================================================
# QUESTION 8: RECOVER MISSING COMPARISON PLOTS AND SAVE OUTPUTS
# ============================================================

stopifnot(
  exists("q8_community_summary")
)

stopifnot(
  exists("q8_video_communities")
)

library(ggplot2)
library(scales)

# Recreate the community-count plot.

q8_community_count_plot <- ggplot(
  q8_community_summary,
  aes(
    x = artist,
    y = communities,
    fill = algorithm
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = communities
    ),
    position = position_dodge(
      width = 0.75
    ),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Girvan-Newman" = "#E15759",
      "Louvain" = "#4E79A7"
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    title = "Number of Detected Audience Communities",
    subtitle = "Comparison of Girvan-Newman and Louvain",
    x = "Artist",
    y = "Number of communities",
    fill = "Algorithm"
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

# Recreate the missing modularity plot.

q8_modularity_plot <- ggplot(
  q8_community_summary,
  aes(
    x = artist,
    y = modularity,
    fill = algorithm
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = round(
        modularity,
        3
      )
    ),
    position = position_dodge(
      width = 0.75
    ),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Girvan-Newman" = "#E15759",
      "Louvain" = "#4E79A7"
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
    title = "Community Modularity Across Artist Networks",
    subtitle = "Higher modularity indicates clearer community separation",
    x = "Artist",
    y = "Weighted modularity",
    fill = "Algorithm"
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

# Save both comparison plots.

ggsave(
  filename = "05_Plots/Q8_Community_Count_Comparison.png",
  plot = q8_community_count_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "05_Plots/Q8_Modularity_Comparison.png",
  plot = q8_modularity_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Export the result tables again.

write.csv(
  q8_community_summary,
  "03_Clean_Data/Q8_Community_Analysis_Summary.csv",
  row.names = FALSE
)

write.csv(
  q8_membership_table,
  "03_Clean_Data/Q8_Community_Memberships.csv",
  row.names = FALSE
)

write.csv(
  q8_community_composition,
  "03_Clean_Data/Q8_Community_Composition.csv",
  row.names = FALSE
)

write.csv(
  q8_video_communities,
  "03_Clean_Data/Q8_Video_Community_Comparison.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

save(
  q8_taylor,
  q8_ariana,
  q8_billie,
  q8_community_summary,
  q8_membership_table,
  q8_community_composition,
  q8_video_communities,
  q8_community_count_plot,
  q8_modularity_plot,
  file = "04_RData/Q8_Community_Analysis.RData"
)

print(q8_community_summary)

print(q8_video_communities)

print(q8_modularity_plot)