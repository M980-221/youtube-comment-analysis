# ============================================================
# 3032ICT ASSIGNMENT
# QUESTION 3: ACTOR NETWORKS AND TOP 5 ACTORS USING PAGERANK
# ============================================================

# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

required_packages <- c(
  "igraph",
  "ggplot2"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(igraph)
library(ggplot2)

# ------------------------------------------------------------
# 2. Load the clean Q2 dataset if it is not in the Environment
# ------------------------------------------------------------

if (!exists("youtube_comments_clean")) {
  load("04_RData/Q2_YouTube_Comments_Clean.RData")
}

stopifnot(exists("youtube_comments_clean"))
stopifnot(nrow(youtube_comments_clean) == 10800)

required_columns <- c(
  "comment_id",
  "artist",
  "video_id",
  "video_title",
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
# 3. Prepare valid author records
# ------------------------------------------------------------

q3_actor_data <- youtube_comments_clean[
  !is.na(youtube_comments_clean$author_channel_id) &
    trimws(youtube_comments_clean$author_channel_id) != "",
  required_columns
]

q3_actor_data$author_node <- paste0(
  "actor::",
  q3_actor_data$author_channel_id
)

q3_actor_data$video_node <- paste0(
  "video::",
  q3_actor_data$video_id
)

# One weighted edge represents the number of comments that an
# actor posted on a selected video.

q3_edges <- aggregate(
  comment_id ~
    artist +
    author_channel_id +
    author_node +
    video_id +
    video_node,
  data = q3_actor_data,
  FUN = length
)

names(q3_edges)[6] <- "comment_frequency"

# ------------------------------------------------------------
# 4. Function to create one network and calculate PageRank
# ------------------------------------------------------------

create_actor_network <- function(artist_name) {
  
  artist_edges <- q3_edges[
    q3_edges$artist == artist_name,
  ]
  
  network_edges <- data.frame(
    from = artist_edges$author_node,
    to = artist_edges$video_node,
    weight = artist_edges$comment_frequency
  )
  
  actor_network <- graph_from_data_frame(
    network_edges,
    directed = FALSE
  )
  
  V(actor_network)$node_type <- ifelse(
    grepl("^actor::", V(actor_network)$name),
    "actor",
    "video"
  )
  
  pagerank_result <- page_rank(
    actor_network,
    directed = FALSE,
    weights = E(actor_network)$weight
  )$vector
  
  actor_names <- V(actor_network)$name[
    V(actor_network)$node_type == "actor"
  ]
  
  actor_results <- data.frame(
    artist = artist_name,
    author_channel_id = sub(
      "^actor::",
      "",
      actor_names
    ),
    page_rank = as.numeric(
      pagerank_result[actor_names]
    ),
    comments_made = as.numeric(
      strength(
        actor_network,
        vids = actor_names,
        weights = E(actor_network)$weight
      )
    ),
    videos_commented_on = as.numeric(
      degree(
        actor_network,
        v = actor_names
      )
    )
  )
  
  actor_results <- actor_results[
    order(
      -actor_results$page_rank,
      -actor_results$comments_made,
      actor_results$author_channel_id
    ),
  ]
  
  row.names(actor_results) <- NULL
  actor_results$rank <- seq_len(nrow(actor_results))
  
  list(
    network = actor_network,
    ranking = actor_results,
    edges = artist_edges
  )
}

# ------------------------------------------------------------
# 5. Create networks for all three artists
# ------------------------------------------------------------

q3_taylor <- create_actor_network(
  "Taylor Swift"
)

q3_ariana <- create_actor_network(
  "Ariana Grande"
)

q3_billie <- create_actor_network(
  "Billie Eilish"
)

q3_top5_actors <- rbind(
  head(q3_taylor$ranking, 5),
  head(q3_ariana$ranking, 5),
  head(q3_billie$ranking, 5)
)

q3_top5_actors <- q3_top5_actors[
  order(
    q3_top5_actors$artist,
    q3_top5_actors$rank
  ),
]

row.names(q3_top5_actors) <- NULL

# ------------------------------------------------------------
# 6. Validate the Q3 results
# ------------------------------------------------------------

stopifnot(
  nrow(q3_top5_actors) == 15
)

stopifnot(
  all(table(q3_top5_actors$artist) == 5)
)

stopifnot(
  all(q3_top5_actors$page_rank > 0)
)

q3_network_summary <- data.frame(
  artist = c(
    "Taylor Swift",
    "Ariana Grande",
    "Billie Eilish"
  ),
  unique_actors = c(
    nrow(q3_taylor$ranking),
    nrow(q3_ariana$ranking),
    nrow(q3_billie$ranking)
  ),
  video_nodes = c(
    sum(
      V(q3_taylor$network)$node_type == "video"
    ),
    sum(
      V(q3_ariana$network)$node_type == "video"
    ),
    sum(
      V(q3_billie$network)$node_type == "video"
    )
  ),
  weighted_connections = c(
    sum(E(q3_taylor$network)$weight),
    sum(E(q3_ariana$network)$weight),
    sum(E(q3_billie$network)$weight)
  )
)

# ------------------------------------------------------------
# 7. Create the top-five PageRank comparison plot
# ------------------------------------------------------------

q3_top5_plot_data <- q3_top5_actors

q3_top5_plot_data$actor_label <- paste0(
  substr(
    q3_top5_plot_data$author_channel_id,
    1,
    10
  ),
  "..."
)

q3_top5_plot_data$actor_label <- factor(
  q3_top5_plot_data$actor_label,
  levels = rev(
    q3_top5_plot_data$actor_label
  )
)

q3_top5_plot <- ggplot(
  q3_top5_plot_data,
  aes(
    x = actor_label,
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
  labs(
    title = "Top Five YouTube Actors by PageRank",
    subtitle = paste(
      "Weighted author-video networks",
      "for the three selected artists"
    ),
    x = "Author channel ID",
    y = "PageRank score"
  ) +
  scale_fill_manual(
    values = c(
      "Taylor Swift" = "#4E79A7",
      "Ariana Grande" = "#B07AA1",
      "Billie Eilish" = "#59A14F"
    )
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
  filename = "05_Plots/Q3_Top5_Actors_PageRank.png",
  plot = q3_top5_plot,
  width = 10,
  height = 11,
  dpi = 300
)

# ------------------------------------------------------------
# 8. Create readable network plots using the top 15 actors
# ------------------------------------------------------------

save_actor_network_plot <- function(
    network_result,
    artist_name,
    file_name,
    actor_colour
) {
  
  top_actor_nodes <- paste0(
    "actor::",
    head(
      network_result$ranking$author_channel_id,
      15
    )
  )
  
  connected_video_nodes <- unique(
    unlist(
      adjacent_vertices(
        network_result$network,
        top_actor_nodes,
        mode = "all"
      )
    )
  )
  
  selected_nodes <- unique(
    c(
      top_actor_nodes,
      names(connected_video_nodes)
    )
  )
  
  plot_network <- induced_subgraph(
    network_result$network,
    vids = selected_nodes
  )
  
  V(plot_network)$label <- ifelse(
    V(plot_network)$node_type == "actor",
    paste0(
      substr(
        sub("^actor::", "", V(plot_network)$name),
        1,
        7
      ),
      "..."
    ),
    paste0(
      "Video ",
      match(
        V(plot_network)$name,
        V(plot_network)$name[
          V(plot_network)$node_type == "video"
        ]
      )
    )
  )
  
  V(plot_network)$color <- ifelse(
    V(plot_network)$node_type == "actor",
    actor_colour,
    "#F28E2B"
  )
  
  V(plot_network)$shape <- ifelse(
    V(plot_network)$node_type == "actor",
    "circle",
    "square"
  )
  
  V(plot_network)$size <- ifelse(
    V(plot_network)$node_type == "actor",
    8,
    13
  )
  
  png(
    filename = file_name,
    width = 2400,
    height = 1800,
    res = 250
  )
  
  set.seed(3032)
  
  plot(
    plot_network,
    layout = layout_with_fr(
      plot_network
    ),
    main = paste(
      artist_name,
      "Actor Network"
    ),
    vertex.label.cex = 0.65,
    vertex.label.color = "black",
    vertex.frame.color = "white",
    edge.color = adjustcolor(
      "grey50",
      alpha.f = 0.55
    ),
    edge.width = pmax(
      1,
      log1p(E(plot_network)$weight)
    ),
    margin = 0.08
  )
  
  legend(
    "topleft",
    legend = c(
      "YouTube actor",
      "Selected video"
    ),
    col = c(
      actor_colour,
      "#F28E2B"
    ),
    pch = c(
      19,
      15
    ),
    pt.cex = 1.7,
    bty = "n"
  )
  
  dev.off()
}

save_actor_network_plot(
  q3_taylor,
  "Taylor Swift",
  "05_Plots/Q3_Taylor_Actor_Network.png",
  "#4E79A7"
)

save_actor_network_plot(
  q3_ariana,
  "Ariana Grande",
  "05_Plots/Q3_Ariana_Actor_Network.png",
  "#B07AA1"
)

save_actor_network_plot(
  q3_billie,
  "Billie Eilish",
  "05_Plots/Q3_Billie_Actor_Network.png",
  "#59A14F"
)

# ------------------------------------------------------------
# 9. Export Q3 evidence
# ------------------------------------------------------------

write.csv(
  q3_top5_actors,
  "03_Clean_Data/Q3_Top5_Actors_PageRank.csv",
  row.names = FALSE
)

write.csv(
  q3_network_summary,
  "03_Clean_Data/Q3_Actor_Network_Summary.csv",
  row.names = FALSE
)

save(
  q3_taylor,
  q3_ariana,
  q3_billie,
  q3_top5_actors,
  q3_network_summary,
  file = "04_RData/Q3_Actor_Networks.RData"
)

# ------------------------------------------------------------
# 10. Display the final Q3 results
# ------------------------------------------------------------

print(q3_network_summary)

print(
  q3_top5_actors[, c(
    "artist",
    "rank",
    "author_channel_id",
    "page_rank",
    "comments_made",
    "videos_commented_on"
  )]
)

print(q3_top5_plot)
