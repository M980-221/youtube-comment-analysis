# Usage: Rscript scripts/run_analysis.R [all|participation|text|networks|sentiment|model|topics]
args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[1] else "all"
modules <- list(
  participation = c("Q3_Actor_Networks.R", "Q4_Unique_Actors.R"),
  text = c("Q5_Term_Document_Matrices.R", "Q6_Semantic_Bigram_Networks.R"),
  networks = c("Q3_Actor_Networks.R", "Q7_Centrality_Analysis.R", "Q8_Community_Analysis.R"),
  sentiment = "Q9_Sentiment_Analysis.R",
  model = c("Q5_Term_Document_Matrices.R", "Q10_Decision_Tree.R"),
  topics = c("Q5_Term_Document_Matrices.R", "Q11_LDA_Topic_Modelling.R")
)
if (!stage %in% c("all", names(modules))) stop("Unknown stage: ", stage)
source("scripts/bootstrap_data.R", local = new.env(parent = globalenv()))
files <- if (stage == "all") c("Q3_Actor_Networks.R", "Q4_Unique_Actors.R",
  "Q5_Term_Document_Matrices.R", "Q6_Semantic_Bigram_Networks.R",
  "Q7_Centrality_Analysis.R", "Q8_Community_Analysis.R", "Q9_Sentiment_Analysis.R",
  "Q10_Decision_Tree.R", "Q11_LDA_Topic_Modelling.R") else modules[[stage]]
for (file in files) {
  message("Running ", file)
  # Each module loads its declared snapshots instead of inheriting stale objects.
  source(file.path("R", file), local = new.env(parent = globalenv()), encoding = "UTF-8")
}
writeLines(capture.output(sessionInfo()), "04_RData/session-info.txt")
message("Finished. New tables, RData snapshots and figures are in 03_Clean_Data, 04_RData and 05_Plots.")
