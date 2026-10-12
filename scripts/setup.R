# Install the packages used by the analysis modules.
options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("igraph", "ggplot2", "tm", "slam", "scales", "tidytext", "dplyr",
              "tidyr", "sentimentr", "syuzhet", "rpart", "rpart.plot", "caret", "topicmodels")
if ("--with-api" %in% commandArgs(trailingOnly = TRUE)) packages <- c(packages, "tuber")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
message("Required packages are available. See docs/reproduction.md for execution order.")
