# Reproduce the analysis

## Start with the saved snapshot

Clone or download this repository and open `youtube-comment-analysis.Rproj`. Work from the repository root. The project disables automatic workspace restoration to avoid inheriting stale R objects.

```sh
Rscript scripts/check_project.R
Rscript scripts/setup.R
Rscript scripts/run_analysis.R all
```

`setup.R` installs missing analysis packages from CRAN. A working R installation and an internet connection are required for installation. The original study used R 4.6.1; package versions, numerical routines and random algorithms can affect exact rerun values. The committed `results/` files are the published snapshot, and reruns are written to separate local folders.

## Run a smaller stage

| Command suffix | Modules run |
|---|---|
| `participation` | Q3 actor networks; Q4 unique/repeat actors |
| `text` | Q5 preprocessing/TDM; Q6 semantic bigrams |
| `networks` | Q3 networks; Q7 centrality; Q8 communities |
| `sentiment` | Q9 polarity and emotion counts |
| `model` | Q5 preprocessing; Q10 baseline/tuned tree |
| `topics` | Q5 preprocessing; Q11 LDA |
| `all` | Q3 through Q11 in dependency order |

Example: `Rscript scripts/run_analysis.R model`.

`bootstrap_data.R` checks the clean CSV and selected-video metadata, then creates `04_RData/Q2_YouTube_Comments_Clean.RData` and `04_RData/Q2_Selected_Videos.RData`. Each analysis module runs in a fresh environment and loads its declared inputs. Q3 supplies the graphs needed by Q7/Q8; Q5 supplies text inputs needed by Q6/Q10/Q11.

New outputs are written to `03_Clean_Data/`, `04_RData/` and `05_Plots/`; these generated directories are ignored by Git. The runner saves `session-info.txt` with package/session information. This prevents an exploratory run from silently replacing the committed published results.

## README charts

The README charts are generated from the committed aggregate tables:

```sh
python3 -m pip install numpy matplotlib
python3 scripts/build_readme_charts.py --data-dir results/tables --output-dir docs/images
```

Python is only used for these portfolio summary charts. The analytical models and the original detailed figures are implemented in R.

## Optional fresh YouTube collection

`R/collection/historical_collection.R` records the original search, selection and capped-collection workflow. It reads OAuth client details from environment variables. By default it loads the public saved snapshot; a live recollection must be explicitly enabled through the `q2.recollect` R option.

```r
source("scripts/setup.R")  # Install tuber separately for live collection.
install.packages("tuber")
Sys.setenv(YOUTUBE_CLIENT_ID = "your_client_id",
           YOUTUBE_CLIENT_SECRET = "your_client_secret")
options(q2.recollect = TRUE)
source("R/collection/historical_collection.R")
```

Use your own Google OAuth configuration; complete the interactive authorization required by tuber. Do not commit credentials or OAuth caches. API access, quota and comment availability apply. Fresh searches can select different videos or comments, so a new collection does not reproduce the historical snapshot exactly and requires rerunning downstream analyses.

`revised_collection.R` is a separate refinement: it targets top-level comments and retrieves available replies, with local checkpointing. It requires local historical collection inputs. It was not the source of the reported 10,800-comment results; no completed revised collection is claimed.

## Verification scope

`check_project.R` checks R syntax, dataset counts, stable public identifiers, unique/repeat participation, sentiment totals and the test confusion-matrix accuracy. These are fast data-contract checks, not a substitute for refitting every model.

Saved CSVs and figures were inspected during preparation. The Power BI project can be checked structurally, but actual Desktop opening, refresh and interactive filtering must be checked in Power BI. There is no claim that this repository alone proves a native-app test.
