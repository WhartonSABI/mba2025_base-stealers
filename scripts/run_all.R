source("scripts/00_helpers.R")

stages <- c(
  "scripts/01_fit-situational.R",
  "scripts/02_optimize-leads.R",
  "scripts/03_paper-figures.R",
  "scripts/04_pca-case-study.R",
  "internal/05_compile_paper.R"
)

for (stage in stages) {
  stage_path <- stage
  message("\n==============================")
  message("Running stage: ", stage_path)
  message("Start time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("==============================")
  source(stage_path, local = new.env(parent = globalenv()))
  message("Completed stage: ", stage_path)
}

message("\nAll stages completed successfully.")
