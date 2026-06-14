source("scripts/00_helpers.R")

ensure_dirs("internal")

message("05_compile_paper.R starting")

paper_dir <- "paper"
paper_file <- "paper.tex"

compile_cmd <- sprintf(
  "cd %s && latexmk -pdf -interaction=nonstopmode -halt-on-error %s",
  shQuote(paper_dir),
  shQuote(paper_file)
)

result <- system(compile_cmd, intern = TRUE, ignore.stderr = FALSE)
writeLines(result, "internal/05_compile_paper.txt")

message("05_compile_paper.R complete")
