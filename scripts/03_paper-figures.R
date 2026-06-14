source("scripts/00_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(scales)
})

ensure_dirs(c("paper/figures", "paper/tables", "internal"))

message("03_make_paper_figures.R starting")

leads_full <- read.csv("data/processed/leadsnew1b_full_v2.csv", stringsAsFactors = FALSE)

leads_full <- leads_full %>%
  mutate(
    leadDev_player = as.numeric(leadDev_player),
    RunnerTeam = case_when(
      TopBottom == "T" ~ Away,
      TopBottom == "B" ~ Home,
      TRUE ~ NA_character_
    )
  )

# ----------------------------
# Main lead deviation histograms
# ----------------------------
mean_all <- mean(leads_full$leadDev_player, na.rm = TRUE)
max_count_all <- max(hist(leads_full$leadDev_player, breaks = 40, plot = FALSE)$counts)

fig_all <- ggplot(leads_full, aes(x = leadDev_player)) +
  geom_histogram(bins = 40, fill = "#7aa6c2", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean_all, color = "#b22222", linewidth = 1) +
  annotate(
    "text",
    x = mean_all,
    y = max_count_all * 0.95,
    label = paste0("Mean = ", sprintf("%.2f", mean_all), " ft"),
    color = "#b22222",
    fontface = "bold",
    hjust = ifelse(mean_all >= 0, -0.05, 1.05),
    vjust = 1
  ) +
  labs(
    title = "Lead Deviation Distribution (All Situations)",
    x = "Lead Deviation = Observed Lead - Optimal Lead (ft)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "paper/figures/Lead-Error-All-MMNew.png",
  plot = fig_all,
  width = 10,
  height = 6,
  dpi = 300
)

sb_attempts <- leads_full %>% filter(SB1 == 1 | CS1 == 1)
mean_sb <- mean(sb_attempts$leadDev_player, na.rm = TRUE)
max_count_sb <- max(hist(sb_attempts$leadDev_player, breaks = 35, plot = FALSE)$counts)

fig_sb <- ggplot(sb_attempts, aes(x = leadDev_player)) +
  geom_histogram(bins = 35, fill = "#4f9a94", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean_sb, color = "#b22222", linewidth = 1) +
  annotate(
    "text",
    x = mean_sb,
    y = max_count_sb * 0.95,
    label = paste0("Mean = ", sprintf("%.2f", mean_sb), " ft"),
    color = "#b22222",
    fontface = "bold",
    hjust = ifelse(mean_sb >= 0, -0.05, 1.05),
    vjust = 1
  ) +
  labs(
    title = "Lead Deviation Distribution (Steal Attempts)",
    x = "Lead Deviation = Observed Lead - Optimal Lead (ft)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "paper/figures/Lead-Error-SB-MMNew.png",
  plot = fig_sb,
  width = 10,
  height = 6,
  dpi = 300
)

# ----------------------------
# Team-level mean absolute error
# ----------------------------
team_colors <- c(
  ARI = "#A71930", ATL = "#CE1141", BAL = "#DF4601", BOS = "#BD3039",
  CHC = "#0E3386", CIN = "#C6011F", CLE = "#0C2340", COL = "#33006F",
  CWS = "#27251F", DET = "#0C2340", HOU = "#EB6E1F", KC = "#004687",
  LAA = "#BA0021", LAD = "#005A9C", MIA = "#00A3E0", MIL = "#12284B",
  MIN = "#002B5C", NYM = "#002D72", NYY = "#132448", OAK = "#003831",
  PHI = "#E81828", PIT = "#FDB827", SD = "#2F241D", SEA = "#005C5C",
  SF = "#FD5A1E", STL = "#C41E3A", TB = "#092C5C", TEX = "#003278",
  TOR = "#134A8E", WSH = "#AB0003"
)

team_tbl <- leads_full %>%
  filter(!is.na(RunnerTeam), !is.na(leadDev_player)) %>%
  group_by(RunnerTeam) %>%
  summarise(
    TeamAvgAbs = mean(abs(leadDev_player), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fig_team <- ggplot(team_tbl, aes(x = reorder(RunnerTeam, TeamAvgAbs), y = TeamAvgAbs, fill = RunnerTeam)) +
  geom_col(color = "black", linewidth = 0.15) +
  scale_fill_manual(values = team_colors, na.value = "grey70") +
  labs(
    title = "Average Absolute Lead Deviation by Team",
    x = "Team",
    y = "Mean |Observed - Optimal| (ft)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = "paper/figures/team-error-lead-MMNew.png",
  plot = fig_team,
  width = 12,
  height = 7,
  dpi = 300
)

# ----------------------------
# Runner-level appendix tables
# ----------------------------

save_latex_table <- function(
  df,
  filename,
  caption,
  label,
  align,
  stripe_color
) {
  tabular <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    align = align,
    linesep = "",
    escape = TRUE
  )

  tabular_lines <- strsplit(tabular, "\n", fixed = TRUE)[[1]]
  top_rule_idx <- match("\\toprule", tabular_lines)
  if (!is.na(top_rule_idx)) {
    tabular_lines <- append(
      tabular_lines,
      values = paste0("\\rowcolor{", stripe_color, "!25}"),
      after = top_rule_idx
    )
  }

  out_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\small",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\rowcolors{2}{", stripe_color, "!10}{white}"),
    tabular_lines,
    "\\end{table}"
  )

  writeLines(out_lines, con = filename)
}

save_rank_bar <- function(
  df,
  filename,
  title,
  fill_color,
  width_px = 1160,
  height_px = 460
) {
  plot_df <- df %>%
    mutate(
      Runner1B = factor(Runner1B, levels = rev(Runner1B)),
      label = sprintf("%.2f", total_cost)
    )

  fig <- ggplot(plot_df, aes(x = total_cost, y = Runner1B)) +
    geom_col(fill = fill_color, width = 0.7) +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = 3.6,
      color = "#333333"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.18)),
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = title,
      x = "Total xRuns Lost",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = filename,
    plot = fig,
    width = width_px / 150,
    height = height_px / 150,
    dpi = 150
  )
}

runner_tbl <- leads_full %>%
    group_by(Runner1B) %>%
    summarise(
        meanChange = mean(leadDev_player, na.rm = TRUE),
        absChange  = mean(abs(leadDev_player), na.rm = TRUE),
        n_obs      = n(),
        .groups    = "drop"
    ) %>%
    filter(n_obs >= 15)

# Build team labels per runner (concatenate with "/" for traded players)
runner_team <- leads_full %>%
    filter(!is.na(RunnerTeam)) %>%
    group_by(Runner1B) %>%
    summarise(Team = paste(unique(RunnerTeam), collapse = "/"), .groups = "drop")

runner_tbl <- runner_tbl %>% left_join(runner_team, by = "Runner1B")

# Closest to optimal
closest_table <- runner_tbl %>%
    arrange(absChange) %>%
    slice_head(n = 10) %>%
    mutate(absChange = round(absChange, 2)) %>%
    select(Runner1B, Team, absChange, n_obs) %>%
    rename(
      Runner = Runner1B,
      Team = Team,
      `Absolute Deviation (ft)` = absChange,
      Observations = n_obs
    )

save_latex_table(
  closest_table,
  filename = "paper/tables/closest_to_optimal_leads.tex",
  caption = "Baserunners with the smallest absolute average lead deviation among runners with at least 15 observations.",
  label = "tab:runners_closest",
  align = c("l", "c", "r", "r"),
  stripe_color = "green"
)

# Conservative runners
conservative_table <- runner_tbl %>%
    arrange(meanChange) %>%
    slice_head(n = 10) %>%
    mutate(meanChange = round(meanChange, 2)) %>%
    select(Runner1B, Team, meanChange, n_obs) %>%
    rename(
      Runner = Runner1B,
      Team = Team,
      `Mean Deviation (ft)` = meanChange,
      Observations = n_obs
    )

save_latex_table(
  conservative_table,
  filename = "paper/tables/conservative_leads.tex",
  caption = "Baserunners with the most negative average lead deviations among runners with at least 15 observations.",
  label = "tab:runners_conservative",
  align = c("l", "c", "r", "r"),
  stripe_color = "blue"
)

# Aggressive runners
aggressive_table <- runner_tbl %>%
    arrange(desc(meanChange)) %>%
    slice_head(n = 10) %>%
    mutate(meanChange = round(meanChange, 2)) %>%
    select(Runner1B, Team, meanChange, n_obs) %>%
    rename(
      Runner = Runner1B,
      Team = Team,
      `Mean Deviation (ft)` = meanChange,
      Observations = n_obs
    )

save_latex_table(
  aggressive_table,
  filename = "paper/tables/aggressive_leads.tex",
  caption = "Baserunners with the most positive average lead deviations among runners with at least 15 observations.",
  label = "tab:runners_aggressive",
  align = c("l", "c", "r", "r"),
  stripe_color = "red"
)

# ----------------------------
# Team and player run-cost figures
# ----------------------------
team_cost_tbl <- leads_full %>%
  filter(!is.na(RunnerTeam), !is.na(xRuns_lost)) %>%
  group_by(RunnerTeam) %>%
  summarise(
    total_cost = sum(xRuns_lost, na.rm = TRUE),
    .groups = "drop"
  )

fig_team_cost <- ggplot(team_cost_tbl, aes(x = reorder(RunnerTeam, total_cost), y = total_cost, fill = RunnerTeam)) +
  geom_col(color = "black", linewidth = 0.15) +
  scale_fill_manual(values = team_colors, na.value = "grey70") +
  labs(
    title = "Total Model-Implied Run Cost by Team",
    x = "Team",
    y = "Total xRuns Lost"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = "paper/figures/team-run-cost.png",
  plot = fig_team_cost,
  width = 12,
  height = 7,
  dpi = 300
)

runner_cost_tbl <- leads_full %>%
  group_by(Runner1B) %>%
  summarise(
    meanChange = mean(leadDev_player, na.rm = TRUE),
    total_cost = sum(xRuns_lost, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 15) %>%
  left_join(runner_team, by = "Runner1B")

cost_aggressive <- runner_cost_tbl %>%
  filter(meanChange > 0) %>%
  arrange(desc(total_cost)) %>%
  slice_head(n = 5)

cost_conservative <- runner_cost_tbl %>%
  filter(meanChange < 0) %>%
  arrange(desc(total_cost)) %>%
  slice_head(n = 5)

save_rank_bar(
  cost_aggressive,
  filename = "paper/figures/run_cost_aggressive.png",
  title = "Most Costly Aggressive Runners",
  fill_color = "#b22222"
)

save_rank_bar(
  cost_conservative,
  filename = "paper/figures/run_cost_conservative.png",
  title = "Most Costly Conservative Runners",
  fill_color = "#1f4e79"
)

sink("internal/03_figure_summary.txt")
cat("Figure generation complete\n")
cat("Mean lead deviation (all):", mean_all, "\n")
cat("Mean lead deviation (SB attempts):", mean_sb, "\n")
cat("Runner sample (n >= 15):", nrow(runner_tbl), "\n")
cat("Total xRuns lost:", sum(leads_full$xRuns_lost, na.rm = TRUE), "\n")
sink()

message("03_make_paper_figures.R complete")
