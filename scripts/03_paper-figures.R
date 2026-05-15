source("scripts/00_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

ensure_dirs(c("paper/figures", "internal"))

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

fig_all <- ggplot(leads_full, aes(x = leadDev_player)) +
  geom_histogram(bins = 40, fill = "#7aa6c2", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean_all, color = "#b22222", linewidth = 1) +
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

fig_sb <- ggplot(sb_attempts, aes(x = leadDev_player)) +
  geom_histogram(bins = 35, fill = "#4f9a94", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean_sb, color = "#b22222", linewidth = 1) +
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
# Runner-level appendix charts
# ----------------------------
runner_tbl <- leads_full %>%
  group_by(Runner1B) %>%
  summarise(
    meanChange = mean(leadDev_player, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 15) %>%
  mutate(absChange = abs(meanChange))

fig_closest <- ggplot(
  runner_tbl %>% arrange(absChange) %>% slice_head(n = 20),
  aes(x = reorder(Runner1B, absChange), y = absChange)
) +
  geom_col(fill = "#40b8c4") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Runners Closest to Optimal Lead Distance",
    x = "Runner",
    y = "|Mean Lead Deviation| (ft)"
  )

ggsave(
  filename = "paper/figures/closest_to_optimal_leads.png",
  plot = fig_closest,
  width = 10,
  height = 8,
  dpi = 300
)

fig_conservative <- ggplot(
  runner_tbl %>% arrange(meanChange) %>% slice_head(n = 20),
  aes(x = reorder(Runner1B, meanChange), y = meanChange)
) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Runners Taking Shorter Leads Than Optimal (Conservative)",
    x = "Runner",
    y = "Mean Lead Deviation (ft)"
  )

ggsave(
  filename = "paper/figures/conservative_leads.png",
  plot = fig_conservative,
  width = 10,
  height = 8,
  dpi = 300
)

fig_aggressive <- ggplot(
  runner_tbl %>% arrange(desc(meanChange)) %>% slice_head(n = 20),
  aes(x = reorder(Runner1B, meanChange), y = meanChange)
) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Runners Taking Bigger Leads Than Optimal (Aggressive)",
    x = "Runner",
    y = "Mean Lead Deviation (ft)"
  )

ggsave(
  filename = "paper/figures/aggressive_leads.png",
  plot = fig_aggressive,
  width = 10,
  height = 8,
  dpi = 300
)

sink("internal/03_figure_summary.txt")
cat("Figure generation complete\n")
cat("Mean lead deviation (all):", mean_all, "\n")
cat("Mean lead deviation (SB attempts):", mean_sb, "\n")
cat("Runner sample (n >= 15):", nrow(runner_tbl), "\n")
sink()

message("03_make_paper_figures.R complete")
