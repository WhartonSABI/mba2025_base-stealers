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
suppressPackageStartupMessages({
    library(gt)
    library(scales)
})

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
    gt() %>%
    tab_header(
        title = md("**Runners Closest to Optimal**"),
        subtitle = "Smallest absolute deviation from optimal lead (min. 15 observations)"
    ) %>%
    cols_label(
        Runner1B = "Runner",
        Team = "Team",
        absChange = "Absolute Deviation (ft)",
        n_obs = "Observations"
    ) %>%
    cols_width(
        Runner1B ~ px(150),
        Team ~ px(75),
        absChange ~ px(180),
        n_obs ~ px(110)
    ) %>%
    data_color(
        columns = absChange,
        colors = scales::col_numeric(
            palette = c("#B7D9BE", "#6FAE86"),
            domain = NULL,
            reverse = TRUE
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(weight = "bold", size = px(15)),
            cell_fill(color = "#f0f0f0")
        ),
        locations = cells_column_labels()
    ) %>%
    tab_style(
        style = cell_text(size = px(14)),
        locations = cells_body()
    ) %>%
    cols_align(align = "center", columns = c(Team, absChange, n_obs)) %>%
    cols_align(align = "left", columns = Runner1B) %>%
    opt_row_striping() %>%
    tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(20),
        heading.subtitle.font.size = px(13),
        table.width = px(520),
        data_row.padding = px(6)
    )

gtsave(closest_table, "paper/figures/closest_to_optimal_leads.png")

# Conservative runners
conservative_table <- runner_tbl %>%
    arrange(meanChange) %>%
    slice_head(n = 10) %>%
    mutate(meanChange = round(meanChange, 2)) %>%
    select(Runner1B, Team, meanChange, n_obs) %>%
    gt() %>%
    tab_header(
        title = md("**Most Conservative Runners**"),
        subtitle = "Taking Shorter Leads Than Optimal (min. 15 observations)"
    ) %>%
    cols_label(
        Runner1B = "Runner",
        Team = "Team",
        meanChange = "Mean Deviation (ft)",
        n_obs = "Observations"
    ) %>%
    cols_width(
        Runner1B ~ px(150),
        Team ~ px(75),
        meanChange ~ px(160),
        n_obs ~ px(110)
    ) %>%
    data_color(
        columns = meanChange,
        colors = scales::col_numeric(
            palette = c("#00008B", "#ADD8E6"),
            domain = NULL
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(weight = "bold", size = px(15)),
            cell_fill(color = "#f0f0f0")
        ),
        locations = cells_column_labels()
    ) %>%
    tab_style(
        style = cell_text(size = px(14)),
        locations = cells_body()
    ) %>%
    cols_align(align = "center", columns = c(Team, meanChange, n_obs)) %>%
    cols_align(align = "left", columns = Runner1B) %>%
    opt_row_striping() %>%
    tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(20),
        heading.subtitle.font.size = px(13),
        table.width = px(500),
        data_row.padding = px(6)
    )

gtsave(conservative_table, "paper/figures/conservative_leads.png")

# Aggressive runners
aggressive_table <- runner_tbl %>%
    arrange(desc(meanChange)) %>%
    slice_head(n = 10) %>%
    mutate(meanChange = round(meanChange, 2)) %>%
    select(Runner1B, Team, meanChange, n_obs) %>%
    gt() %>%
    tab_header(
        title = md("**Most Aggressive Runners**"),
        subtitle = "Taking Longer Leads Than Optimal (min. 15 observations)"
    ) %>%
    cols_label(
        Runner1B = "Runner",
        Team = "Team",
        meanChange = "Mean Deviation (ft)",
        n_obs = "Observations"
    ) %>%
    cols_width(
        Runner1B ~ px(150),
        Team ~ px(75),
        meanChange ~ px(160),
        n_obs ~ px(110)
    ) %>%
    data_color(
        columns = meanChange,
        colors = scales::col_numeric(
            palette = c("#FFB6C1", "#8B0000"),
            domain = NULL
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(weight = "bold", size = px(15)),
            cell_fill(color = "#f0f0f0")
        ),
        locations = cells_column_labels()
    ) %>%
    tab_style(
        style = cell_text(size = px(14)),
        locations = cells_body()
    ) %>%
    cols_align(align = "center", columns = c(Team, meanChange, n_obs)) %>%
    cols_align(align = "left", columns = Runner1B) %>%
    opt_row_striping() %>%
    tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(20),
        heading.subtitle.font.size = px(13),
        table.width = px(500),
        data_row.padding = px(6)
    )

gtsave(aggressive_table, "paper/figures/aggressive_leads.png")

sink("internal/03_figure_summary.txt")
cat("Figure generation complete\n")
cat("Mean lead deviation (all):", mean_all, "\n")
cat("Mean lead deviation (SB attempts):", mean_sb, "\n")
cat("Runner sample (n >= 15):", nrow(runner_tbl), "\n")
sink()

message("03_make_paper_figures.R complete")
