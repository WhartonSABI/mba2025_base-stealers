library(tidyverse)
library(ggplot2)

# =========================
# 1. LOAD DATA (NEW)
# =========================
# If you already have leadsnew1b_full in memory, skip the read.
# Otherwise:
leadsnew1b_full <- read_csv("data/full_leads_mixed_player.csv")


leadTeamNew <- leadsnew1b_full %>%
  select(
    Date, Home, Away, TopBottom,
    Play, Runner1B,
    SB1, CS1, PK1,
    leadDev_player
  ) %>%
  mutate(
    RunnerTeam = case_when(
      TopBottom == "T" ~ Away,
      TopBottom == "B" ~ Home,
      TRUE ~ NA_character_
    )) %>%
  filter(!is.na(RunnerTeam), !is.na(leadDev_player))



# Mean absolute lead deviation by team
leadTeamOnlyNew <- leadTeamNew %>%
  group_by(RunnerTeam) %>%
  summarise(
    TeamAvg = mean(abs(leadDev_player), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# =========================
# 3. PLOT: AVERAGE LEAD ERROR BY TEAM
# =========================

team_colors <- c(
  ARI = "#A71930", ATL = "#CE1141", BAL = "#DF4601", BOS = "#BD3039",
  CHC = "#0E3386", CIN = "#C6011F", CLE = "#0C2340", COL = "#33006F",
  CWS = "#27251F", DET = "#0C2340", HOU = "#EB6E1F", KC  = "#004687",
  LAA = "#BA0021", LAD = "#005A9C", MIA = "#00A3E0", MIL = "#12284B",
  MIN = "#002B5C", NYM = "#002D72", NYY = "#132448", OAK = "#003831",
  PHI = "#E81828", PIT = "#FDB827", SD  = "#2F241D", SEA = "#005C5C",
  SF  = "#FD5A1E", STL = "#C41E3A", TB  = "#092C5C", TEX = "#003278",
  TOR = "#134A8E", WSH = "#AB0003"
)

team_lead_error_plotNew <- ggplot(
  leadTeamOnlyNew,
  aes(x = reorder(RunnerTeam, TeamAvg), y = TeamAvg, fill = RunnerTeam)
) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = team_colors, na.value = "grey70") +
  labs(
    title = "Average Lead Error by Team",
    x = "Team",
    y = "Mean |PrimaryLead1B − OptimalLead| (ft)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14),
    plot.title  = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )

print(team_lead_error_plotNew)

ggsave("figures/team_lead_error_plotNew.png", team_lead_error_plotNew, width = 10, height = 6, dpi = 300)












































