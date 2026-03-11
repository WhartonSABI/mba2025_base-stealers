##Looking at runners WITH NEW MODEL 
leadRunnersNew <- leadsnew1b_full %>%
  group_by(Runner1B) %>%
  summarise(
    meanChange = mean(leadDev_player, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 15) %>%  # FIX: Only include players with meaningful sample
  mutate(absChange = abs(meanChange)) %>%
  arrange(absChange)


leadRunnersNew <-leadRunnersNew %>%
  mutate(absChange = abs(meanChange)) %>%
  arrange(absChange) 

leadRunnersNew


p1 <- ggplot(leadRunnersNew %>% slice_head(n = 20),
             aes(x = reorder(Runner1B, absChange), y = absChange)) +
  geom_col(fill = "#40b8c4") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    x = "Runner",
    y = "|Mean Lead Change| (ft)",
    title = "Runners Closest to Optimal Lead Distance"
  )


print(p1)
print(p2)
print(p3)
ggsave("figures/closest_to_optimal_leads.png",
       plot = p1,
       width = 10, height = 8, dpi = 300,)

p2 <- ggplot(leadRunnersNew %>% slice_min(meanChange, n = 20),
             aes(x = reorder(Runner1B, meanChange), y = meanChange)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    x = "Runner",
    y = "Mean Lead Change (ft)",
    title = "Runners Taking Shorter Leads Than Optimal (Conservative)"
  ) +
  theme_minimal(base_size = 14)

ggsave("figures/conservative_leads.png",
       plot = p2,
       width = 10, height = 8, dpi = 300)

p3 <- ggplot(leadRunnersNew %>% slice_max(meanChange, n = 20),
             aes(x = reorder(Runner1B, meanChange), y = meanChange)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    x = "Runner",
    y = "Mean Lead Change (ft)",
    title = "Runners Taking Bigger Leads Than Optimal (Aggressive)"
  ) +
  theme_minimal(base_size = 14)

ggsave("figures/aggressive_leads.png",
       plot = p3,
       width = 10, height = 8, dpi = 300)



# =============================================================================
# TOP 10 CLOSEST TO OPTIMAL - SHOW TEAMS AS "TB/MIA"
# =============================================================================

closest_table <- leadRunnersNew %>%
  # First aggregate across all teams for each player
  group_by(Runner1B) %>%
  summarise(
    n_obs = sum(n_obs),
    # Weight-averaged deviation by observations
    meanChange = weighted.mean(meanChange, w = n_obs),
    absChange = abs(meanChange),
    .groups = "drop"
  ) %>%
  # Now join teams AFTER aggregation
  left_join(
    leadsnew1b_full %>%
      mutate(RunnerTeam = case_when(TopBottom == "T" ~ Away, TopBottom == "B" ~ Home)) %>%
      group_by(Runner1B) %>%
      summarise(
        Team = paste(unique(RunnerTeam), collapse = "/"),  # CHANGED: just use this directly
        .groups = "drop"
      ),
    by = "Runner1B"
  ) %>%
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
  cols_align(
    align = "center",
    columns = c(Team, absChange, n_obs)
  ) %>%
  cols_align(
    align = "left",
    columns = Runner1B
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(13),
    table.width = px(520),
    data_row.padding = px(6)
  )

print(closest_table)
gtsave(closest_table, "figures/closest_to_optimal_gt.png")

# =============================================================================
# CONSERVATIVE RUNNERS - SHOW TEAMS AS "TB/MIA"
# =============================================================================

conservative_table <- leadRunnersNew %>%
  group_by(Runner1B) %>%
  summarise(
    n_obs = sum(n_obs),
    meanChange = weighted.mean(meanChange, w = n_obs),
    .groups = "drop"
  ) %>%
  left_join(
    leadsnew1b_full %>%
      mutate(RunnerTeam = case_when(TopBottom == "T" ~ Away, TopBottom == "B" ~ Home)) %>%
      group_by(Runner1B) %>%
      summarise(
        Team = paste(unique(RunnerTeam), collapse = "/"),  # CHANGED
        .groups = "drop"
      ),
    by = "Runner1B"
  ) %>%
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
  cols_align(
    align = "center",
    columns = c(Team, meanChange, n_obs)
  ) %>%
  cols_align(
    align = "left",
    columns = Runner1B
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(13),
    table.width = px(500),
    data_row.padding = px(6)
  )

print(conservative_table)

gtsave(conservative_table, "figures/conservative_runners_gt.png")

# =============================================================================
# AGGRESSIVE RUNNERS - SHOW TEAMS AS "TB/MIA"
# =============================================================================

aggressive_table <- leadRunnersNew %>%
  group_by(Runner1B) %>%
  summarise(
    n_obs = sum(n_obs),
    meanChange = weighted.mean(meanChange, w = n_obs),
    .groups = "drop"
  ) %>%
  left_join(
    leadsnew1b_full %>%
      mutate(RunnerTeam = case_when(TopBottom == "T" ~ Away, TopBottom == "B" ~ Home)) %>%
      group_by(Runner1B) %>%
      summarise(
        Team = paste(unique(RunnerTeam), collapse = "/"),  # CHANGED
        .groups = "drop"
      ),
    by = "Runner1B"
  ) %>%
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
  cols_align(
    align = "center",
    columns = c(Team, meanChange, n_obs)
  ) %>%
  cols_align(
    align = "left",
    columns = Runner1B
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(13),
    table.width = px(500),
    data_row.padding = px(6)
  )

print(aggressive_table)
gtsave(aggressive_table, "figures/aggressive_runners_gt.png")


