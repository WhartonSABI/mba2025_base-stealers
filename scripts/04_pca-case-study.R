source("scripts/00_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

ensure_dirs(c("paper/figures", "data/processed", "internal"))

message("04_pca_case_study_figures.R starting")

bundle <- readRDS("data/processed/situational_model_bundle.rds")
leads_full <- read.csv("data/processed/leadsnew1b_full_v2.csv", stringsAsFactors = FALSE)

case_rows <- leads_full %>%
  filter(
    Runner1B == "Pete Crow-Armstrong",
    Pitcher == "Paul Skenes",
    Catcher == "Yasmani Grandal"
  )

if (nrow(case_rows) == 0L) {
  stop("Could not locate PCA/Skenes/Grandal case rows in leads dataset.")
}

case_row <- case_rows %>%
  mutate(delta = abs(PrimaryLead1B - 11.46)) %>%
  arrange(delta) %>%
  slice(1)

ctx <- list(
  threat = case_row$Threat[[1]],
  poptime = case_row$poptime[[1]],
  sprint_speed = case_row$sprint_speed[[1]],
  dis_stage = as.integer(case_row$dis_stage[[1]]),
  outs = as.integer(case_row$outs[[1]]),
  runner_id = case_row$Runner1B_ID[[1]],
  catcher_id = case_row$CatcherID[[1]]
)

lead_grid <- seq(0, 20, by = 0.1)

grid_out <- lapply(lead_grid, function(lead_ft) {
  probs <- predict_outcome_probs(
    bundle = bundle,
    lead_ft = lead_ft,
    threat = ctx$threat,
    poptime = ctx$poptime,
    sprint_speed = ctx$sprint_speed,
    dis_stage_val = ctx$dis_stage,
    outs_val = ctx$outs,
    runner_id = ctx$runner_id,
    catcher_id = ctx$catcher_id,
    use_re = TRUE
  )
  data.frame(
    PrimaryLead1B = as.numeric(lead_ft),
    P_PK = as.numeric(probs$P_PK[[1]]),
    P_SB = as.numeric(probs$P_SB[[1]]),
    P_CS = as.numeric(probs$P_CS[[1]]),
    xRuns = as.numeric(probs$xRuns[[1]])
  )
})

grid_df <- dplyr::bind_rows(grid_out)

optimum <- optimize(
  f = function(L) {
    predict_outcome_probs(
      bundle = bundle,
      lead_ft = L,
      threat = ctx$threat,
      poptime = ctx$poptime,
      sprint_speed = ctx$sprint_speed,
      dis_stage_val = ctx$dis_stage,
      outs_val = ctx$outs,
      runner_id = ctx$runner_id,
      catcher_id = ctx$catcher_id,
      use_re = TRUE
    )$xRuns
  },
  interval = c(0, 20),
  maximum = TRUE
)

xopt <- as.numeric(optimum$maximum)
yopt <- as.numeric(optimum$objective)
obs_lead <- as.numeric(case_row$PrimaryLead1B[[1]])
obs_xruns <- as.numeric(
  predict_outcome_probs(
    bundle = bundle,
    lead_ft = obs_lead,
    threat = ctx$threat,
    poptime = ctx$poptime,
    sprint_speed = ctx$sprint_speed,
    dis_stage_val = ctx$dis_stage,
    outs_val = ctx$outs,
    runner_id = ctx$runner_id,
    catcher_id = ctx$catcher_id,
    use_re = TRUE
  )$xRuns[[1]]
)

grid_long <- grid_df %>%
  pivot_longer(
    cols = c("xRuns", "P_PK", "P_SB", "P_CS"),
    names_to = "metric",
    values_to = "value"
  )

colormap <- c(
  "xRuns" = "#0072B2",
  "P_PK" = "#D55E00",
  "P_SB" = "#009E73",
  "P_CS" = "#E69F00"
)

fig_probs <- ggplot(grid_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
  geom_line(aes(linewidth = metric == "xRuns"), show.legend = TRUE) +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
  scale_color_manual(
    values = colormap,
    labels = c(
      "xRuns" = "xRuns",
      "P_PK" = "Pickoff Prob.",
      "P_SB" = "Stolen Base Prob.",
      "P_CS" = "Caught Stealing Prob."
    )
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20), expand = c(0, 0)) +
  labs(
    title = "Model Results: PCA vs. Skenes/Grandal",
    x = "Primary Lead at 1B (ft)",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", legend.title = element_blank())

ggsave(
  filename = "paper/figures/pca-probs-MMNew.png",
  plot = fig_probs,
  width = 10,
  height = 6,
  dpi = 300
)

grid_df <- grid_df %>% mutate(ev_pos = xRuns > 0)

label_x <- if ((xopt + 1.6) > 20) xopt - 1.6 else xopt + 1.6
label_hjust <- if (label_x > xopt) 0 else 1

  fig_xruns <- ggplot(grid_df, aes(PrimaryLead1B, xRuns)) +
  geom_area(data = subset(grid_df, ev_pos), aes(y = xRuns), alpha = 0.15, fill = "#0072B2") +
  geom_line(color = "#0072B2", linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = obs_lead, color = "#1f77b4", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = xopt, color = "red", linewidth = 1) +
  annotate("point", x = obs_lead, y = obs_xruns, color = "#1f77b4", size = 3) +
  annotate("point", x = xopt, y = yopt, color = "red", size = 3) +
  annotate(
    "segment",
    x = xopt,
    y = yopt,
    xend = label_x,
    yend = yopt + 0.002,
    color = "red",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = label_x,
    y = yopt + 0.003,
    label = paste0(
      "Optimal Lead = ",
      sprintf("%.2f", xopt),
      " ft\nxRuns = ",
      sprintf("%.3f", yopt)
    ),
    hjust = label_hjust,
    vjust = 0,
    color = "red",
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20), expand = c(0, 0)) +
  labs(
    title = "Expected Runs vs. Primary Lead: PCA Case Study",
    x = "Primary Lead at 1B (ft)",
    y = "xRuns per opportunity"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5)
  )

ggsave(
  filename = "paper/figures/pca-xruns-MMNew.png",
  plot = fig_xruns,
  width = 10,
  height = 6,
  dpi = 300
)

case_summary <- data.frame(
  Date = case_row$Date[[1]],
  Runner = case_row$Runner1B[[1]],
  Pitcher = case_row$Pitcher[[1]],
  Catcher = case_row$Catcher[[1]],
  observed_lead = case_row$PrimaryLead1B[[1]],
  optimal_lead = xopt,
  observed_minus_optimal = case_row$PrimaryLead1B[[1]] - xopt,
  optimal_xRuns = yopt,
  outs = ctx$outs,
  dis_stage = ctx$dis_stage
)

write.csv(case_summary, "data/processed/pca_case_study_summary.csv", row.names = FALSE)

sink("internal/04_pca_case_summary.txt")
print(case_summary)
sink()

message("04_pca_case_study_figures.R complete")
