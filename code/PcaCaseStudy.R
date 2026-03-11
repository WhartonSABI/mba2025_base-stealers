# ============================================
# PCA CASE STUDY - Player-Specific Mixed Model (CUT AT 20 FT)
# ============================================

# Extract PCA's player info
pca_data <- leadsnew1b %>%
  filter(Runner1B == "Pete Crow-Armstrong") %>%
  slice(1)

pca_speed <- pca_data$sprint_speed
pca_id <- pca_data$Runner1B_ID
ps_threat <- leadsnew1b$Threat[leadsnew1b$Pitcher == "Paul Skenes"][1]
yg_poptime <- leadsnew1b$poptime[leadsnew1b$Catcher == "Yasmani Grandal"][1]

# Assume dis_stage = 0 for this case study (no prior pickoffs)
dis_stage_val <- 0

# ============================================
# PLOT 1: All Probability Curves (CUT AT 20 FT)
# ============================================

doPlot_Player <- function(thr, pop, spd, runner_id, dis_stage_val = 0) {
  lead_vec <- seq(0, 20, by = 0.1)  # CHANGED: 30 → 20
  
  results <- sapply(lead_vec, function(x) {
    tmp <- get_prob_mixed_player(x, thr, pop, spd, dis_stage_val, runner_id, TRUE)
    c(xRuns = tmp$xRuns, P_PK = tmp$P_PK, P_SB = tmp$P_SB, P_CS = tmp$P_CS)
  })
  
  df <- data.frame(
    PrimaryLead1B = lead_vec,
    xRuns = results["xRuns", ],
    P_PK = results["P_PK", ],
    P_SB = results["P_SB", ],
    P_CS = results["P_CS", ]
  )
  
  df_long <- pivot_longer(df, cols = c(xRuns, P_PK, P_SB, P_CS),
                          names_to = "metric", values_to = "value")
  
  # Custom colors matching original
  colormap <- c(
    "xRuns" = "#0072B2",
    "P_PK" = "#D55E00",
    "P_SB" = "#009E73",
    "P_CS" = "#E69F00"
  )
  
  ggplot(df_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
    geom_line(aes(size = (metric == "xRuns")), show.legend = TRUE) +
    scale_size_manual(values = c("TRUE" = 2.3, "FALSE" = 1.1), guide = "none") +
    scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20), expand = c(0, 0)) +  # CHANGED: 30 → 20
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(
      values = colormap,
      labels = c("xRuns" = "xRuns", "P_PK" = "Pickoff Prob.", 
                 "P_SB" = "Stolen Base Prob.", "P_CS" = "Caught Stealing Prob.")
    ) +
    labs(
      x = "Primary Lead at 1B (ft)", 
      y = "", 
      title = "Model Results: Given Situation (Player-Specific)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", legend.title = element_blank())
}

# ============================================
# PLOT 2: xRuns Only (CUT AT 20 FT)
# ============================================

doPlot_Player_xRuns <- function(thr, pop, spd, runner_id, dis_stage_val = 0) {
  lead_vec <- seq(0, 20, by = 0.1)  # CHANGED: 30 → 20
  xr <- sapply(lead_vec, function(x) {
    get_prob_mixed_player(x, thr, pop, spd, dis_stage_val, runner_id, TRUE)$xRuns
  })
  df <- tibble(PrimaryLead1B = lead_vec, xRuns = as.numeric(xr))
  
  # Get optimal lead
  opt <- get_optimal_lead_mixed_player(thr, pop, spd, dis_stage_val, runner_id, TRUE)
  xopt <- opt$optimal_lead
  yopt <- approx(df$PrimaryLead1B, df$xRuns, xout = xopt)$y
  
  df <- df %>% mutate(ev_pos = xRuns > 0)
  
  # Label positioning
  pad <- 1.6
  xmax <- 20  # CHANGED: 30 → 20
  lab_x <- if (xopt + pad > xmax) xopt - pad else xopt + pad
  lab_hjust <- if (lab_x > xopt) 0 else 1
  
  ggplot(df, aes(PrimaryLead1B, xRuns)) +
    geom_area(data = df %>% filter(ev_pos), aes(y = xRuns), alpha = 0.15, fill = "#0072B2") +
    geom_line(color = "#0072B2", linewidth = 1.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = xopt, color = "red", linewidth = 1) +
    geom_point(aes(x = xopt, y = yopt), color = "red", size = 3) +
    geom_segment(aes(x = xopt, y = yopt, xend = lab_x, yend = yopt + 0.001),
                 color = "red", linewidth = 0.6) +
    annotate("text",
             x = lab_x, y = yopt + 0.002,
             label = paste0("Optimal Lead = ", sprintf('%.2f', xopt), " ft\nxRuns = ", sprintf('%.3f', yopt)),
             hjust = lab_hjust, vjust = 0, color = "red", fontface = "bold", size = 4) +
    scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, xmax), expand = c(0, 0)) +  # CHANGED: 30 → 20
    labs(title = "Expected Runs vs. Primary Lead: Given Situation (Player-Specific)",
         x = "Primary Lead at 1B (ft)", y = "xRuns per opportunity") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5)
    )
}

# ============================================
# GENERATE THE TWO CASE STUDY PLOTS
# ============================================

# Plot 1: All curves (0-20 ft)
pca_plot1 <- doPlot_Player(ps_threat, yg_poptime, pca_speed, pca_id, dis_stage_val = 0)
print(pca_plot1)
ggsave("figures/pca-probs-MM.png", pca_plot1, width = 10, height = 6, dpi = 300)

# Plot 2: xRuns with optimal (0-20 ft)
pca_plot2 <- doPlot_Player_xRuns(ps_threat, yg_poptime, pca_speed, pca_id, dis_stage_val = 0)
print(pca_plot2)
ggsave("figures/pca-xruns-MM.png", pca_plot2, width = 10, height = 6, dpi = 300)

# Get the actual optimal lead value
pca_optimal <- get_optimal_lead_mixed_player(
  ps_threat, yg_poptime, pca_speed, dis_stage_val = 0, pca_id, TRUE
)

print(pca_optimal)