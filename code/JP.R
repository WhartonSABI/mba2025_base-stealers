# libraries
library(tidyverse)

# =========================
# 1. LOAD DATA FROM OTHER SCRIPTS
# =========================

# Load data processed by zach.R
leadsnew1b <- read_csv("data/processed/full_leads_1b_final.csv")

# Load pitcher data for threat analysis
ipdata <- read_csv("data/raw/innings-pitched.csv") %>%
  left_join(read_csv("data/raw/net-bases-prevented.csv"), by = "player_id") %>%
  mutate(
    netBP = net_attr_plus + net_attr_minus,
    IP = round(as.numeric(p_formatted_ip), 0),
    Threat = 100 * netBP / IP
  ) %>%
  filter(IP > 10)

#  (Actual − Optimal) ---
leadsnew1b <- leadsnew1b %>%
  mutate(
    leadError_old = PrimaryLead1B - optimalLead1B,
    leadError_new = PrimaryLead1B - optimalLead1BNew
  )
leadsnew1b <- leadsnew1b %>%
  mutate(leadChangeNew = leadError_new)

# Create player-level summary data for plotting
newplayerleads1b <- leadsnew1b %>%
  filter(!is.na(PrimaryLead1B)) %>%
  group_by(Runner1B_ID) %>%
  mutate(playeravg1Blead = mean(PrimaryLead1B), leadChange) %>%
  filter(n() >= 15) %>% 
  slice_head(n = 1) %>% 
  ungroup()

# Sample of 100 leads on pitches
leads1bsample <- leadsnew1b[sample(nrow(leadsnew1b), 100),] %>%
  rowwise() %>%
  mutate(
    actualxRunsNew = get_prob_new(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optimalNew = get_optimal_lead_new(Threat, poptime, sprint_speed)
  ) %>%
  mutate(
    optimalLead1BNew = optimalNew$PrimaryLead1B,
    optimalxRunsNew = optimalNew$xRuns
  ) %>%
  ungroup() %>%
  mutate(recommendationNew = if_else(optimalxRunsNew > 0, "Steal", "Stay"),
         leadChangeNew = PrimaryLead1B- optimalLead1BNew) %>%
  select(-optimalNew)

#Functions to create plots of probability curves. Old model and new model respectively
doPlot <- function(thr, pop, spd) {
  lead_vec <- seq(0, 30, by = 0.1)
  
  results <- sapply(lead_vec, function(x) {
    tmp <- get_prob(x, thr, pop, spd)
    c(xRuns = tmp$xRuns, P_PK = tmp$P_PK, P_SB = tmp$P_SB, P_CS = tmp$P_CS)
  })
  
  df <- data.frame(
    PrimaryLead1B = lead_vec,
    xRuns = results["xRuns", ],
    P_PK = results["P_PK", ],
    P_SB = results["P_SB", ],
    P_CS = results["P_CS", ]
  )
  
  library(tidyr)
  df_long <- pivot_longer(df, cols = c(xRuns, P_PK, P_SB, P_CS),
                          names_to = "metric", values_to = "value")
  
  # Custom colors for each metric
  colormap <- c(
    "xRuns" = "#0072B2",
    "P_PK" = "#D55E00",
    "P_SB" = "#009E73",
    "P_CS" = "#E69F00"
  )
  
  ggplot(df_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
    geom_line(aes(size = (metric == "xRuns")), show.legend = TRUE) +
    scale_size_manual(values = c("TRUE" = 2.3, "FALSE" = 1.1), guide = "none") +  # Thicc line for xRuns
    scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(
      values = colormap,
      labels = c("xRuns" = "xRuns", "P_PK" = "Pickoff Prob.", "P_SB" = "Stolen Base Prob.", "P_CS" = "Caught Stealing Prob.")
    ) +
    labs(
      x = "Primary Lead at 1B (ft)", y = "", title = "Model Results: Given Situation"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", legend.title = element_blank())
}

doPlotNew <- function(thr, pop, spd) {
  lead_vec <- seq(0, 30, by = 0.1)
  
  results <- sapply(lead_vec, function(x) {
    tmp <- get_prob_new(x, thr, pop, spd)
    c(xRuns = tmp$xRuns, P_PK = tmp$P_PK, P_SB = tmp$P_SB, P_CS = tmp$P_CS)
  })
  
  df <- data.frame(
    PrimaryLead1B = lead_vec,
    xRuns = results["xRuns", ],
    P_PK = results["P_PK", ],
    P_SB = results["P_SB", ],
    P_CS = results["P_CS", ]
  )
  
  library(tidyr)
  df_long <- pivot_longer(df, cols = c(xRuns, P_PK, P_SB, P_CS),
                          names_to = "metric", values_to = "value")
  
  # Custom colors for each metric
  colormap <- c(
    "xRuns" = "#0072B2",
    "P_PK" = "#D55E00",
    "P_SB" = "#009E73",
    "P_CS" = "#E69F00"
  )
  
  ggplot(df_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
    geom_line(aes(size = (metric == "xRuns")), show.legend = TRUE) +
    scale_size_manual(values = c("TRUE" = 2.3, "FALSE" = 1.1), guide = "none") +  # Thicc line for xRuns
    scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(
      values = colormap,
      labels = c("xRuns" = "xRuns", "P_PK" = "Pickoff Prob.", "P_SB" = "Stolen Base Prob.", "P_CS" = "Caught Stealing Prob.")
    ) +
    labs(
      x = "Primary Lead at 1B (ft)", y = "", title = "Model Results: Given Situation"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", legend.title = element_blank())
}


#Plot for the optimal lead j
doPlotNew_xRuns <- function(thr, pop, spd) {
  lead_vec <- seq(0, 30, by = 0.1)
  xr <- sapply(lead_vec, function(x) get_prob_new(x, thr, pop, spd)$xRuns)
  df <- tibble(PrimaryLead1B = lead_vec, xRuns = as.numeric(xr))
  
  opt  <- get_optimal_lead_new(thr, pop, spd)
  xopt <- opt$PrimaryLead1B
  yopt <- approx(df$PrimaryLead1B, df$xRuns, xout = xopt)$y
  
  df <- df %>% mutate(ev_pos = xRuns > 0)
  
  pad <- 1.6
  xmax <- 30
  lab_x <- if (xopt + pad > xmax) xopt - pad else xopt + pad
  lab_hjust <- if (lab_x > xopt) 0 else 1
  
  ggplot(df, aes(PrimaryLead1B, xRuns)) +
    geom_area(data = df %>% filter(ev_pos), aes(y = xRuns), alpha = 0.15, fill = "#0072B2") +
    geom_line(color = "#0072B2", linewidth = 1.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = xopt, color = "red", linewidth = 1) +
    geom_point(aes(x = xopt, y = yopt), color = "red", size = 3) +
    # pointer line
    geom_segment(aes(x = xopt, y = yopt, xend = lab_x, yend = yopt + 0.01),
                 color = "red", linewidth = 0.6) +
    annotate("text",
             x = lab_x, y = yopt + 0.012,
             label = paste0("Optimal Lead = ", sprintf('%.2f', xopt), " ft\nxRuns = ", sprintf('%.3f', yopt)),
             hjust = lab_hjust, vjust = 0, color = "red", fontface = "bold", size = 4) +
    scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, xmax), expand = c(0, 0)) +
    labs(title = "Expected Runs vs. Primary Lead: Given Situation",
         x = "Primary Lead at 1B (ft)", y = "xRuns per opportunity") +
    coord_cartesian(clip = "off") +                         # <- allow label to extend outside panel
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5)  # extra right margin
    )
}

##Case study
#PCA on first
pca_speed <- leadsnew1b$sprint_speed[leadsnew1b$Runner1B == "Pete Crow-Armstrong"][1]

ps_threat <- leadsnew1b$Threat[leadsnew1b$Pitcher == "Paul Skenes"][1]

yg_poptime<-leadsnew1b$poptime[leadsnew1b$Catcher == "Yasmani Grandal"][1]

doPlotNew(ps_threat, yg_poptime,pca_speed)
doPlotNew_xRuns(ps_threat, yg_poptime,pca_speed)

# =========================
# 2. CREATE PLOTS
# =========================

# Create visualization plots
plot1 <- ggplot(leadsnew1b) + #originally leads1b
  geom_histogram(aes(x = PrimaryLead1B), 
                 binwidth = 1, 
                 fill = "lightgray", 
                 color = "black") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B") +
  theme_minimal(base_size = 14)

plot2 <- ggplot(subset(leadsnew1b, SB1 == 1 | CS1 == 1)) + #originally leads1b
  geom_histogram(aes(x = PrimaryLead1B, fill = factor(CS1)), 
                 binwidth = 1, 
                 color = "black", 
                 position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("forestgreen", "red3"), 
                    labels = c("Safe", "Out"), 
                    name = "Outcome at 2nd Base") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B on Stolen Base Attempts") +
  theme_minimal(base_size = 14) +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

plot3 <- ggplot(newplayerleads1b) +
  geom_histogram(aes(x = playeravg1Blead), 
                 binwidth = 0.2, 
                 fill = "orange", 
                 color = "black") + 
  labs(x = "Average Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B by Player") +
  theme_minimal(base_size = 15)

plot4 <- ggplot(ipdata) +
  geom_histogram(aes(x = Threat), 
                 binwidth = 1, 
                 fill = "forestgreen", 
                 color = "black") +  
  labs(x = "Pitcher threat", 
       y = "Count", 
       title = "") +
  theme_minimal(base_size = 15)

plot5 <- ggplot(newplayerleads1b, aes(x = sprint_speed, y = playeravg1Blead)) +
  geom_point(color = "firebrick", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = "Sprint Speed (ft/s)",
    y = "Average Primary Lead from 1B (ft)",
    title = "Primary Lead vs. Sprint Speed",
  ) +
  theme_minimal(base_size = 14)

plot6 <- ggplot(leads1bsample, aes(x = actualxRuns, y = optimalxRuns, color = recommendation)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "gray", lty = "dashed", size = 1.2) +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("Stay" = "red", "Steal" = "forestgreen")) +
  labs(
    x = "xRuns (Actual)",
    y = "xRuns (Optimal)",
    color = "Recommendation",
    title = "Actual vs. Optimal  - Sample of 100"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

plot6new <- ggplot(leads1bsample, aes(x = actualxRunsNew, y = optimalxRunsNew, color = recommendation)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "gray", lty = "dashed", size = 1.2) +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("Stay" = "red", "Steal" = "forestgreen")) +
  labs(
    x = "xRuns (Actual)",
    y = "xRuns (Optimal)",
    color = "Recommendation",
    title = "Actual vs. Optimal  - Sample of 100"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

mean_leadchange <- 1.75 #only to 2 decimal places, take mean of (leadsnew1b %>% filter(SB1 == 1))$leadChange for true
mean_leadchange_new <- -0.588



plot7 <- ggplot(leadsnew1b %>% filter(SB1 == 1), aes(x = leadChange)) +
  geom_histogram(
    fill = "#40b8c4", 
    color = "black",      
    bins = 30               
  ) +
  geom_vline(
    xintercept = mean_leadchange,
    color = "red",
    size = 1.2
  ) +
  annotate(
    "text",
    x = mean_leadchange - 1.5,
    y = 265,
    label = paste0("Mean: ", round(mean_leadchange, 2)),
    color = "red",
    vjust = -0.5,
    fontface = "bold",
    size = 5
  ) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Error: Optimal Lead - Actual Lead (ft)",
    y = "Count",
    title = "Lead Error on SB attempts"
  )


plot7new <- ggplot(leadsnew1b %>% filter(SB1 == 1), aes(x = leadChangeNew)) +
  geom_histogram(
    fill = "#40b8c4", 
    color = "black",      
    bins = 30               
  ) +
  geom_vline(
    xintercept = mean_leadchange_new,
    color = "red",
    size = 1.2
  ) +
  annotate(
    "text",
    x = mean_leadchange_new - 3.5,
    y = 285,
    label = paste0("Mean: ", round(mean_leadchange_new, 2)),
    color = "red",
    vjust = -0.5,
    fontface = "bold",
    size = 5
  ) +
  
  theme_minimal(base_size = 15) +
  labs(
    x = "Error: Optimal Lead - Actual Lead (ft)",
    y = "Count",
    title = "Lead Error on SB attempts"
  )

df_long <- leadsnew1b %>%
  filter(SB1 + CS1 >= 1) %>% 
  pivot_longer(
    cols = c(PrimaryLead1B, optimalLead1B),
    names_to = "type", values_to = "lead"
  ) %>%
  mutate(type = recode(type,
                       PrimaryLead1B = "Actual Lead (ft)",
                       optimalLead1B = "Optimal Lead (ft)"
  ))

library(tidyverse)
library(ggplot2)

# --- Medians ---# --- MEAN calculations ---
meanAll <- mean(leadsnew1b$leadChangeNew, na.rm = TRUE)

mean_leadchange_SB <- leadsnew1b %>%
  filter(SB1 == 1) %>%
  summarize(m = mean(leadChangeNew, na.rm = TRUE)) %>%
  pull(m)


# --- SB attempts only (mean) ---
plot7new <- ggplot(leadsnew1b %>% filter(SB1 == 1),
                   aes(x = leadChangeNew)) +
  geom_histogram(
    fill = "#40b8c4",
    color = "black",
    bins = 30,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = mean_leadchange_SB,
    color = "red",
    size = 1.2
  ) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Error:  Actual Lead - Optimal Lead (ft)",
    y = "Count",
    title = "Lead Error on SB Attempts (Mean)"
  )

# --- All situations (mean) ---
plotAllSituations <- ggplot(leadsnew1b, aes(x = leadChangeNew)) +
  geom_histogram(
    fill = "#40b8c4",
    color = "black",
    bins = 30,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = meanAll,
    color = "red",
    size = 1.2
  ) +
  annotate(
    "label",
    x = meanAll, y = Inf,
    label = paste0("Mean (All): ", sprintf("%.2f", meanAll)),
    vjust = 1.2, label.size = 0, color = "red", size = 4.5
  ) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Error:  Actual Lead - Optimal Lead (ft)",
    y = "Count",
    title = "Lead Error Across All Situations (Mean)"
  )

# Print them
plot7new
plotAllSituations

# --- MEAN calculations ---
meanAllNew <- mean(leadsnew1b_full$leadDev_player, na.rm = TRUE)

mean_leadchange_SBNew <- leadsnew1b_full %>%
  filter(SB1 == 1 | CS1 == 1) %>%
  summarize(m = mean(leadDev_player, na.rm = TRUE)) %>%
  pull(m)

# --- SB attempts only (mean) ---
plot7newPlayer <- ggplot(leadsnew1b_full %>% filter(SB1 == 1 | CS1 == 1),
                         aes(x = leadDev_player)) +
  geom_histogram(
    fill = "#40b8c4",
    color = "black",
    bins = 30,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = mean_leadchange_SBNew,
    color = "red",
    size = 1.2
  ) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Error:  Actual Lead - Optimal Lead (ft)",
    y = "Count",
    title = "Lead Error on SB Attempts (Mean)"
  )

# --- All situations (mean) ---
plotAllSituationsNew <- ggplot(leadsnew1b_full, aes(x = leadDev_player)) +
  geom_histogram(
    fill = "#40b8c4",
    color = "black",
    bins = 30,
    na.rm = TRUE
  ) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Error:  Actual Lead - Optimal Lead (ft)",
    y = "Count",
    title = "Lead Error Across All Situations (Mean)"
  )


abs_dev_data <- leadsnew1b_full %>%
  mutate(abs_leadDev = abs(leadDev_player))

# Get mean for annotation
mean_abs_dev <- mean(abs_dev_data$abs_leadDev, na.rm = TRUE)

# Create plot
p_abs_deviation <- ggplot(abs_dev_data, aes(x = abs_leadDev)) +
  geom_histogram(bins = 50, fill = "#40b8c4", color = "black", alpha = 0.8) +
  geom_vline(xintercept = mean_abs_dev, 
             color = "red", 
             linetype = "dashed", 
             size = 1.2) +
  annotate("text",
           x = mean_abs_dev + 0.5,
           y = Inf,
           label = paste0("Mean = ", round(mean_abs_dev, 2), " ft"),
           vjust = 2,
           hjust = 0,
           color = "red",
           fontface = "bold",
           size = 5) +
  scale_x_continuous(
    breaks = seq(0, 15, by = 2.5),
    limits = c(0, 15),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 20000, by = 5000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Absolute Lead Error Distribution",
    subtitle = "Distance from optimal lead across all situations",
    x = "Absolute Deviation from Optimal Lead (ft)",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )



print(p_abs_deviation)




# Separate the data
conservative_data <- leadsnew1b_full %>%
  filter(leadDev_player < 0) %>%
  mutate(abs_dev = abs(leadDev_player))

aggressive_data <- leadsnew1b_full %>%
  filter(leadDev_player > 0)

# Calculate means
mean_conservative <- mean(conservative_data$abs_dev, na.rm = TRUE)
mean_aggressive <- mean(aggressive_data$leadDev_player, na.rm = TRUE)

# LEFT: Conservative (show as positive distance "too short by X ft")
p_left <- ggplot(conservative_data, aes(x = abs_dev)) +
  geom_histogram(bins = 40, fill = "#4169E1", color = "black", alpha = 0.8) +
  geom_vline(xintercept = mean_conservative, 
             color = "red", linetype = "dashed", size = 1.2) +
  annotate("text",
           x = mean_conservative + 0.3, y = Inf,
           label = paste0("Mean = ", round(mean_conservative, 2), " ft"),
           vjust = 2, hjust = 0,
           color = "red", fontface = "bold", size = 4.5) +
  scale_x_continuous(
    breaks = seq(0, 15, by = 2.5),
    limits = c(0, 15),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 12000, by = 3000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Conservative Deviations",
    subtitle = "Leads SHORTER than optimal",
    x = "Distance Too Short (ft)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#4169E1"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# RIGHT: Aggressive (show as positive distance "too long by X ft")
p_right <- ggplot(aggressive_data, aes(x = leadDev_player)) +
  geom_histogram(bins = 40, fill = "#DC143C", color = "black", alpha = 0.8) +
  geom_vline(xintercept = mean_aggressive, 
             color = "red", linetype = "dashed", size = 1.2) +
  annotate("text",
           x = mean_aggressive + 0.3, y = Inf,
           label = paste0("Mean = ", round(mean_aggressive, 2), " ft"),
           vjust = 2, hjust = 0,
           color = "red", fontface = "bold", size = 4.5) +
  scale_x_continuous(
    breaks = seq(0, 15, by = 2.5),
    limits = c(0, 15),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 12000, by = 3000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Aggressive Deviations",
    subtitle = "Leads LONGER than optimal",
    x = "Distance Too Long (ft)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#DC143C"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
# Print them
print(plot7newPlayer)
print(plotAllSituationsNew)

# Save them
ggsave("figures/lead_error_sb_attempts_player.png", plot7newPlayer, width = 10, height = 6, dpi = 300)
ggsave("figures/lead_error_all_situations_player.png", plotAllSituationsNew, width = 10, height = 6, dpi = 300)
# Save them
ggsave("figures/lead_error_sb_attempts_player.png", plot7new, width = 10, height = 6, dpi = 300)
ggsave("figures/lead_error_all_situations_player.png", plotAllSituations, width = 10, height = 6, dpi = 300)



N <- nrow(leadsnew1b)
plot8 <- ggplot(df_long, aes(x = lead, color = type, fill = type)) +
  geom_density(
    aes(y = after_stat(density * N)),
    size = 1.5,
    alpha = 0.25,
    adjust = 1.4
  ) +
  labs(
    x = "Lead Distance (ft)",
    color = NULL,
    fill = NULL,
    title = "Frequency of leads: Stolen base attempts only"
  ) +
  scale_color_manual(
    values = c("Optimal Lead (ft)" = "#d95f02", "Actual Lead (ft)" = "#1b9e77"),
    breaks = c("Optimal Lead (ft)", "Actual Lead (ft)")
  ) +
  scale_fill_manual(
    values = c("Optimal Lead (ft)" = "#d95f02", "Actual Lead (ft)" = "#1b9e77"),
    breaks = c("Optimal Lead (ft)", "Actual Lead (ft)")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

df_long_new <- leadsnew1b %>%
  filter(SB1 + CS1 >= 1) %>% 
  pivot_longer(
    cols = c(PrimaryLead1B, optimalLead1B, optimalLead1BNew),
    names_to = "type", values_to = "lead"
  ) %>%
  mutate(type = recode(type,
                       PrimaryLead1B = "Actual Lead (ft)",
                       optimalLead1B = "Optimal Lead (ft) [OLD MODEL: RUNNER GOES]",
                       optimalLead1BNew = "Optimal Lead (ft) [NEW MODEL: NO INTENT]"
  ))

plot8new <- ggplot(df_long_new, aes(x = lead, color = type, fill = type)) +
  geom_density(
    aes(y = after_stat(density * N)),
    size = 1.5,
    alpha = 0.25,
    adjust = 1.4
  ) +
  labs(
    x = "Lead Distance (ft)",
    color = NULL,
    fill = NULL,
    title = "Frequency of leads: Stolen base attempts only"
  ) +
  scale_color_manual(
    values = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]" = "#d95f02", "Actual Lead (ft)" = "#1b9e77", "Optimal Lead (ft) [NEW MODEL: NO INTENT]" = "#ca88f3"),
    breaks = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]", "Actual Lead (ft)", "Optimal Lead (ft) [NEW MODEL: NO INTENT]")
  ) +
  scale_fill_manual(
    values = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]" = "#d95f02", "Actual Lead (ft)" = "#1b9e77", "Optimal Lead (ft) [NEW MODEL: NO INTENT]" = "#ca88f3"),
    breaks = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]", "Actual Lead (ft)", "Optimal Lead (ft) [NEW MODEL: NO INTENT]")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plotModelComparison <- ggplot(leadsnew1bSAMPLE, aes(x = optimalLead1B, y = optimalLead1BNew)) +
  geom_point()
plotRecComparison <- with(leadsnew1b, table(recommendation, recommendationNew)) #not really a plot

### examples + plots
doPlot(4.03, 2.09, 30) # the one from presentation
doPlotNew(0, 1.9, 30) # same one, with the new multinomial model
plot1
plot2
plot3
plot4
plot5
plot6
plot6new
plot7
plot7new
plot8
plot8new
plotRecComparison
plotModelComparison
ggplot( leadsnew1b, aes(x = PrimaryLead1B, y = p_ATT, color = Threat)) +
  geom_line() + facet_wrap(~cut(sprint_speed, breaks=3))





# Save plots to results folder
ggsave("results/lead_histogram.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("results/steal_attempts_by_lead.png", plot2, width = 10, height = 6, dpi = 300)
ggsave("results/player_average_leads.png", plot3, width = 10, height = 6, dpi = 300)
ggsave("results/pitcher_threat_distribution.png", plot4, width = 10, height = 6, dpi = 300)
ggsave("results/lead_vs_sprint_speed.png", plot5, width = 10, height = 6, dpi = 300)
ggsave("results/actual_vs_optimal_xruns.png", plot6, width = 10, height = 6, dpi = 300)
ggsave("results/steal_lead_error_histogram.png", plot7, width = 10, height = 6, dpi = 300)
#NOT ALL OF THESE ARE HERE: THE 'NEW' VERSIONS AS WELL AS 8 NEED TO BE ADDED
ggsave("results/actual_vs_optimal_xruns_new.png", plot6new, width = 10, height = 6, dpi = 300)
ggsave("results/steal_lead_error_histogram_new.png", plot7new, width = 10, height = 6, dpi = 300)
ggsave("results/density_frequncy_of_leads_all3.png", plot8new, width = 10, height = 6, dpi = 300)
ggsave("results/model_comparison.png", plotModelComparison, width = 10, height = 6, dpi = 300)

# All plots saved via ggsave above




