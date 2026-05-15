source("scripts/00_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
})

ensure_dirs(c("data/processed", "internal"))

message("01_fit_models_situational.R starting")

# ----------------------------
# Load raw data
# ----------------------------
leads_raw <- read.csv("data/raw/lead-distances.csv", stringsAsFactors = FALSE)
nbp_raw <- read.csv("data/raw/net-bases-prevented.csv", stringsAsFactors = FALSE)
ip_raw <- read.csv("data/raw/innings-pitched.csv", stringsAsFactors = FALSE)
ss_raw <- read.csv("data/raw/sprint-speed.csv", stringsAsFactors = FALSE)
pt_raw <- read.csv("data/raw/pop-time.csv", stringsAsFactors = FALSE)

# ----------------------------
# Build lookup tables
# ----------------------------
nbp_tbl <- nbp_raw %>%
  transmute(
    player_id = player_id,
    NBP = net_attr_plus + net_attr_minus
  )

pitcher_tbl <- ip_raw %>%
  transmute(
    PitcherID = player_id,
    IP = round(as.numeric(p_formatted_ip), 0)
  ) %>%
  left_join(nbp_tbl, by = c("PitcherID" = "player_id")) %>%
  mutate(Threat = 100 * NBP / IP) %>%
  filter(IP >= 10) %>%
  select(PitcherID, Threat)

catcher_tbl <- pt_raw %>%
  transmute(
    CatcherID = entity_id,
    poptime = pop_2b_sba
  )

runner_tbl <- ss_raw %>%
  transmute(
    Runner1B_ID = player_id,
    sprint_speed = sprint_speed
  )

# ----------------------------
# Merge + filter base state
# ----------------------------
leads <- leads_raw %>%
  left_join(pitcher_tbl, by = "PitcherID") %>%
  left_join(catcher_tbl, by = "CatcherID") %>%
  left_join(runner_tbl, by = "Runner1B_ID") %>%
  mutate(BaseState = if_else(BaseState == "--1", "1--", BaseState)) %>%
  filter(BaseState == "1--") %>%
  filter(!is.na(Threat), !is.na(poptime), !is.na(sprint_speed), !is.na(PrimaryLead1B))

message("Rows after filters: ", nrow(leads))

# ----------------------------
# Disengagement stage feature
# ----------------------------
leads <- leads %>%
  group_by(Date, Home, Away) %>%
  mutate(
    row_in_game = row_number(),
    start_pa = (Play == "Pitch" & Balls == 0 & Strikes == 0),
    pa_id = cumsum(start_pa),
    pa_id = if_else(pa_id == 0L, 1L, pa_id)
  ) %>%
  arrange(row_in_game, .by_group = TRUE) %>%
  group_by(Date, Home, Away, pa_id) %>%
  mutate(
    is_diseng = as.integer(Play == "Pickoff"),
    dis_used_before = dplyr::lag(cumsum(is_diseng), default = 0L),
    dis_stage = factor(pmin(2L, dis_used_before), levels = c(0, 1, 2))
  ) %>%
  ungroup() %>%
  select(-row_in_game, -start_pa)

# ----------------------------
# Model-ready columns
# ----------------------------
leads <- leads %>%
  mutate(
    lead_scaled = as.numeric(scale(PrimaryLead1B)),
    threat_scaled = as.numeric(scale(Threat)),
    poptime_scaled = as.numeric(scale(poptime)),
    sprint_scaled = as.numeric(scale(sprint_speed)),
    outs_f = factor(as.integer(outs), levels = c(0, 1, 2)),
    Runner1B_ID = factor(Runner1B_ID),
    CatcherID = factor(CatcherID),
    dis_stage = factor(dis_stage, levels = c(0, 1, 2)),
    PO_y = as.integer(Play == "Pickoff")
  )

scaler <- list(
  lead_mean = mean(leads$PrimaryLead1B, na.rm = TRUE),
  lead_sd = sd(leads$PrimaryLead1B, na.rm = TRUE),
  threat_mean = mean(leads$Threat, na.rm = TRUE),
  threat_sd = sd(leads$Threat, na.rm = TRUE),
  pop_mean = mean(leads$poptime, na.rm = TRUE),
  pop_sd = sd(leads$poptime, na.rm = TRUE),
  sprint_mean = mean(leads$sprint_speed, na.rm = TRUE),
  sprint_sd = sd(leads$sprint_speed, na.rm = TRUE)
)

# ----------------------------
# Situational RE24-based weights
# ----------------------------
sit_weights <- data.frame(
  outs = c(0L, 1L, 2L),
  w_SB = c(1.068 - 0.831, 0.644 - 0.489, 0.305 - 0.214),
  w_CS = c(0.243 - 0.831, 0.095 - 0.489, 0.000 - 0.214),
  w_PK = c(0.243 - 0.831, 0.095 - 0.489, 0.000 - 0.214)
)

# ----------------------------
# Fit models
# ----------------------------
message("Fitting stage models...")

m_PO <- glm(
  PO_y ~ lead_scaled + threat_scaled + dis_stage + outs_f,
  family = binomial(),
  data = leads
)

m_PK <- glmer(
  PK1 ~ lead_scaled + threat_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = subset(leads, Play == "Pickoff"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

leads_pitch <- subset(leads, Play == "Pitch")
leads_pitch$ATT_y <- as.integer(leads_pitch$SB1 == 1L | leads_pitch$CS1 == 1L)

m_ATT <- glmer(
  ATT_y ~ threat_scaled + poptime_scaled + sprint_scaled + dis_stage + outs_f +
    (1 | Runner1B_ID) + (1 | CatcherID),
  family = binomial(),
  data = leads_pitch,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

leads_attempt <- subset(leads, SB1 == 1L | CS1 == 1L)
leads_attempt$SB_y <- as.integer(leads_attempt$SB1 == 1L)

m_SB <- glmer(
  SB_y ~ lead_scaled + threat_scaled + poptime_scaled + sprint_scaled + dis_stage + outs_f +
    (1 | Runner1B_ID) + (1 | CatcherID),
  family = binomial(),
  data = leads_attempt,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

bundle <- list(
  models = list(
    m_PO = m_PO,
    m_PK = m_PK,
    m_ATT = m_ATT,
    m_SB = m_SB
  ),
  scaler = scaler,
  sit_weights = sit_weights
)

saveRDS(bundle, "data/processed/situational_model_bundle.rds")
write.csv(leads, "data/processed/leads_1b_situational.csv", row.names = FALSE)

sink("internal/01_model_summaries.txt")
cat("Model: m_PO\n")
print(summary(m_PO))
cat("\nModel: m_PK\n")
print(summary(m_PK))
cat("\nModel: m_ATT\n")
print(summary(m_ATT))
cat("\nModel: m_SB\n")
print(summary(m_SB))
sink()

message("01_fit_models_situational.R complete")
