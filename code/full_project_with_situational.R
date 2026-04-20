library(tidyverse)
library(baseballr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(splines)
library(lme4)

# =============================================================================
# DATA LOADING
# =============================================================================

# leadsnew: play-level lead distance data. "play" means pitch / pickoff attempt
leadsnew <- read.csv("data/raw/lead-distances.csv")

# Net Bases Prevented data from Savant (player-level)
nbpdata <- read.csv("data/raw/net-bases-prevented.csv")

# Innings Pitched data from Savant (player-level)
ipdata <- read.csv("data/raw/innings-pitched.csv")

# Sprint Speed data from Savant (player-level)
ssdata <- read.csv("data/raw/sprint-speed.csv")

# Catcher pop time data from Savant (player-level)
ptdata <- read.csv("data/raw/pop-time.csv")


# =============================================================================
# DATA PREP: MERGE BASEBALL SAVANT COVARIATES
# =============================================================================

nbpdata <- nbpdata %>%
  reframe(player_id = player_id, player_name = player_name,
          NBP = net_attr_plus + net_attr_minus)

ipdata <- ipdata %>%
  left_join(x = ipdata, y = nbpdata, by = c("player_id" = "player_id"))

ipdata <- ipdata %>%
  reframe(PitcherID = player_id, Pitcher = player_name,
          NBP = NBP, IP = round(p_formatted_ip, 1)) %>%
  mutate(IP = round(IP, 0)) %>%
  mutate(Threat = 100 * NBP / IP) %>%
  filter(IP >= 10)

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))

ptdata <- ptdata %>%
  reframe(Catcher = entity_name, CatcherID = entity_id,
          poptime = pop_2b_sba, popcount = pop_2b_sba_count)

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))

ssdata <- ssdata %>%
  reframe(Runner1B_ID = player_id, sprint_speed = sprint_speed)

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))

leadsnew <- leadsnew %>%
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))


# =============================================================================
# DATA CLEANING AND FILTERING
# =============================================================================

leadsnewf <- leadsnew %>%
  select(-Pitcher.y, -Catcher.y, -popcount.x) %>%
  mutate(
    Pitcher     = Pitcher.x,
    Catcher     = Catcher.x,
    Threat      = Threat.x,
    sprint_speed = sprint_speed.x,
    poptime     = poptime.x
  ) %>%
  select(-Pitcher.x, -Catcher.x, -Threat.x, -sprint_speed.x, -poptime.x) %>%
  select(Date, Home, Away, Inning, TopBottom,
         Runner1B, PrimaryLead1B, sprint_speed,
         SB1, CS1, PK1,
         Pitcher, pitch_hand, Threat, Catcher, poptime,
         outs, Balls, Strikes, Play, everything()) %>%
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))

# Restrict to base state: runner on 1st only, all covariates present
leadsnew1b <- leadsnewf %>%
  filter(BaseState == "1--") %>%
  filter(!is.na(Threat)) %>%
  filter(!is.na(poptime)) %>%
  filter(!is.na(sprint_speed)) %>%
  filter(!is.na(PrimaryLead1B)) %>%
  mutate(pickoffthrow = case_when(
    Play == "Pitch"   ~ 0,
    Play == "Pickoff" ~ 1
  ))

# Add disengagement state (tracks prior pickoff attempts within PA per 2023 rule)
leadsnew1b <- leadsnew1b %>%
  group_by(Date, Home, Away) %>%
  mutate(
    row_in_game = row_number(),
    start_pa    = (Play == "Pitch" & Balls == 0 & Strikes == 0),
    pa_id       = cumsum(start_pa),
    pa_id       = if_else(pa_id == 0, 1L, pa_id)
  ) %>%
  arrange(row_in_game, .by_group = TRUE) %>%
  group_by(Date, Home, Away, pa_id) %>%
  mutate(
    is_diseng       = as.integer(Play %in% c("Pickoff")),
    dis_used_before = dplyr::lag(cumsum(is_diseng), default = 0L),
    dis_stage       = factor(pmin(2L, dis_used_before), levels = c(0, 1, 2))
  ) %>%
  ungroup() %>%
  select(-row_in_game, -start_pa)

# Scale predictors and factorize IDs
leadsnew1b <- leadsnew1b %>%
  mutate(
    lead_scaled    = as.vector(scale(PrimaryLead1B)),
    threat_scaled  = as.vector(scale(Threat)),
    poptime_scaled = as.vector(scale(poptime)),
    sprint_scaled  = as.vector(scale(sprint_speed)),
    Runner1B_ID    = factor(Runner1B_ID),
    PitcherID      = factor(PitcherID),
    CatcherID      = factor(CatcherID),
    dis_stage      = factor(dis_stage, levels = c(0, 1, 2)),
    PO_y           = as.integer(Play == "Pickoff")
  )


# =============================================================================
# SITUATIONAL WEIGHTS
# Based on FanGraphs run expectancy table (Table 1 in paper)
# Base state: runner on 1st only (1 _ _)
#
# w_SB = RE(runner on 2nd, same outs) - RE(runner on 1st, same outs)
# w_CS = RE(bases empty, outs+1)      - RE(runner on 1st, same outs)
# w_PK = same as w_CS (pickoff = out at first, structurally identical to CS)
#
# Both w_CS and w_PK are negative (losing an out from a non-scoring state)
# xRuns = w_SB * P_SB + w_CS * P_CS + w_PK * P_PK
# The subtraction is preserved in the sign of the weights.
# =============================================================================

sit_weights <- data.frame(
  outs = c(0L, 1L, 2L),
  w_SB = c(1.068 - 0.831,   # 0 outs: RE(2nd,0) - RE(1st,0)
            0.644 - 0.489,   # 1 out:  RE(2nd,1) - RE(1st,1)
            0.305 - 0.214),  # 2 outs: RE(2nd,2) - RE(1st,2)
  w_CS = c(0.243 - 0.831,   # 0 outs: RE(empty,1) - RE(1st,0)
            0.095 - 0.489,   # 1 out:  RE(empty,2) - RE(1st,1)
            0.000 - 0.214),  # 2 outs: RE(empty,inning over) - RE(1st,2)
  w_PK = c(0.243 - 0.831,   # same structure as CS
            0.095 - 0.489,
            0.000 - 0.214)
)


# =============================================================================
# MODEL FITTING
# Stage 1 (PO):      glm,  no random effects
# Stage 2 (PK|PO):   glmer, runner RE only 
# Stage 3 (ATT|~PO): glmer, runner RE + catcher RE
# Stage 4 (SB|ATT):  glmer, runner RE + catcher RE
# =============================================================================

# --- Stage 1: Pickoff attempt (no random effects) ---
m_PO_v2 <- glm(
  PO_y ~ lead_scaled + threat_scaled + dis_stage,
  family  = binomial(),
  data    = leadsnew1b
)

# --- Stage 2: Pickoff success given attempt (runner RE only) ---
m_PK_v2 <- glmer(
  PK1 ~ lead_scaled + threat_scaled + dis_stage + (1 | Runner1B_ID),
  family  = binomial(),
  data    = subset(leadsnew1b, Play == "Pickoff"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# --- Stage 3: Steal attempt given no pickoff (runner RE + catcher RE) ---
leads_pitch_v2        <- subset(leadsnew1b, Play == "Pitch")
leads_pitch_v2$ATT_y  <- as.integer(leads_pitch_v2$SB1 == 1L | leads_pitch_v2$CS1 == 1L)

m_ATT_v2 <- glmer(
  ATT_y ~ threat_scaled + poptime_scaled + sprint_scaled + dis_stage +
    (1 | Runner1B_ID) + (1 | CatcherID),
  family  = binomial(),
  data    = leads_pitch_v2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# --- Stage 4: Stolen base success given attempt (runner RE + catcher RE) ---
leads_attempt_v2      <- subset(leadsnew1b, SB1 == 1L | CS1 == 1L)
leads_attempt_v2$SB_y <- as.integer(leads_attempt_v2$SB1 == 1L)

m_SB_v2 <- glmer(
  SB_y ~ lead_scaled + threat_scaled + poptime_scaled + sprint_scaled + dis_stage +
    (1 | Runner1B_ID) + (1 | CatcherID),
  family  = binomial(),
  data    = leads_attempt_v2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(m_PO_v2)
summary(m_PK_v2)
summary(m_ATT_v2)
summary(m_SB_v2)


# =============================================================================
# SCALER AND LEVEL LOOKUP
# =============================================================================

scaler_v2 <- list(
  lead_mean   = mean(leadsnew1b$PrimaryLead1B,  na.rm = TRUE),
  lead_sd     = sd(leadsnew1b$PrimaryLead1B,    na.rm = TRUE),
  threat_mean = mean(leadsnew1b$Threat,          na.rm = TRUE),
  threat_sd   = sd(leadsnew1b$Threat,            na.rm = TRUE),
  pop_mean    = mean(leadsnew1b$poptime,         na.rm = TRUE),
  pop_sd      = sd(leadsnew1b$poptime,           na.rm = TRUE),
  sprint_mean = mean(leadsnew1b$sprint_speed,    na.rm = TRUE),
  sprint_sd   = sd(leadsnew1b$sprint_speed,      na.rm = TRUE),

  # Runner levels (stages with runner RE)
  runner_lvls_PK  = rownames(ranef(m_PK_v2)$Runner1B_ID),
  runner_lvls_ATT = rownames(ranef(m_ATT_v2)$Runner1B_ID),
  runner_lvls_SB  = rownames(ranef(m_SB_v2)$Runner1B_ID),

  # Catcher levels (stages 3 and 4 only)
  catcher_lvls_ATT = rownames(ranef(m_ATT_v2)$CatcherID),
  catcher_lvls_SB  = rownames(ranef(m_SB_v2)$CatcherID)
)


# =============================================================================
# PREDICTION FUNCTIONS
# =============================================================================

# --- Population-level prediction (no random effects, any runner/catcher) ---
get_prob_v2 <- function(PrimaryLead1B, Threat, poptime, sprint_speed,
                        dis_stage_val = 0, outs_val = 0) {

  w <- sit_weights[sit_weights$outs == outs_val, ]

  nd <- data.frame(
    lead_scaled    = (PrimaryLead1B - scaler_v2$lead_mean)   / scaler_v2$lead_sd,
    threat_scaled  = (Threat        - scaler_v2$threat_mean) / scaler_v2$threat_sd,
    poptime_scaled = (poptime       - scaler_v2$pop_mean)    / scaler_v2$pop_sd,
    sprint_scaled  = (sprint_speed  - scaler_v2$sprint_mean) / scaler_v2$sprint_sd,
    dis_stage      = factor(dis_stage_val, levels = c(0, 1, 2))
  )

  p_PO   <- as.numeric(predict(m_PO_v2,  nd, type = "response"))
  p_PKPO <- as.numeric(predict(m_PK_v2,  nd, type = "response", re.form = NA))
  p_ATT  <- as.numeric(predict(m_ATT_v2, nd, type = "response", re.form = NA))
  pi_SB  <- as.numeric(predict(m_SB_v2,  nd, type = "response", re.form = NA))

  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)

  xRuns <- w$w_SB * P_SB + w$w_CS * P_CS + w$w_PK * P_PK

  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}


# --- Player-level prediction (uses runner and catcher random effects if known) ---
get_prob_v2_player <- function(PrimaryLead1B, Threat, poptime, sprint_speed,
                               dis_stage_val = 0, outs_val = 0,
                               Runner1B_ID, CatcherID,
                               use_RE = TRUE) {

  rid <- as.character(Runner1B_ID)
  cid <- as.character(CatcherID)

  in_runner_PK  <- rid %in% scaler_v2$runner_lvls_PK
  in_runner_ATT <- rid %in% scaler_v2$runner_lvls_ATT
  in_runner_SB  <- rid %in% scaler_v2$runner_lvls_SB

  in_catcher_ATT <- cid %in% scaler_v2$catcher_lvls_ATT
  in_catcher_SB  <- cid %in% scaler_v2$catcher_lvls_SB

  # Fall back to population-level if RE disabled or either entity unknown
  runner_known  <- in_runner_PK  && in_runner_ATT  && in_runner_SB
  catcher_known <- in_catcher_ATT && in_catcher_SB

  if (!use_RE || !runner_known || !catcher_known) {
    return(get_prob_v2(PrimaryLead1B, Threat, poptime, sprint_speed,
                       dis_stage_val, outs_val))
  }

  w <- sit_weights[sit_weights$outs == outs_val, ]

  nd_base <- data.frame(
    lead_scaled    = (PrimaryLead1B - scaler_v2$lead_mean)   / scaler_v2$lead_sd,
    threat_scaled  = (Threat        - scaler_v2$threat_mean) / scaler_v2$threat_sd,
    poptime_scaled = (poptime       - scaler_v2$pop_mean)    / scaler_v2$pop_sd,
    sprint_scaled  = (sprint_speed  - scaler_v2$sprint_mean) / scaler_v2$sprint_sd,
    dis_stage      = factor(dis_stage_val, levels = c(0, 1, 2))
  )

  # Stage 1: PO — no random effects
  p_PO <- as.numeric(predict(m_PO_v2, nd_base, type = "response"))

  # Stage 2: PK — runner RE only
  nd_PK <- nd_base
  nd_PK$Runner1B_ID <- factor(rid, levels = scaler_v2$runner_lvls_PK)
  p_PKPO <- as.numeric(predict(m_PK_v2, nd_PK, type = "response", re.form = NULL))

  # Stage 3: ATT — runner RE + catcher RE
  nd_ATT <- nd_base
  nd_ATT$Runner1B_ID <- factor(rid, levels = scaler_v2$runner_lvls_ATT)
  nd_ATT$CatcherID   <- factor(cid, levels = scaler_v2$catcher_lvls_ATT)
  p_ATT <- as.numeric(predict(m_ATT_v2, nd_ATT, type = "response", re.form = NULL))

  # Stage 4: SB — runner RE + catcher RE
  nd_SB <- nd_base
  nd_SB$Runner1B_ID <- factor(rid, levels = scaler_v2$runner_lvls_SB)
  nd_SB$CatcherID   <- factor(cid, levels = scaler_v2$catcher_lvls_SB)
  pi_SB <- as.numeric(predict(m_SB_v2, nd_SB, type = "response", re.form = NULL))

  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)

  xRuns <- w$w_SB * P_SB + w$w_CS * P_CS + w$w_PK * P_PK

  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}


# --- Optimal lead for a given situation ---
get_optimal_lead_v2 <- function(Threat, poptime, sprint_speed,
                                dis_stage_val = 0, outs_val = 0,
                                Runner1B_ID, CatcherID,
                                use_RE = TRUE,
                                interval = c(0, 90)) {

  xRuns_func <- function(L) {
    get_prob_v2_player(
      L, Threat, poptime, sprint_speed,
      dis_stage_val, outs_val,
      Runner1B_ID, CatcherID, use_RE
    )$xRuns
  }

  opt <- optimize(xRuns_func, interval = interval, maximum = TRUE)

  rid <- as.character(Runner1B_ID)
  cid <- as.character(CatcherID)

  runner_known  <- rid %in% scaler_v2$runner_lvls_PK &&
                   rid %in% scaler_v2$runner_lvls_ATT &&
                   rid %in% scaler_v2$runner_lvls_SB
  catcher_known <- cid %in% scaler_v2$catcher_lvls_ATT &&
                   cid %in% scaler_v2$catcher_lvls_SB

  tibble::tibble(
    Runner1B_ID          = rid,
    CatcherID            = cid,
    runner_known         = runner_known,
    catcher_known        = catcher_known,
    used_player_RE       = runner_known && catcher_known && use_RE,
    dis_stage            = dis_stage_val,
    outs                 = outs_val,
    optimal_lead         = round(opt$maximum, 2),
    optimal_xRuns        = opt$objective
  )
}


# =============================================================================
# COMPUTE OPTIMAL LEADS AND DEVIATIONS FOR ALL OBSERVATIONS
# =============================================================================

leadsnew1b_full_v2 <- leadsnew1b %>%
  rowwise() %>%
  mutate(
    opt_result = list(get_optimal_lead_v2(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)),
      outs,
      Runner1B_ID, CatcherID,
      use_RE = TRUE
    )),
    xRuns_player = get_prob_v2_player(
      PrimaryLead1B, Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)),
      outs,
      Runner1B_ID, CatcherID,
      use_RE = TRUE
    )$xRuns
  ) %>%
  ungroup() %>%
  mutate(
    optLead_player   = map_dbl(opt_result, "optimal_lead"),
    optxRuns_player  = map_dbl(opt_result, "optimal_xRuns"),
    runner_known     = map_lgl(opt_result, "runner_known"),
    catcher_known    = map_lgl(opt_result, "catcher_known"),
    used_player_RE   = map_lgl(opt_result, "used_player_RE"),
    leadDev_player   = PrimaryLead1B - optLead_player,
    rec_player       = if_else(optxRuns_player > 0, "Steal", "Stay")
  ) %>%
  select(-opt_result)


leadsnew1b_full_v2 <- read.csv("data/leadsnew1b_full_v2.csv") %>%
  mutate(xRuns_lost = optxRuns_player - xRuns_player)
