library(tidyverse)
library(baseballr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(splines)
library(lme4)

### Read in data (Replace with your own file path)

#leadsnew: play-level lead distance data. here "play" means pitch / pickoff att, designated by Play col
leadsnew <- read.csv("data/raw/lead-distances.csv")

#Net Bases Prevented data from Savant (player-level)
nbpdata <- read.csv("data/raw/net-bases-prevented.csv")

#Innings Pitched data from Savant (player-level)
ipdata <- read.csv("data/raw/innings-pitched.csv")

#Sprint Speed data from Savant (player-level)
ssdata <- read.csv("data/raw/sprint-speed.csv")

#Catcher pop time data from Savant (player-level)
ptdata <- read.csv("data/raw/pop-time.csv")

### Filter and left_join Baseball Savant data into leadsnew

nbpdata <- nbpdata %>% 
  reframe(player_id = player_id, player_name = player_name, NBP = net_attr_plus + net_attr_minus)
ipdata <- ipdata %>% 
  left_join(x = ipdata, y = nbpdata, by = c("player_id" = "player_id"))
ipdata <- ipdata %>% 
  reframe(PitcherID = player_id, Pitcher = player_name, NBP = NBP, IP = round(p_formatted_ip, 1))
ipdata <- ipdata %>% 
  mutate(IP = round(IP, 0)) %>% 
  mutate(Threat = 100 * NBP / IP) %>% 
  filter(IP >= 10)
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))
ptdata <- ptdata %>% 
  reframe(Catcher = entity_name, CatcherID = entity_id, poptime = pop_2b_sba, popcount = pop_2b_sba_count)
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

### Make new filtered datasets

# leadsnewf: fixed/cleaned version of leadsnew with columns rearranged

leadsnewf <- leadsnew %>% 
  select(-Pitcher.y, -Catcher.y, -popcount.x) %>% 
  mutate(Pitcher = Pitcher.x, Catcher = Catcher.x, Threat = Threat.x, sprint_speed = sprint_speed.x, poptime = poptime.x) %>% 
  select(-Pitcher.x, -Catcher.x, -Threat.x, -sprint_speed.x, -poptime.x) %>% 
  select(Date, Home, Away, Inning, TopBottom, 
         Runner1B, PrimaryLead1B, sprint_speed, 
         SB1, CS1, PK1, 
         Pitcher, pitch_hand, Threat, Catcher, poptime, 
         outs, Balls, Strikes, Play, everything()) %>% 
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))

leadsnew1b <- leadsnewf %>% 
  filter(BaseState == "1--") %>% 
  filter(!is.na(Threat)) %>% 
  filter(!is.na(poptime)) %>% 
  filter(!is.na(sprint_speed)) %>% 
  filter(!is.na(PrimaryLead1B)) %>% 
  mutate(pickoffthrow = case_when(
    Play == "Pitch" ~ 0,
    Play == "Pickoff" ~ 1
  ))

leadsnew1b <- leadsnew1b %>%
  group_by(Date, Home, Away) %>%
  mutate(
    row_in_game = row_number(),
    start_pa = (Play == "Pitch" & Balls == 0 & Strikes == 0),
    pa_id = cumsum(start_pa),
    pa_id = if_else(pa_id == 0, 1L, pa_id)
  ) %>%
  arrange(row_in_game, .by_group = TRUE) %>%
  group_by(Date, Home, Away, pa_id) %>%
  mutate(
    is_diseng = as.integer(Play %in% c("Pickoff")),
    dis_used_before = dplyr::lag(cumsum(is_diseng), default = 0L),
    dis_stage = factor(pmin(2L, dis_used_before), levels = c(0, 1, 2))
  ) %>%
  ungroup() %>%
  select(-row_in_game, -start_pa)

leadsnew1b <- leadsnew1b %>%
  mutate(
    lead_scaled = as.vector(scale(PrimaryLead1B)),
    threat_scaled = as.vector(scale(Threat)),
    poptime_scaled = as.vector(scale(poptime)),
    sprint_scaled = as.vector(scale(sprint_speed)),
    Runner1B_ID = factor(Runner1B_ID),
    PitcherID = factor(PitcherID),
    CatcherID = factor(CatcherID),
    dis_stage = factor(dis_stage, levels = c(0, 1, 2)),
    PO_y = as.integer(Play == "Pickoff")
  )



# 1. PO - NO random effects (regular glm)
m_PO <- glm(
  PO_y ~ lead_scaled + threat_scaled + dis_stage,
  family = binomial(),
  data = leadsnew1b
)

# 2. PK|PO - YES random effects
m_PK_given_PO_mixed <- glmer(
  PK1 ~ lead_scaled + threat_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = subset(leadsnew1b, Play == "Pickoff"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# 3. ATT - YES random effects (keep it!)
leads_pitch_mixed <- subset(leadsnew1b, Play == "Pitch")
leads_pitch_mixed$ATT_y <- as.integer(leads_pitch_mixed$SB1 == 1L | leads_pitch_mixed$CS1 == 1L)

m_ATT_given_Pitch_mixed <- glmer(
  ATT_y ~ threat_scaled + poptime_scaled + sprint_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = leads_pitch_mixed,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# 4. SB|ATT - YES random effects
leads_attempt_mixed <- subset(leadsnew1b, SB1 == 1L | CS1 == 1L)
leads_attempt_mixed$SB_y <- as.integer(leads_attempt_mixed$SB1 == 1L)

m_SB_given_ATT_mixed <- glmer(
  SB_y ~ lead_scaled + threat_scaled + poptime_scaled + sprint_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = leads_attempt_mixed,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



summary(m_PO)
summary(m_PK_given_PO_mixed)
summary(m_ATT_given_Pitch_mixed)
summary(m_SB_given_ATT_mixed)



# Create scaler

scaler <- list(
  lead_mean   = mean(leadsnew1b$PrimaryLead1B, na.rm = TRUE),
  lead_sd     = sd(leadsnew1b$PrimaryLead1B, na.rm = TRUE),
  threat_mean = mean(leadsnew1b$Threat, na.rm = TRUE),
  threat_sd   = sd(leadsnew1b$Threat, na.rm = TRUE),
  pop_mean    = mean(leadsnew1b$poptime, na.rm = TRUE),
  pop_sd      = sd(leadsnew1b$poptime, na.rm = TRUE),
  sprint_mean = mean(leadsnew1b$sprint_speed, na.rm = TRUE),
  sprint_sd   = sd(leadsnew1b$sprint_speed, na.rm = TRUE),
  
  # ONLY include models WITH random effects
  runner_lvls_PK = rownames(ranef(m_PK_given_PO_mixed)$Runner1B_ID),
  runner_lvls_ATT = rownames(ranef(m_ATT_given_Pitch_mixed)$Runner1B_ID),
  runner_lvls_SB = rownames(ranef(m_SB_given_ATT_mixed)$Runner1B_ID)
  # NO runner_lvls_PO!
)

# Prediction functions
get_prob_mixed <- function(PrimaryLead1B, Threat, poptime, sprint_speed, dis_stage_val = 0) {
  nd <- data.frame(
    lead_scaled = (PrimaryLead1B - scaler$lead_mean) / scaler$lead_sd,
    threat_scaled = (Threat - scaler$threat_mean) / scaler$threat_sd,
    poptime_scaled = (poptime - scaler$pop_mean) / scaler$pop_sd,
    sprint_scaled = (sprint_speed - scaler$sprint_mean) / scaler$sprint_sd,
    dis_stage = factor(dis_stage_val, levels = c(0, 1, 2))
  )
  
  # m_PO is glm - just predict normally (no re.form parameter)
  p_PO <- as.numeric(predict(m_PO, nd, type = "response"))
  
  # Others use re.form = NA for population-level
  p_PKPO <- as.numeric(predict(m_PK_given_PO_mixed, nd, type = "response", re.form = NA))
  p_ATT <- as.numeric(predict(m_ATT_given_Pitch_mixed, nd, type = "response", re.form = NA))
  pi_SB <- as.numeric(predict(m_SB_given_ATT_mixed, nd, type = "response", re.form = NA))
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)
  
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}

get_prob_mixed_player <- function(PrimaryLead1B, Threat, poptime, sprint_speed,
                                  dis_stage_val = 0, Runner1B_ID, use_RE = TRUE) {
  
  rid <- as.character(Runner1B_ID)
  
  # Check if runner is in models WITH random effects (NOT m_PO!)
  # REMOVE: in_PO check
  in_PK  <- rid %in% scaler$runner_lvls_PK
  in_ATT <- rid %in% scaler$runner_lvls_ATT
  in_SB  <- rid %in% scaler$runner_lvls_SB
  
  # If not in ALL mixed models OR use_RE is FALSE, use population-level
  if (!use_RE || !in_PK || !in_ATT || !in_SB) {  # REMOVED in_PO check
    return(get_prob_mixed(PrimaryLead1B, Threat, poptime, sprint_speed, dis_stage_val))
  }
  
  # If we get here, runner IS known in all MIXED models
  nd <- data.frame(
    lead_scaled = (PrimaryLead1B - scaler$lead_mean) / scaler$lead_sd,
    threat_scaled = (Threat - scaler$threat_mean) / scaler$threat_sd,
    poptime_scaled = (poptime - scaler$pop_mean) / scaler$pop_sd,
    sprint_scaled = (sprint_speed - scaler$sprint_mean) / scaler$sprint_sd,
    dis_stage = factor(dis_stage_val, levels = c(0, 1, 2))
    # NO Runner1B_ID needed for m_PO since it's not in the model
  )
  
  # m_PO has NO random effects - just predict normally
  p_PO <- as.numeric(predict(m_PO, nd, type = "response"))
  
  # For PK model - use runner-specific
  nd_PK <- nd
  nd_PK$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_PK)
  p_PKPO <- as.numeric(predict(m_PK_given_PO_mixed, nd_PK, type = "response", re.form = NULL))
  
  # For ATT model - use runner-specific
  nd_ATT <- nd
  nd_ATT$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_ATT)
  p_ATT <- as.numeric(predict(m_ATT_given_Pitch_mixed, nd_ATT, type = "response", re.form = NULL))
  
  # For SB model - use runner-specific
  nd_SB <- nd
  nd_SB$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_SB)
  pi_SB <- as.numeric(predict(m_SB_given_ATT_mixed, nd_SB, type = "response", re.form = NULL))
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)
  
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  
  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}

get_optimal_lead_mixed_player <- function(Threat, poptime, sprint_speed, 
                                          dis_stage_val = 0, Runner1B_ID, 
                                          use_RE = TRUE, interval = c(0, 90)) {
  
  xRuns_func <- function(L) {
    get_prob_mixed_player(L, Threat, poptime, sprint_speed, 
                          dis_stage_val, Runner1B_ID, use_RE)$xRuns
  }
  
  opt <- optimize(xRuns_func, interval = interval, maximum = TRUE)
  
  rid <- as.character(Runner1B_ID)
  
  # Check if runner is in all MIXED models (NOT m_PO since it has no RE)
  in_all <- rid %in% scaler$runner_lvls_PK &&   # REMOVED in_PO check
    rid %in% scaler$runner_lvls_ATT && 
    rid %in% scaler$runner_lvls_SB
  
  tibble::tibble(
    Runner1B_ID = rid,
    runner_known_all_models = in_all,
    dis_stage = dis_stage_val,
    optimal_lead = round(opt$maximum, 2),
    optimal_xRuns = opt$objective
  )
}


# THE ACTUAL PREDICTION
leadsnew1b_full <- leadsnew1b %>%
  rowwise() %>%
  mutate(
    opt_result = list(get_optimal_lead_mixed_player(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), 
      Runner1B_ID, TRUE
    )),
    xRuns_player = get_prob_mixed_player(
      PrimaryLead1B, Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), Runner1B_ID, TRUE
    )$xRuns
  ) %>%
  ungroup() %>%
  mutate(
    optLead_player = map_dbl(opt_result, "optimal_lead"),
    optxRuns_player = map_dbl(opt_result, "optimal_xRuns"),
    runner_known = map_lgl(opt_result, "runner_known_all_models"),
    leadDev_player = PrimaryLead1B - optLead_player,
    rec_player = if_else(optxRuns_player > 0, "Steal", "Stay")
  ) %>%
  select(-opt_result)

write.csv(leadsnew1b_full, "data/full_leads_mixed_player.csv", row.names = FALSE)
