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
view(leads_attempt)
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

# leadsnew1b: filtered version of leadsnewf to situations with:
# valid catcher, pitcher, runner metrics, runner on 1st only.
# Will serve as the main df from here since we only focus on these game situations

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

# leads1bpkthrow contains only pickoff attempts (Throws to 1B). 127 successful pickoffs, 7439 attempts

leads1bpkthrow <- leadsnew1b %>% 
  filter(pickoffthrow == 1)

# leadsnewer1b filters out outlier situations where 1B is not holding runner on
# for (maybe)? training pickoff throw model

leadsnewer1b <- leadsnew1b %>% 
  filter(!(PrimaryLead1B >= 18 & pickoffthrow == 0))

# CREATE dis_stage VARIABLE (tracks disengagement count in PA)
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
    is_diseng = as.integer(Play %in% c("Pickoff")),  # add "StepOff" here if it exists
    dis_used_before = dplyr::lag(cumsum(is_diseng), default = 0L),
    dis_stage = factor(pmin(2L, dis_used_before), levels = c(0, 1, 2))
  ) %>%
  ungroup() %>%
  select(-row_in_game, -start_pa)

# SCALE CONTINUOUS PREDICTORS (needed for mixed effects models)
leadsnew1b <- leadsnew1b %>%
  mutate(
    lead_scaled = as.vector(scale(PrimaryLead1B)),
    threat_scaled = as.vector(scale(Threat)),
    poptime_scaled = as.vector(scale(poptime)),
    sprint_scaled = as.vector(scale(sprint_speed)),
    
    # Ensure factors
    Runner1B_ID = factor(Runner1B_ID),
    PitcherID = factor(PitcherID),
    CatcherID = factor(CatcherID),
    dis_stage = factor(dis_stage, levels = c(0, 1, 2)),
    
    # Create outcome variables
    PO_y = as.integer(Play == "Pickoff")
  )

### Models (OLD)

# (1) probability of successful SB on given pitch (NOT ASSUMING THE RUNNER GOES)
xsbModelnew <- glm(SB1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
                   family = binomial(link = "logit"), data = leadsnew1b)

# (2) probability of caught stealing on given pitch (NOT ASSUMING THE RUNNER GOES)
xcsModelnew <- glm(CS1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
                   family = binomial(link = "logit"), data = leadsnew1b)

# (3) probability pitcher throws over to 1B, i.e. attempts a pickoff
xThrowOverModel <- glm(pickoffthrow ~ PrimaryLead1B + Threat, 
                       family = binomial(link = "logit"), data = leadsnew1b) # IS THIS THE RIGHT TRAINING DATA?

# (4) given a pickoff throw, probability it's successful given lead distance only
xpksuccessModel <- glm(PK1 ~ PrimaryLead1B, 
                       family = binomial(link = "logit"), data = leads1bpkthrow)



### Models (GLM)

# Node 1: Pickoff attempt?  (all plays)
leadsnew1b <- leadsnew1b %>% mutate(PO_y = as.integer(Play == "Pickoff"))
m_PO_glm <- glm(PO_y ~ PrimaryLead1B + Threat, family = binomial(), data = leadsnew1b)

# Node 2: Pickoff success?  (pickoff plays only)
m_PK_given_PO_glm <- glm(PK1 ~ PrimaryLead1B + Threat, family = binomial(),
                     data = subset(leadsnew1b, Play == "Pickoff"))

# Node 3: Runner goes?  (pitch plays only). Note: DOES NOT DEPEND ON LEAD DIST BC WE ARE CHANGING THAT IN GAME
leads_pitch <- subset(leadsnew1b, Play == "Pitch")
leads_pitch$ATT_y <- as.integer(leads_pitch$SB1 == 1L | leads_pitch$CS1 == 1L)
m_ATT_given_Pitch_glm <- glm(ATT_y ~ Threat + poptime + sprint_speed,
                         family = binomial(), data = leads_pitch)

# Node 4: Safe at 2nd?  (attempted steals only)
leads_attempt <- subset(leadsnew1b, SB1 == 1L | CS1 == 1L)
leads_attempt$SB_y <- as.integer(leads_attempt$SB1 == 1L)
m_SB_given_ATT_glm <- glm(SB_y ~ PrimaryLead1B + Threat + poptime + sprint_speed,
                      family = binomial(), data = leads_attempt)


#####
### Models (Mixed Model) NEW


# Node 1: Pickoff attempt? (all plays)
m_PO_mixed <- glmer(
  PO_y ~ lead_scaled + threat_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = leadsnew1b,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# Node 2: Pickoff success? (pickoff plays only)

m_PK_given_PO_mixed <- glmer(
  PK1 ~ lead_scaled + threat_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = subset(leadsnew1b, Play == "Pickoff"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Node 3: Runner goes? (pitch plays only)

leads_pitch_mixed <- subset(leadsnew1b, Play == "Pitch")
leads_pitch_mixed$ATT_y <- as.integer(leads_pitch_mixed$SB1 == 1L | leads_pitch_mixed$CS1 == 1L)
m_ATT_given_Pitch_mixed <- glmer(
  ATT_y ~ threat_scaled + poptime_scaled + sprint_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = leads_pitch_mixed,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Node 4: Safe at 2nd? (attempted steals only)

leads_attempt_mixed <- subset(leadsnew1b, SB1 == 1L | CS1 == 1L)
leads_attempt_mixed$SB_y <- as.integer(leads_attempt_mixed$SB1 == 1L)
m_SB_given_ATT_mixed <- glmer(
  SB_y ~ lead_scaled + threat_scaled + poptime_scaled + sprint_scaled + dis_stage + (1 | Runner1B_ID),
  family = binomial(),
  data = leads_attempt_mixed,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

################################################################################
### PREDICTION FUNCTIONS
################################################################################

#get_prob inputs game situation (threat, pop time, sprint speed) + lead
#outputs SCALED probabilities of the 3 possible outcomes and corresponding xRuns value
get_prob_old <- function(PrimaryLead1B, Threat, poptime, sprint_speed) {
  newdata1 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed
  )
  newdata2 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat
  )
  newdata3 <- data.frame(
    PrimaryLead1B = PrimaryLead1B
  )
  xsb = predict(xsbModelnew, newdata1, type = "response")
  xcs = predict(xcsModelnew, newdata1, type = "response")
  xpk = predict(xThrowOverModel, newdata2, type = "response") *
    predict(xpksuccessModel, newdata3, type = "response")
  steal_attempt_prob <- xsb + xcs
  xsb_pct <- ifelse(steal_attempt_prob == 0, 0, xsb / steal_attempt_prob)
  xcs_pct <- ifelse(steal_attempt_prob == 0, 0, xcs / steal_attempt_prob)
  P_PK <- xpk
  P_SB <- (1 - xpk) * xsb_pct
  P_CS <- (1 - xpk) * xcs_pct
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  data.frame(
    P_PK = P_PK,
    P_SB = P_SB,
    P_CS = P_CS,
    xRuns = xRuns
  )
}

get_prob_glm <- function(PrimaryLead1B, Threat, poptime, sprint_speed) {
  nd <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat        = Threat,
    poptime       = poptime,
    sprint_speed  = sprint_speed
  )
  
  p_PO <- as.numeric(predict(m_PO_glm, nd, type = "response"))  
  p_PKPO <- as.numeric(predict(m_PK_given_PO_glm, nd, type = "response"))  
  p_ATT <- as.numeric(predict(m_ATT_given_Pitch_glm, nd, type = "response"))  
  pi_SB <- as.numeric(predict(m_SB_given_ATT_glm, nd, type = "response"))  
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)
  
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}

##Mixed Effects version

# Create scaler object with runner levels for EACH model
scaler <- list(
  lead_mean   = mean(leadsnew1b$PrimaryLead1B, na.rm = TRUE),
  lead_sd     = sd(leadsnew1b$PrimaryLead1B, na.rm = TRUE),
  threat_mean = mean(leadsnew1b$Threat, na.rm = TRUE),
  threat_sd   = sd(leadsnew1b$Threat, na.rm = TRUE),
  pop_mean    = mean(leadsnew1b$poptime, na.rm = TRUE),
  pop_sd      = sd(leadsnew1b$poptime, na.rm = TRUE),
  sprint_mean = mean(leadsnew1b$sprint_speed, na.rm = TRUE),
  sprint_sd   = sd(leadsnew1b$sprint_speed, na.rm = TRUE),
  
  # Get runner levels directly from the fitted models (most reliable!)
  runner_lvls_PO = rownames(ranef(m_PO_mixed)$Runner1B_ID),
  runner_lvls_PK = rownames(ranef(m_PK_given_PO_mixed)$Runner1B_ID),
  runner_lvls_ATT = rownames(ranef(m_ATT_given_Pitch_mixed)$Runner1B_ID),
  runner_lvls_SB = rownames(ranef(m_SB_given_ATT_mixed)$Runner1B_ID)
)

#Helper function to validate runner IDs
coerce_runner <- function(runner_id) {
  runner_id <- as.character(runner_id)
  if (!runner_id %in% scaler$runner_lvls) return(NA_character_)
  runner_id
}


# W NO RUNNER EFFECTS
get_prob_mixed <- function(PrimaryLead1B, Threat, poptime, sprint_speed, dis_stage_val = 0) {
  
  nd <- data.frame(
    lead_scaled = (PrimaryLead1B - scaler$lead_mean) / scaler$lead_sd,
    threat_scaled = (Threat - scaler$threat_mean) / scaler$threat_sd,
    poptime_scaled = (poptime - scaler$pop_mean) / scaler$pop_sd,
    sprint_scaled = (sprint_speed - scaler$sprint_mean) / scaler$sprint_sd,
    dis_stage = factor(dis_stage_val, levels = c(0, 1, 2))
  )
  
  # re.form = NA means population-level (ignore random effects)
  p_PO <- as.numeric(predict(m_PO_mixed, nd, type = "response", re.form = NA))
  p_PKPO <- as.numeric(predict(m_PK_given_PO_mixed, nd, type = "response", re.form = NA))
  p_ATT <- as.numeric(predict(m_ATT_given_Pitch_mixed, nd, type = "response", re.form = NA))
  pi_SB <- as.numeric(predict(m_SB_given_ATT_mixed, nd, type = "response", re.form = NA))
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)
  
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  
  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}

# Runner-specific predictions (includes runner random effects)
get_prob_mixed_player <- function(PrimaryLead1B, Threat, poptime, sprint_speed,
                                  dis_stage_val = 0,
                                  Runner1B_ID,
                                  use_RE = TRUE) {
  
  rid <- as.character(Runner1B_ID)
  
  # Check if runner is in each model's training data
  in_PO  <- rid %in% scaler$runner_lvls_PO
  in_PK  <- rid %in% scaler$runner_lvls_PK
  in_ATT <- rid %in% scaler$runner_lvls_ATT
  in_SB  <- rid %in% scaler$runner_lvls_SB
  
  # If runner is unknown in ANY model OR use_RE is FALSE, use population-level
  if (!use_RE || !in_PO || !in_PK || !in_ATT || !in_SB) {
    return(get_prob_mixed(PrimaryLead1B, Threat, poptime, sprint_speed, dis_stage_val))
  }
  
  # If we get here, runner IS known in ALL models
  nd <- data.frame(
    lead_scaled = (PrimaryLead1B - scaler$lead_mean) / scaler$lead_sd,
    threat_scaled = (Threat - scaler$threat_mean) / scaler$threat_sd,
    poptime_scaled = (poptime - scaler$pop_mean) / scaler$pop_sd,
    sprint_scaled = (sprint_speed - scaler$sprint_mean) / scaler$sprint_sd,
    dis_stage = factor(dis_stage_val, levels = c(0, 1, 2)),
    Runner1B_ID = factor(rid, levels = scaler$runner_lvls_PO)  # Use PO levels as reference
  )
  
  # Use runner-specific random effects
  p_PO <- as.numeric(predict(m_PO_mixed, nd, type = "response", re.form = NULL))
  
  # For PK model, create newdata with PK-specific levels
  nd_PK <- nd
  nd_PK$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_PK)
  p_PKPO <- as.numeric(predict(m_PK_given_PO_mixed, nd_PK, type = "response", re.form = NULL))
  
  # For ATT model
  nd_ATT <- nd
  nd_ATT$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_ATT)
  p_ATT <- as.numeric(predict(m_ATT_given_Pitch_mixed, nd_ATT, type = "response", re.form = NULL))
  
  # For SB model
  nd_SB <- nd
  nd_SB$Runner1B_ID <- factor(rid, levels = scaler$runner_lvls_SB)
  pi_SB <- as.numeric(predict(m_SB_given_ATT_mixed, nd_SB, type = "response", re.form = NULL))
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * pi_SB
  P_CS <- (1 - p_PO) * p_ATT * (1 - pi_SB)
  
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  
  data.frame(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS, xRuns = xRuns)
}















#note on outcomes/scaling:
#the model output probabilities xsb, xcs, xpk do not necessarily add to 1. (Because the models are independent and don't assume the runner intends to go) 
#we fix this by scaling the first two
#specifically:
#since the runner intends to go, there are basically 2 outcomes: pickoff or SB attempt. this is governed by P(Pickoff) = xpk, P(SB attempt) = 1 - xpk
#then SB attempt has 2 sub-outcomes: Steal or Caught. we don't know them, but we know the ratio, which is sb% = xsb / (xsb + xcs)
#so P(SB) = P(SB attempt) * P(Steal | SB Attempt) = (1 - xpk) * (sb%)
#and P(CS) = (1 - xpk) * (1 - sb%). so we get 3 scaled probabilities that add to 1

#this would have to be changed if we wanted to add "nothing happens" outcome. 
#i think this is very hard to do because it necessitates measuring runner intent to steal, which is very unclear 

# example: 30ft lead
# get_prob(30, 3.068454647, 2.01, 29.5)

#get_optimal_lead inputs a given threat, pop time, sprint speedf
# outputs:
# - optimal lead distance (which maximizes xRuns according to get_prob)
# - maximum xRuns value, i.e. the xRuns at that optimal lead distance
get_optimal_lead_old <- function(Threat, poptime, sprint_speed) {
  xRuns_func <- function(PrimaryLead1B) {
    get_prob_old(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns  
  }
  opt <- optimize(xRuns_func, interval = c(0, 90), maximum = TRUE)
  tibble::tibble(
    PrimaryLead1B = round(opt$maximum, 2),
    xRuns = opt$objective
  )
}
get_optimal_lead_glm <- function(Threat, poptime, sprint_speed) {
  xRuns_func <- function(PrimaryLead1B) {
    get_prob_glm(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns  
  }
  opt <- optimize(xRuns_func, interval = c(0, 90), maximum = TRUE)
  tibble::tibble(
    PrimaryLead1B = round(opt$maximum, 2),
    xRuns = opt$objective
  )
}
get_optimal_lead_mixed <- function(Threat, poptime, sprint_speed, dis_stage_val = 0) {
  xRuns_func <- function(PrimaryLead1B) {
    get_prob_mixed(PrimaryLead1B, Threat, poptime, sprint_speed, dis_stage_val)$xRuns
  }
  opt <- optimize(xRuns_func, interval = c(0, 90), maximum = TRUE)
  tibble::tibble(
    PrimaryLead1B = round(opt$maximum, 2),
    xRuns = opt$objective
  )
}

get_optimal_lead_mixed_player <- function(Threat, poptime, sprint_speed,
                                          dis_stage_val = 0,
                                          Runner1B_ID,
                                          use_RE = TRUE,
                                          interval = c(0, 90)) {
  
  xRuns_func <- function(L) {
    get_prob_mixed_player(L, Threat, poptime, sprint_speed,
                          dis_stage_val = dis_stage_val,
                          Runner1B_ID = Runner1B_ID,
                          use_RE = use_RE)$xRuns
  }
  
  opt <- optimize(xRuns_func, interval = interval, maximum = TRUE)
  
  rid <- as.character(Runner1B_ID)
  
  # Check if in all models
  in_all <- rid %in% scaler$runner_lvls_PO && 
    rid %in% scaler$runner_lvls_PK && 
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







leadsnew1b_full <- leadsnew1b %>%
  rowwise() %>%
  mutate(
    # Approach 1: OLD
    xRuns_old = get_prob_old(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optLead_old = get_optimal_lead_old(Threat, poptime, sprint_speed)$PrimaryLead1B,
    optxRuns_old = get_optimal_lead_old(Threat, poptime, sprint_speed)$xRuns,
    
    # Approach 2: GLM
    xRuns_glm = get_prob_glm(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optLead_glm = get_optimal_lead_glm(Threat, poptime, sprint_speed)$PrimaryLead1B,
    optxRuns_glm = get_optimal_lead_glm(Threat, poptime, sprint_speed)$xRuns,
    
    # Approach 3: MIXED (population-level)
    xRuns_mixed = get_prob_mixed(
      PrimaryLead1B, Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$xRuns,
    optLead_mixed = get_optimal_lead_mixed(
      Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$PrimaryLead1B,
    optxRuns_mixed = get_optimal_lead_mixed(
      Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$xRuns,
    
    # Approach 4: MIXED (runner-specific)
    xRuns_player = get_prob_mixed_player(
      PrimaryLead1B, Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), Runner1B_ID, TRUE
    )$xRuns,
    optLead_player = get_optimal_lead_mixed_player(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), 
      Runner1B_ID, TRUE
    )$optimal_lead,
    optxRuns_player = get_optimal_lead_mixed_player(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), 
      Runner1B_ID, TRUE
    )$optimal_xRuns
  ) %>%
  ungroup() %>%
  mutate(
    # Lead deviations
    leadDev_old = PrimaryLead1B - optLead_old,
    leadDev_glm = PrimaryLead1B - optLead_glm,
    leadDev_mixed = PrimaryLead1B - optLead_mixed,
    leadDev_player = PrimaryLead1B - optLead_player,
    
    # Recommendations
    rec_old = if_else(optxRuns_old > 0, "Steal", "Stay"),
    rec_glm = if_else(optxRuns_glm > 0, "Steal", "Stay"),
    rec_mixed = if_else(optxRuns_mixed > 0, "Steal", "Stay"),
    rec_player = if_else(optxRuns_player > 0, "Steal", "Stay")
  )



leadsnew1b_full %>%
  summarise(
    `OLD Model` = round(mean(leadDev_old, na.rm = TRUE), 3),
    `GLM Model` = round(mean(leadDev_glm, na.rm = TRUE), 3),
    `Mixed (Population)` = round(mean(leadDev_mixed, na.rm = TRUE), 3),
    `Mixed (Player-Specific)` = round(mean(leadDev_player, na.rm = TRUE), 3)
  ) %>%
  print()


















#SAMPLE.
leadsnew1bSAMPLE <- leadsnew1b[sample(nrow(leadsnew1b), 30), ]

leadsnew1bSAMPLE <- leadsnew1bSAMPLE %>%
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
  mutate(recommendation = if_else(optimalxRunsNew > 0, "Steal", "Stay"),
         leadChange = optimalLead1BNew - PrimaryLead1B) %>%
  select(-optimalNew)

#warning: this takes a long time

set.seed(123)
leadsnew1bSAMPLE <- leadsnew1b[sample(nrow(leadsnew1b), 100), ]

# Add predictions from all approaches
leadsnew1bSAMPLE <- leadsnew1bSAMPLE %>%
  rowwise() %>%
  mutate(
    # Approach 1: OLD
    xRuns_old = get_prob_old(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    # Call the function and extract directly (no intermediate storage)
    optLead_old = get_optimal_lead_old(Threat, poptime, sprint_speed)$PrimaryLead1B,
    optxRuns_old = get_optimal_lead_old(Threat, poptime, sprint_speed)$xRuns,
    
    # Approach 2: GLM
    xRuns_glm = get_prob_glm(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optLead_glm = get_optimal_lead_glm(Threat, poptime, sprint_speed)$PrimaryLead1B,
    optxRuns_glm = get_optimal_lead_glm(Threat, poptime, sprint_speed)$xRuns,
    
    # Approach 3: MIXED (population-level)
    xRuns_mixed = get_prob_mixed(
      PrimaryLead1B, Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$xRuns,
    optLead_mixed = get_optimal_lead_mixed(
      Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$PrimaryLead1B,
    optxRuns_mixed = get_optimal_lead_mixed(
      Threat, poptime, sprint_speed, 
      as.numeric(as.character(dis_stage))
    )$xRuns,
    
    # Approach 3: MIXED (runner-specific)
    xRuns_player = get_prob_mixed_player(
      PrimaryLead1B, Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), Runner1B_ID, TRUE
    )$xRuns,
    optLead_player = get_optimal_lead_mixed_player(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), 
      Runner1B_ID, TRUE
    )$optimal_lead,
    optxRuns_player = get_optimal_lead_mixed_player(
      Threat, poptime, sprint_speed,
      as.numeric(as.character(dis_stage)), 
      Runner1B_ID, TRUE
    )$optimal_xRuns
  ) %>%
  ungroup() %>%
  mutate(
    # Lead deviations
    leadDev_old = PrimaryLead1B - optLead_old,
    leadDev_glm = PrimaryLead1B - optLead_glm,
    leadDev_mixed = PrimaryLead1B - optLead_mixed,
    leadDev_player = PrimaryLead1B - optLead_player,
    
    # Recommendations
    rec_old = if_else(optxRuns_old > 0, "Steal", "Stay"),
    rec_glm = if_else(optxRuns_glm > 0, "Steal", "Stay"),
    rec_mixed = if_else(optxRuns_mixed > 0, "Steal", "Stay"),
    rec_player = if_else(optxRuns_player > 0, "Steal", "Stay")
  )
# View results
print(leadsnew1bSAMPLE %>%
        select(Runner1B, PrimaryLead1B, dis_stage, 
               optLead_old, optLead_glm, optLead_mixed, optLead_player,
               xRuns_old, xRuns_glm, xRuns_mixed, xRuns_player) %>%
        head(5))








##grouping together the at-bats

leads_with_ab <- leadsnew1b %>%
  arrange(Date, Home, Away, Inning, TopBottom, outs, Balls, Strikes) %>%
  group_by(Date, Home, Away) %>%
  mutate(
    # new PA if:
    # 1) a pitch with 0-0 count,
    # 2) OR a pickoff that occurs before the first pitch of the PA (0-0 count, no outs change)
    is_new_ab = 
      (Play == "Pitch" & Balls == 0 & Strikes == 0) |
      (Play == "Pickoff" & Balls == 0 & Strikes == 0),
    is_new_ab = replace_na(is_new_ab, FALSE),
    ab_seq = cumsum(is_new_ab),
    ab_id  = str_c(Date, Home, Away, Inning, TopBottom, ab_seq, sep = "-")
  )


leads_ab <- leads_with_ab %>%
  group_by(
    ab_id, Date, Home, Away, Inning, TopBottom,
    PitcherID, Pitcher, CatcherID, Catcher, Runner1B_ID, Runner1B
  ) %>%
  summarise(
    mean_lead = mean(PrimaryLead1B, na.rm = TRUE)
  )


# extra analysis to do: what % of situations is it best to run given real lead? given optimal lead?
# 72% // 86%
# Save all key processed datasets from full project
write.csv(leadsnew, "data/processed/full_leads_with_all_metrics.csv", row.names = FALSE)
write.csv(leadsnew1b, "data/processed/full_leads_1b_final.csv", row.names = FALSE)
write.csv(leadsnewer1b, "data/processed/full_leads_1b_filtered_final.csv", row.names = FALSE)
