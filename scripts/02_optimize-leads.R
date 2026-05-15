source("scripts/00_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
})

ensure_dirs(c("data/processed", "internal"))

message("02_optimize_leads_parallel.R starting")

bundle <- readRDS("data/processed/situational_model_bundle.rds")
leads <- read.csv("data/processed/leads_1b_situational.csv", stringsAsFactors = FALSE)

leads <- leads %>%
  mutate(
    Runner1B_ID = as.character(Runner1B_ID),
    CatcherID = as.character(CatcherID),
    dis_stage = factor(dis_stage, levels = c(0, 1, 2))
  )

n_cores <- detect_n_cores(default_cores = 12L)
message("Using n_cores = ", n_cores)

pick_coef <- function(beta, name) {
  if (name %in% names(beta)) {
    as.numeric(beta[[name]])
  } else {
    0
  }
}

dis_stage_effect <- function(beta, stage_int) {
  d1 <- pick_coef(beta, "dis_stage1")
  d2 <- pick_coef(beta, "dis_stage2")
  ifelse(stage_int == 1L, d1, ifelse(stage_int == 2L, d2, 0))
}

outs_effect <- function(beta, outs_int) {
  o1 <- pick_coef(beta, "outs_f1")
  o2 <- pick_coef(beta, "outs_f2")
  ifelse(outs_int == 1L, o1, ifelse(outs_int == 2L, o2, 0))
}

scaler <- bundle$scaler

b_po <- stats::coef(bundle$models$m_PO)
b_pk <- lme4::fixef(bundle$models$m_PK)
b_att <- lme4::fixef(bundle$models$m_ATT)
b_sb <- lme4::fixef(bundle$models$m_SB)

u_pk <- setNames(
  ranef(bundle$models$m_PK)$Runner1B_ID[["(Intercept)"]],
  rownames(ranef(bundle$models$m_PK)$Runner1B_ID)
)
u_att <- setNames(
  ranef(bundle$models$m_ATT)$Runner1B_ID[["(Intercept)"]],
  rownames(ranef(bundle$models$m_ATT)$Runner1B_ID)
)
v_att <- setNames(
  ranef(bundle$models$m_ATT)$CatcherID[["(Intercept)"]],
  rownames(ranef(bundle$models$m_ATT)$CatcherID)
)
u_sb <- setNames(
  ranef(bundle$models$m_SB)$Runner1B_ID[["(Intercept)"]],
  rownames(ranef(bundle$models$m_SB)$Runner1B_ID)
)
v_sb <- setNames(
  ranef(bundle$models$m_SB)$CatcherID[["(Intercept)"]],
  rownames(ranef(bundle$models$m_SB)$CatcherID)
)

runner_in <- function(ids, lookup) {
  out <- lookup[ids]
  out[is.na(out)] <- 0
  as.numeric(out)
}

stage_int <- as.integer(as.character(leads$dis_stage))
outs_int <- as.integer(leads$outs)
rid <- leads$Runner1B_ID
cid <- leads$CatcherID

re_pk <- runner_in(rid, u_pk)
re_att_u <- runner_in(rid, u_att)
re_att_v <- runner_in(cid, v_att)
re_sb_u <- runner_in(rid, u_sb)
re_sb_v <- runner_in(cid, v_sb)

eta_po <- pick_coef(b_po, "(Intercept)") +
  pick_coef(b_po, "lead_scaled") * leads$lead_scaled +
  pick_coef(b_po, "threat_scaled") * leads$threat_scaled +
  dis_stage_effect(b_po, stage_int) +
  outs_effect(b_po, outs_int)

eta_pk <- pick_coef(b_pk, "(Intercept)") +
  pick_coef(b_pk, "lead_scaled") * leads$lead_scaled +
  pick_coef(b_pk, "threat_scaled") * leads$threat_scaled +
  dis_stage_effect(b_pk, stage_int) +
  re_pk

eta_att <- pick_coef(b_att, "(Intercept)") +
  pick_coef(b_att, "threat_scaled") * leads$threat_scaled +
  pick_coef(b_att, "poptime_scaled") * leads$poptime_scaled +
  pick_coef(b_att, "sprint_scaled") * leads$sprint_scaled +
  dis_stage_effect(b_att, stage_int) +
  outs_effect(b_att, outs_int) +
  re_att_u + re_att_v

eta_sb <- pick_coef(b_sb, "(Intercept)") +
  pick_coef(b_sb, "lead_scaled") * leads$lead_scaled +
  pick_coef(b_sb, "threat_scaled") * leads$threat_scaled +
  pick_coef(b_sb, "poptime_scaled") * leads$poptime_scaled +
  pick_coef(b_sb, "sprint_scaled") * leads$sprint_scaled +
  dis_stage_effect(b_sb, stage_int) +
  outs_effect(b_sb, outs_int) +
  re_sb_u + re_sb_v

p_po <- plogis(eta_po)
p_pkpo <- plogis(eta_pk)
p_att <- plogis(eta_att)
p_sb_att <- plogis(eta_sb)

leads$P_PK <- p_po * p_pkpo
leads$P_SB <- (1 - p_po) * p_att * p_sb_att
leads$P_CS <- (1 - p_po) * p_att * (1 - p_sb_att)

weight_idx <- match(as.integer(leads$outs), bundle$sit_weights$outs)
leads$actual_xRuns <- (bundle$sit_weights$w_SB[weight_idx] * leads$P_SB) +
  (bundle$sit_weights$w_CS[weight_idx] * leads$P_CS) +
  (bundle$sit_weights$w_PK[weight_idx] * leads$P_PK)

contexts <- leads %>%
  transmute(
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed,
    dis_stage = as.integer(as.character(dis_stage)),
    outs = as.integer(outs),
    Runner1B_ID = Runner1B_ID,
    CatcherID = CatcherID
  ) %>%
  distinct() %>%
  mutate(context_id = row_number())

message("Unique optimization contexts: ", nrow(contexts))

to_scaled <- function(x, mu, sig) {
  (x - mu) / sig
}

optimize_one_context <- function(i) {
  ctx <- contexts[i, , drop = FALSE]

  thr_s <- to_scaled(ctx$Threat, scaler$threat_mean, scaler$threat_sd)
  pop_s <- to_scaled(ctx$poptime, scaler$pop_mean, scaler$pop_sd)
  spr_s <- to_scaled(ctx$sprint_speed, scaler$sprint_mean, scaler$sprint_sd)
  d_int <- ctx$dis_stage

  u_pk_i <- if (ctx$Runner1B_ID %in% names(u_pk)) u_pk[[ctx$Runner1B_ID]] else 0
  u_att_i <- if (ctx$Runner1B_ID %in% names(u_att)) u_att[[ctx$Runner1B_ID]] else 0
  u_sb_i <- if (ctx$Runner1B_ID %in% names(u_sb)) u_sb[[ctx$Runner1B_ID]] else 0
  v_att_i <- if (ctx$CatcherID %in% names(v_att)) v_att[[ctx$CatcherID]] else 0
  v_sb_i <- if (ctx$CatcherID %in% names(v_sb)) v_sb[[ctx$CatcherID]] else 0

  # eta(L) = slope * L + intercept
  slope_po <- pick_coef(b_po, "lead_scaled") / scaler$lead_sd
  int_po <- pick_coef(b_po, "(Intercept)") +
    pick_coef(b_po, "threat_scaled") * thr_s +
    dis_stage_effect(b_po, d_int) +
    outs_effect(b_po, ctx$outs) -
    pick_coef(b_po, "lead_scaled") * (scaler$lead_mean / scaler$lead_sd)

  slope_pk <- pick_coef(b_pk, "lead_scaled") / scaler$lead_sd
  int_pk <- pick_coef(b_pk, "(Intercept)") +
    pick_coef(b_pk, "threat_scaled") * thr_s +
    dis_stage_effect(b_pk, d_int) +
    u_pk_i -
    pick_coef(b_pk, "lead_scaled") * (scaler$lead_mean / scaler$lead_sd)

  p_att_const <- plogis(
    pick_coef(b_att, "(Intercept)") +
      pick_coef(b_att, "threat_scaled") * thr_s +
      pick_coef(b_att, "poptime_scaled") * pop_s +
      pick_coef(b_att, "sprint_scaled") * spr_s +
      dis_stage_effect(b_att, d_int) +
      outs_effect(b_att, ctx$outs) +
      u_att_i + v_att_i
  )

  slope_sb <- pick_coef(b_sb, "lead_scaled") / scaler$lead_sd
  int_sb <- pick_coef(b_sb, "(Intercept)") +
    pick_coef(b_sb, "threat_scaled") * thr_s +
    pick_coef(b_sb, "poptime_scaled") * pop_s +
    pick_coef(b_sb, "sprint_scaled") * spr_s +
    dis_stage_effect(b_sb, d_int) +
    outs_effect(b_sb, ctx$outs) +
    u_sb_i + v_sb_i -
    pick_coef(b_sb, "lead_scaled") * (scaler$lead_mean / scaler$lead_sd)

  w <- bundle$sit_weights[bundle$sit_weights$outs == ctx$outs, , drop = FALSE]

  objective <- function(L) {
    p_po_L <- plogis((slope_po * L) + int_po)
    p_pkpo_L <- plogis((slope_pk * L) + int_pk)
    p_sb_att_L <- plogis((slope_sb * L) + int_sb)

    p_pk_L <- p_po_L * p_pkpo_L
    p_sb_L <- (1 - p_po_L) * p_att_const * p_sb_att_L
    p_cs_L <- (1 - p_po_L) * p_att_const * (1 - p_sb_att_L)

    (w$w_SB * p_sb_L) + (w$w_CS * p_cs_L) + (w$w_PK * p_pk_L)
  }

  opt <- optimize(objective, interval = c(0, 90), maximum = TRUE)

  runner_known <- ctx$Runner1B_ID %in% names(u_pk) &&
    ctx$Runner1B_ID %in% names(u_att) &&
    ctx$Runner1B_ID %in% names(u_sb)
  catcher_known <- ctx$CatcherID %in% names(v_att) &&
    ctx$CatcherID %in% names(v_sb)

  data.frame(
    context_id = ctx$context_id,
    optLead_player = round(opt$maximum, 2),
    optxRuns_player = as.numeric(opt$objective),
    runner_known = runner_known,
    catcher_known = catcher_known,
    stringsAsFactors = FALSE
  )
}

num_chunks <- max(1L, n_cores * 4L)
context_chunks <- split(
  seq_len(nrow(contexts)),
  cut(seq_len(nrow(contexts)), breaks = num_chunks, labels = FALSE)
)

t_start <- Sys.time()
opt_chunks <- safe_mclapply(
  context_chunks,
  function(chunk_idx) {
    do.call(rbind, lapply(chunk_idx, optimize_one_context))
  },
  n_cores = n_cores
)
opt_tbl <- dplyr::bind_rows(opt_chunks)
t_end <- Sys.time()

message("Optimization runtime (seconds): ", round(as.numeric(difftime(t_end, t_start, units = "secs")), 2))

contexts <- contexts %>% left_join(opt_tbl, by = "context_id")

leads_full <- leads %>%
  mutate(
    dis_stage_int = as.integer(as.character(dis_stage)),
    outs = as.integer(outs)
  ) %>%
  left_join(
    contexts %>%
      select(
        Threat, poptime, sprint_speed, dis_stage, outs,
        Runner1B_ID, CatcherID,
        optLead_player, optxRuns_player, runner_known, catcher_known
      ),
    by = c(
      "Threat" = "Threat",
      "poptime" = "poptime",
      "sprint_speed" = "sprint_speed",
      "dis_stage_int" = "dis_stage",
      "outs" = "outs",
      "Runner1B_ID" = "Runner1B_ID",
      "CatcherID" = "CatcherID"
    )
  ) %>%
  mutate(
    leadDev_player = PrimaryLead1B - optLead_player,
    rec_player = if_else(optxRuns_player > 0, "Steal", "Stay"),
    used_player_RE = runner_known & catcher_known,
    xRuns_lost = optxRuns_player - actual_xRuns
  ) %>%
  select(-dis_stage_int)

write.csv(leads_full, "data/processed/leadsnew1b_full_v2.csv", row.names = FALSE)

summary_tbl <- leads_full %>%
  summarise(
    n_rows = n(),
    mean_lead_dev = mean(leadDev_player, na.rm = TRUE),
    mean_abs_lead_dev = mean(abs(leadDev_player), na.rm = TRUE),
    mean_actual_xRuns = mean(actual_xRuns, na.rm = TRUE),
    mean_opt_xRuns = mean(optxRuns_player, na.rm = TRUE),
    mean_xRuns_lost = mean(xRuns_lost, na.rm = TRUE)
  )

write.csv(summary_tbl, "data/processed/summary_lead_metrics_v2.csv", row.names = FALSE)

sink("internal/02_optimization_summary.txt")
print(summary_tbl)
cat("\nCore count used:", n_cores, "\n")
cat("Unique contexts:", nrow(contexts), "\n")
sink()

message("02_optimize_leads_parallel.R complete")
