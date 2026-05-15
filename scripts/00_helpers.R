suppressPackageStartupMessages({
  if (dir.exists("r_lib")) {
    .libPaths(c(normalizePath("r_lib"), .libPaths()))
  }
})

detect_n_cores <- function(default_cores = 12L, reserve_cores = 4L) {
  env_cores <- suppressWarnings(as.integer(Sys.getenv("N_CORES", "")))

  detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (is.na(detected) || detected < 1L) {
    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }

  if (!is.na(env_cores) && env_cores > 0L) {
    return(env_cores)
  }
  if (!is.na(detected) && detected > 0L) {
    usable <- as.integer(detected) - as.integer(reserve_cores)
    if (usable < 1L) {
      usable <- 1L
    }
    return(as.integer(usable))
  }
  as.integer(default_cores)
}

ensure_dirs <- function(paths) {
  for (path in paths) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

safe_mclapply <- function(X, FUN, n_cores, ...) {
  if (.Platform$OS.type == "windows") {
    lapply(X, FUN, ...)
  } else {
    parallel::mclapply(X, FUN, ..., mc.cores = n_cores)
  }
}

build_scaled_newdata <- function(bundle, lead_ft, threat, poptime, sprint_speed, dis_state_val, outs_val) {
  data.frame(
    lead_scaled = (lead_ft - bundle$scaler$lead_mean) / bundle$scaler$lead_sd,
    threat_scaled = (threat - bundle$scaler$threat_mean) / bundle$scaler$threat_sd,
    poptime_scaled = (poptime - bundle$scaler$pop_mean) / bundle$scaler$pop_sd,
    sprint_scaled = (sprint_speed - bundle$scaler$sprint_mean) / bundle$scaler$sprint_sd,
    dis_state = factor(dis_state_val, levels = c(0, 1, 2)),
    outs_f = factor(outs_val, levels = c(0, 1, 2))
  )
}

get_situational_weights <- function(bundle, outs_val) {
  weights <- bundle$sit_weights[bundle$sit_weights$outs == outs_val, , drop = FALSE]
  if (nrow(weights) != 1L) {
    stop("Could not resolve situational weights for outs = ", outs_val)
  }
  weights
}

predict_outcome_probs <- function(
  bundle,
  lead_ft,
  threat,
  poptime,
  sprint_speed,
  dis_state_val,
  outs_val,
  runner_id = NA,
  catcher_id = NA,
  use_re = TRUE
) {
  nd_base <- build_scaled_newdata(
    bundle = bundle,
    lead_ft = lead_ft,
    threat = threat,
    poptime = poptime,
    sprint_speed = sprint_speed,
    dis_state_val = dis_state_val,
    outs_val = outs_val
  )

  # Stage 1 (PO) has no random effects in the final pipeline.
  p_po <- as.numeric(stats::predict(bundle$models$m_PO, nd_base, type = "response"))

  nd_pk <- nd_base
  nd_pk$Runner1B_ID <- factor(as.character(runner_id))

  nd_att <- nd_base
  nd_att$Runner1B_ID <- factor(as.character(runner_id))
  nd_att$CatcherID <- factor(as.character(catcher_id))

  nd_sb <- nd_base
  nd_sb$Runner1B_ID <- factor(as.character(runner_id))
  nd_sb$CatcherID <- factor(as.character(catcher_id))

  if (use_re) {
    p_pkpo <- as.numeric(stats::predict(
      bundle$models$m_PK,
      nd_pk,
      type = "response",
      re.form = NULL,
      allow.new.levels = TRUE
    ))
    p_att <- as.numeric(stats::predict(
      bundle$models$m_ATT,
      nd_att,
      type = "response",
      re.form = NULL,
      allow.new.levels = TRUE
    ))
    p_sb_att <- as.numeric(stats::predict(
      bundle$models$m_SB,
      nd_sb,
      type = "response",
      re.form = NULL,
      allow.new.levels = TRUE
    ))
  } else {
    p_pkpo <- as.numeric(stats::predict(
      bundle$models$m_PK,
      nd_pk,
      type = "response",
      re.form = NA,
      allow.new.levels = TRUE
    ))
    p_att <- as.numeric(stats::predict(
      bundle$models$m_ATT,
      nd_att,
      type = "response",
      re.form = NA,
      allow.new.levels = TRUE
    ))
    p_sb_att <- as.numeric(stats::predict(
      bundle$models$m_SB,
      nd_sb,
      type = "response",
      re.form = NA,
      allow.new.levels = TRUE
    ))
  }

  p_pk <- p_po * p_pkpo
  p_sb <- (1 - p_po) * p_att * p_sb_att
  p_cs <- (1 - p_po) * p_att * (1 - p_sb_att)

  w <- get_situational_weights(bundle, outs_val)
  x_runs <- (w$w_SB * p_sb) + (w$w_CS * p_cs) + (w$w_PK * p_pk)

  data.frame(
    P_PK = p_pk,
    P_SB = p_sb,
    P_CS = p_cs,
    xRuns = x_runs
  )
}
