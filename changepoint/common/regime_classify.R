# regime_classify.R
# Classify an excess-return time series as STABLE or UNSTABLE
# using a majority vote of changepoint-detection methods, and return
# a date-level regime tibble.
#
# Regime = majority vote across HMM, LR (12), CPM (16), and Barry-Hartigan.
# Each method partitions the series into segments via its detected
# changepoints; segments with above-average |return| are UNSTABLE.
# HMM labels its higher-variance state as UNSTABLE.

required_pkgs <- c(
  "xts", "changepoint", "cpm", "depmixS4", "bcp", "tibble"
)

load_packages <- function() {
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing)
  }
  invisible(lapply(required_pkgs, library, character.only = TRUE))
}

cp_dates <- function(idxs, dates) {
  if (length(idxs) == 0) return(as.Date(character()))
  dates[idxs]
}

# ---------------------------------------------------------------------------
# Recursive binary segmentation (CPM batch)
# ---------------------------------------------------------------------------
recursive_batch <- function(x, cpmType, alpha = 0.05, minseg = 20) {
  cps <- integer()

  recurse <- function(start, end) {
    if ((end - start + 1) < 2 * minseg)
      return()

    seg <- x[start:end]

    fit <- cpm::detectChangePointBatch(
      seg,
      cpmType = cpmType,
      alpha    = alpha
    )

    if (!fit$changeDetected)
      return()

    cp_local  <- fit$changePoint
    cp_global <- start + cp_local - 1

    cps <<- c(cps, cp_global)

    recurse(start, cp_global - 1)
    recurse(cp_global + 1, end)
  }

  recurse(1, length(x))
  sort(unique(cps))
}

# ---------------------------------------------------------------------------
# Main entry point
#
# Returns a list (same shape as analyze_baseline):
#
#   $regime_tbl   : tibble(Date, Regime, N_Unstable, N_Total)
#                   Regime = majority vote of 30 methods per date
#   $HMM          : list(optimal_states, cp_dates)
#   $LR           : list of Date vectors (12 methods)
#   $CPM          : list of Date vectors (16 methods)
#   $BarryHartigan: Date vector
#   $summary      : tibble(Method, N_ChangePoints)
# ---------------------------------------------------------------------------
classify_regime <- function(excess_returns_xts, verbose = FALSE) {

  load_packages()

  stopifnot(xts::is.xts(excess_returns_xts))
  x     <- as.numeric(na.omit(excess_returns_xts[, 1]))
  dates <- zoo::index(na.omit(excess_returns_xts))

  # max changepoints for methods that need a Q parameter
  max_cp <- min(floor(length(x) / 2) - 1, 1000)

  results <- list()
  summary <- tibble::tibble(Method = character(), N_ChangePoints = integer())

  # ---- HMM (depmixS4) ----
  bics <- rep(Inf, 8)
  fits <- vector("list", 8)
  hmm_data <- tibble::tibble(y = x)

  for (k in 1:8) {
    if (verbose) cat("  HMM k =", k, "...\n")

    fit <- try({
      mod <- depmixS4::depmix(
        response = y ~ 1,
        data     = hmm_data,
        family   = gaussian(),
        nstates  = k,
        ntimes   = length(x)
      )
      depmixS4::fit(mod, verbose = FALSE)
    }, silent = TRUE)

    if (!inherits(fit, "try-error")) {
      fits[[k]] <- fit
      bic <- try(BIC(fit), silent = TRUE)
      if (!inherits(bic, "try-error") && is.finite(bic)) {
        bics[k] <- bic
      }
    }
  }

  best   <- which.min(bics)
  post   <- depmixS4::posterior(fits[[best]])
  hmm_cp <- which(diff(post$state) != 0) + 1

  results$HMM <- list(
    optimal_states = best,
    cp_dates       = cp_dates(hmm_cp, dates)
  )

  summary <- rbind(summary, tibble::tibble(
    Method         = "HMM",
    N_ChangePoints = length(hmm_cp)
  ))

  # ---- LR family (changepoint) ----
  lr_specs <- expand.grid(
    target = c("mean", "var", "meanvar"),
    method = c("AMOC", "BinSeg", "PELT", "SegNeigh"),
    stringsAsFactors = FALSE
  )

  results$LR <- list()

  for (i in seq_len(nrow(lr_specs))) {
    tgt  <- lr_specs$target[i]
    meth <- lr_specs$method[i]

    fit <- switch(
      tgt,
      mean    = changepoint::cpt.mean(x,    method = meth, penalty = "BIC", Q = max_cp),
      var     = changepoint::cpt.var(x,     method = meth, penalty = "BIC", Q = max_cp),
      meanvar = changepoint::cpt.meanvar(x, method = meth, penalty = "BIC", Q = max_cp)
    )

    cps   <- changepoint::cpts(fit)
    nm    <- paste(meth, tgt, sep = "_")

    results$LR[[nm]] <- cp_dates(cps, dates)

    summary <- rbind(summary, tibble::tibble(
      Method         = nm,
      N_ChangePoints = length(cps)
    ))
  }

  # ---- CPM (offline + online) ----
  cpm_types <- c(
    "Student", "Bartlett", "GLR", "Mann-Whitney",
    "Mood", "Lepage", "Kolmogorov-Smirnov", "Cramer-von-Mises"
  )

  results$CPM <- list()

  for (tp in cpm_types) {
    # Offline
    off_cp <- recursive_batch(x, tp, alpha = 0.05)
    results$CPM[[paste0(tp, "_offline")]] <- cp_dates(off_cp, dates)

    summary <- rbind(summary, tibble::tibble(
      Method         = paste0(tp, "_offline"),
      N_ChangePoints = length(off_cp)
    ))

    # Online
    on    <- cpm::processStream(x, cpmType = tp, ARL0 = 500, startup = 20)
    on_cp <- on$changePoints
    results$CPM[[paste0(tp, "_online")]] <- cp_dates(on_cp, dates)

    summary <- rbind(summary, tibble::tibble(
      Method         = paste0(tp, "_online"),
      N_ChangePoints = length(on_cp)
    ))
  }

  # ---- Barry-Hartigan (bcp) ----
  bh    <- bcp::bcp(x, p0 = 0.2, w0 = 0.2)
  bh_cp <- which(bh$posterior.prob > 0.5)

  results$BarryHartigan <- cp_dates(bh_cp, dates)

  summary <- rbind(summary, tibble::tibble(
    Method         = "BarryHartigan",
    N_ChangePoints = length(bh_cp)
  ))

  results$summary <- summary

  # ---- Consolidated date-level regime tibble (majority vote) ----
  # Each method casts a STABLE / UNSTABLE vote for every date.
  # Final regime is the majority vote across HMM, LR, CPM, and Barry-Hartigan.

  n <- length(x)
  overall_abs_mean <- mean(abs(x), na.rm = TRUE)

  # Helper: assign a label to every segment defined by cp indices.
  # Segments whose absolute mean exceeds the overall absolute mean
  # get UNSTABLE; otherwise STABLE.
  segment_labels <- function(cp_indices, n, x, threshold) {
    if (length(cp_indices) == 0) {
      # No changepoints → one segment, label it STABLE
      return(rep("STABLE", n))
    }
    boundaries <- sort(unique(c(0, cp_indices, n)))
    labels <- character(n)
    for (i in seq_len(length(boundaries) - 1)) {
      seg_start <- boundaries[i] + 1
      seg_end   <- boundaries[i + 1]
      if (seg_end < seg_start) next
      seg_abs_mean <- mean(abs(x[seg_start:seg_end]), na.rm = TRUE)
      lbl <- if (seg_abs_mean > threshold) "UNSTABLE" else "STABLE"
      labels[seg_start:seg_end] <- lbl
    }
    labels
  }

  # 1. HMM vote
  hmm_states <- post$state
  state_vars <- sapply(1:best, function(s) var(x[hmm_states == s], na.rm = TRUE))
  unstable_state <- which.max(state_vars)
  hmm_vote <- ifelse(hmm_states == unstable_state, "UNSTABLE", "STABLE")

  # 2. LR votes (12 methods)
  lr_votes <- lapply(results$LR, function(cp_dt) {
    cp_idx <- if (length(cp_dt) > 0) which(dates %in% cp_dt) else integer()
    segment_labels(cp_idx, n, x, overall_abs_mean)
  })

  # 3. CPM votes (16 methods)
  cpm_votes <- lapply(results$CPM, function(cp_dt) {
    cp_idx <- if (length(cp_dt) > 0) which(dates %in% cp_dt) else integer()
    segment_labels(cp_idx, n, x, overall_abs_mean)
  })

  # 4. Barry-Hartigan vote
  bh_cp_idx <- if (length(results$BarryHartigan) > 0) {
    which(dates %in% results$BarryHartigan)
  } else {
    integer()
  }
  bh_vote <- segment_labels(bh_cp_idx, n, x, overall_abs_mean)

  # Build voting matrix: rows = dates, cols = methods
  all_votes <- c(list(HMM = hmm_vote), lr_votes, cpm_votes, list(BH = bh_vote))
  vote_mat <- do.call(cbind, all_votes)  # character matrix, n × 30
  n_methods <- ncol(vote_mat)
  n_unstable <- rowSums(vote_mat == "UNSTABLE")

  regime_final <- ifelse(n_unstable >= n_methods / 2, "UNSTABLE", "STABLE")

  results$regime_tbl <- tibble::tibble(
    Date       = dates,
    Regime     = regime_final,
    N_Unstable = n_unstable,
    N_Total    = n_methods
  )

  return(results)
}
