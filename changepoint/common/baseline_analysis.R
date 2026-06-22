
# baseline_analysis.R
# Skeleton implementation of baseline analysis from the paper

required_pkgs <- c(
  "xts","changepoint","cpm","depmixS4","bcp"
)

load_packages <- function() {
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly=TRUE)]
  if(length(missing)) {
    install.packages(missing)
  }
  invisible(lapply(required_pkgs, library, character.only=TRUE))
}

cp_dates <- function(idxs, dates) {
  if(length(idxs)==0) return(as.Date(character()))
  dates[idxs]
}

recursive_batch <- function(x,
                            cpmType,
                            alpha = 0.05,
                            minseg = 20) {
  
  cps <- integer()
  
  recurse <- function(start, end) {
    
    if ((end - start + 1) < 2 * minseg)
      return()
    
    seg <- x[start:end]
    
    fit <- cpm::detectChangePointBatch(
      seg,
      cpmType = cpmType,
      alpha = alpha
    )
    
    if (!fit$changeDetected)
      return()
    
    cp_local <- fit$changePoint
    
    cp_global <- start + cp_local - 1
    
    cps <<- c(cps, cp_global)
    
    recurse(start, cp_global - 1)
    recurse(cp_global + 1, end)
  }
  
  recurse(1, length(x))
  
  sort(unique(cps))
}

analyze_baseline <- function(excess_returns_xts) {

  load_packages()

  stopifnot(xts::is.xts(excess_returns_xts))
  x <- as.numeric(na.omit(excess_returns_xts[,1]))
  dates <- zoo::index(na.omit(excess_returns_xts))

  results <- list()
  summary <- data.frame(Method=character(), N_ChangePoints=integer())

  ## HMM
  maxQ <- floor(length(x) / 2) - 1
  Q <- min(1000, maxQ)
  
  bics <- rep(NA, 8)
  fits <- vector("list", 8)
  
  hmm_data <- data.frame(y = x)
  
  bics <- rep(Inf, 8)
  fits <- vector("list", 8)
  
  for (k in 1:8) {
    
    cat("Trying HMM with", k, "states...\n")
    
    fit <- try({
      
      mod <- depmixS4::depmix(
        response = y ~ 1,
        data = hmm_data,
        family = gaussian(),
        nstates = k,
        ntimes = length(x)
      )
      
      depmixS4::fit(
        mod,
        verbose = FALSE
      )
      
    }, silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      
      fits[[k]] <- fit
      
      bic <- try(BIC(fit), silent = TRUE)
      
      if (!inherits(bic, "try-error") &&
          is.finite(bic)) {
        
        bics[k] <- bic
      }
    }
  }
  
  best <- which.min(bics)
  
  post <- depmixS4::posterior(fits[[best]])
  
  states <- post$state
  
  hmm_cp <- which(diff(states) != 0) + 1

  results$HMM <- list(
    optimal_states=best,
    cp_dates=cp_dates(hmm_cp, dates)
  )

  summary <- rbind(summary,
                   data.frame(Method="HMM",
                              N_ChangePoints=length(hmm_cp)))

  ## LR family
  lr_specs <- expand.grid(
    target=c("mean","var","meanvar"),
    method=c("AMOC","BinSeg","PELT","SegNeigh"),
    stringsAsFactors=FALSE
  )

  results$LR <- list()

  for(i in seq_len(nrow(lr_specs))) {

    tgt <- lr_specs$target[i]
    meth <- lr_specs$method[i]

    fit <- switch(
      tgt,
      mean=changepoint::cpt.mean(
        x, method=meth, penalty="BIC", Q=Q),
      var=changepoint::cpt.var(
        x, method=meth, penalty="BIC", Q=Q),
      meanvar=changepoint::cpt.meanvar(
        x, method=meth, penalty="BIC", Q=Q)
    )

    cps <- changepoint::cpts(fit)

    nm <- paste(meth,tgt,sep="_")

    results$LR[[nm]] <- cp_dates(cps, dates)

    summary <- rbind(summary,
                     data.frame(Method=nm,
                                N_ChangePoints=length(cps)))
  }

  ## CPM offline/online
  cpm_types <- c(
    "Student",
    "Bartlett",
    "GLR",
    "Mann-Whitney",
    "Mood",
    "Lepage",
    "Kolmogorov-Smirnov",
    "Cramer-von-Mises"
  )
  
  results$CPM <- list()
  
  for (tp in cpm_types) {
    
    ## Offline CPM
    off_cp <- recursive_batch(
      x,
      tp,
      alpha = 0.05
    )
    
    results$CPM[[paste0(tp, "_offline")]] <-
      cp_dates(off_cp, dates)
    
    summary <- rbind(
      summary,
      data.frame(
        Method = paste0(tp, "_offline"),
        N_ChangePoints = length(off_cp)
      )
    )
    
    ## Online CPM
    on <- cpm::processStream(
      x,
      cpmType = tp,
      ARL0 = 500,
      startup = 20
    )
    
    on_cp <- on$changePoints
    
    results$CPM[[paste0(tp, "_online")]] <-
      cp_dates(on_cp, dates)
    
    summary <- rbind(
      summary,
      data.frame(
        Method = paste0(tp, "_online"),
        N_ChangePoints = length(on_cp)
      )
    )
  }

  ## Barry-Hartigan
  bh <- bcp::bcp(x,p0=0.2,w0=0.2)
  bh_cp <- which(bh$posterior.prob > 0.5)

  results$BarryHartigan <- cp_dates(bh_cp, dates)

  summary <- rbind(summary,
                   data.frame(Method="BarryHartigan",
                              N_ChangePoints=length(bh_cp)))

  ## Placeholder Adams-MacKay
  results$AdamsMacKay <- list(
    maxprob=as.Date(character()),
    thrshld50=as.Date(character()),
    maxCP=as.Date(character()),
    note="Implement using BOCPD posterior."
  )

  summary <- rbind(summary,
                   data.frame(Method=c(
                     "AM_maxprob",
                     "AM_thrshld50",
                     "AM_maxCP"),
                     N_ChangePoints=NA))

  results$summary <- summary

  return(results)
}

summarize_cross_section <- function(stock_list){

  out <- lapply(stock_list, function(x){
    analyze_baseline(x)$summary
  })

  methods <- unique(out[[1]]$Method)

  res <- do.call(rbind, lapply(methods, function(m){

    vals <- sapply(out, function(df){
      df$N_ChangePoints[df$Method==m]
    })

    vals <- vals[!is.na(vals)]

    data.frame(
      Method=m,
      Min=min(vals),
      Q1=quantile(vals,0.25),
      Median=median(vals),
      Mean=mean(vals),
      Q3=quantile(vals,0.75),
      Max=max(vals),
      StdDev=sd(vals),
      CV=sd(vals)/mean(vals)
    )
  }))

  rownames(res) <- NULL
  res
}
