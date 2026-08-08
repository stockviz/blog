# ============================================================================
# backtest-common.R — Shared portfolio construction + chart helpers
# ============================================================================

compoundReturn <- function(rets) prod(1 + rets, na.rm = TRUE) - 1

computeMetrics <- function(rets) {
  if (nrow(rets) < 60) return(c(CAGR = NA_real_, Vol = NA_real_, Sharpe = NA_real_,
                                 MaxDD = NA_real_, Calmar = NA_real_))
  annRet <- Return.annualized(rets)[1, 1]
  annVol <- sd(coredata(rets), na.rm = TRUE) * sqrt(252)
  sharpe <- tryCatch(SharpeRatio.annualized(rets)[1, 1], error = function(e) NA_real_)
  maxDD  <- maxDrawdown(rets)
  calmar <- if (!is.na(maxDD) && maxDD > 0) annRet / maxDD else NA_real_
  c(CAGR = annRet, Vol = annVol, Sharpe = sharpe, MaxDD = maxDD, Calmar = calmar)
}

lighten <- function(hex, amount = 0.85) {
  rgb <- col2rgb(hex) / 255
  rgb <- rgb + (1 - rgb) * amount
  rgb(rgb[1], rgb[2], rgb[3])
}

# ── Single-stock holding-period returns ─────────────────────────────────────

stockReturns <- function(df, fromDate, toDate) {
  if (is.null(df)) return(NULL)
  preRows <- df[df$date_stamp <= fromDate, , drop = FALSE]
  if (nrow(preRows) == 0) return(NULL)
  baseline <- preRows[nrow(preRows), ]
  holdRows <- df[df$date_stamp >= fromDate & df$date_stamp <= toDate, , drop = FALSE]
  if (nrow(holdRows) < 5) return(NULL)
  sub <- rbind(baseline, holdRows)
  sub <- sub[order(sub$date_stamp), ]
  pXts <- xts(sub$c, sub$date_stamp)
  rets <- na.omit(dailyReturn(pXts, type = "arithmetic"))
  if (nrow(rets) < 5) return(NULL)
  xts(coredata(rets), as.Date(index(rets)))
}

# ── Build one holding-period equal-weight strand ────────────────────────────

buildStrand <- function(stocks, priceVol, holdStart, holdingEnd,
                         drag = 0, nStocks = 20L) {
  xList <- vector("list", length(stocks)); vc <- 0L
  for (j in seq_along(stocks)) {
    rets <- stockReturns(priceVol[[stocks[j]]], holdStart, holdingEnd)
    if (is.null(rets)) next
    vc <- vc + 1L; xList[[vc]] <- rets
  }
  if (vc == 0) return(NULL)
  xList <- xList[1:vc]
  mat <- do.call(merge.xts, xList)
  vec <- rowMeans(coredata(mat), na.rm = TRUE)
  kd <- !is.na(vec); vec <- vec[kd]; dates <- index(mat)[kd]
  if (length(vec) < 5) return(NULL)
  if (drag > 0) vec[1] <- vec[1] - drag
  xts(vec, dates)
}

# ── Full portfolio construction loop ────────────────────────────────────────

makePortfolio <- function(pickStocks, liqcCache, universeCache, monthEnds,
                           priceVol, label, topN = 20L, holdK = 1L,
                           skip = FALSE, drag = 0.005,
                           warmupCache = liqcCache, ...) {
  warmupMI <- which(!sapply(warmupCache, is.null))[1]
  if (is.na(warmupMI)) {
    cat(sprintf("  %s: no warmup data\n", label))
    return(NULL)
  }
  strands <- list(); nTraded <- 0L; nSkipped <- 0L
  for (mi in seq(warmupMI, length(monthEnds))) {
    sigDate <- monthEnds[mi]
    if (mi >= length(monthEnds)) break

    stocks <- pickStocks(mi, sigDate, liqcCache, universeCache, topN, ...)
    if (is.null(stocks) || length(stocks) < topN) {
      nSkipped <- nSkipped + 1L; next
    }

    holdStart <- if (skip) monthEnds[mi + 1L] + 1 else sigDate + 1
    holdingEndIdx <- min(mi + holdK + if (skip) 1L else 0L, length(monthEnds))
    holdingEnd <- monthEnds[holdingEndIdx]

    s <- buildStrand(stocks, priceVol, holdStart, holdingEnd, drag, topN)
    if (!is.null(s)) {
      strands[[length(strands) + 1L]] <- s; nTraded <- nTraded + 1L
    } else {
      nSkipped <- nSkipped + 1L
    }
  }
  if (length(strands) == 0) return(NULL)
  mat <- do.call(merge.xts, strands)
  rets <- na.omit(xts(rowMeans(coredata(mat), na.rm = TRUE), index(mat)))
  colnames(rets) <- label
  cat(sprintf("  %s: %d traded, %d skipped, %d days\n",
      label, nTraded, nSkipped, nrow(rets)))
  rets
}

# ── Momentum helpers ────────────────────────────────────────────────────────

buildMomentumCache <- function(monthEnds, universeCache, priceVol, momLb = 12L) {
  momCache <- vector("list", length(monthEnds))
  for (mi in seq(momLb + 1L, length(monthEnds))) {
    sigDate  <- monthEnds[mi]
    momEnd   <- sigDate
    momStart <- momEnd %m-% months(momLb)

    syms <- universeCache[[mi]]
    if (is.null(syms) || length(syms) == 0) next

    momRets <- vapply(syms, function(tkr) {
      df <- priceVol[[tkr]]
      if (is.null(df) || nrow(df) < 260) return(NA_real_)
      sub <- df[df$date_stamp >= momStart & df$date_stamp <= momEnd, , drop = FALSE]
      if (nrow(sub) < 230) return(NA_real_)
      as.numeric(tail(sub$c, 1)) / as.numeric(sub$c[1]) - 1
    }, double(1))

    names(momRets) <- syms
    momRets <- momRets[!is.na(momRets)]
    if (length(momRets) > 0) momCache[[mi]] <- sort(momRets, decreasing = TRUE)
  }
  cat(sprintf("  Momentum: %d months cached\n",
      sum(!sapply(momCache, is.null))))
  momCache
}

pickMomentum <- function(momCache, excludeCache, topN = 20L, exclude = TRUE) {
  function(mi, sigDate, liqcCache, universeCache, topN, ...) {
    mr <- momCache[[mi]]
    if (is.null(mr) || length(mr) < topN) return(NULL)

    u <- universeCache[[mi]]
    if (is.null(u) || length(u) < topN) return(NULL)

    mf <- mr[names(mr) %in% u]
    if (length(mf) < topN) return(NULL)

    if (exclude && !is.null(excludeCache)) {
      excl <- excludeCache[[mi]]
      if (!is.null(excl) && length(excl) > 0)
        mf <- mf[!names(mf) %in% excl]
    }
    if (length(mf) < topN) return(NULL)
    names(mf)[1:topN]
  }
}

# ── Charts ──────────────────────────────────────────────────────────────────

makeAnnualChart <- function(combined, title, filePath, width = 16, height = 8) {
  annual <- apply.yearly(combined, Return.cumulative)
  annualDf <- fortify(annual, melt = TRUE)
  names(annualDf) <- c("Year", "Strategy", "Return")
  annualDf$Year <- as.numeric(format(annualDf$Year, "%Y"))
  p <- ggplot(annualDf, aes(x = factor(Year), y = Return, fill = Strategy)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_viridis_d(end = 0.9) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       breaks = seq(-1, 2, 0.1)) +
    labs(title = title,
         subtitle = sprintf("%s → %s", first(index(combined)), last(index(combined))),
         caption = "@StockViz", x = NULL, y = NULL) +
    theme_economist() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(size = 8, color = "grey50", hjust = 1),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "bottom")
  ggsave(filePath, p, width = width, height = height, units = "in")
  cat(sprintf("  %s\n", basename(filePath)))
}

makeCumretChart <- function(combined, title, filePath) {
  toPlot <- combined; toPlot[1, ] <- 0.0
  sr <- SharpeRatio.annualized(toPlot)
  srVals <- paste(sapply(colnames(toPlot), function(nm)
    sprintf("%s=%.2f", nm, sr[1, nm])), collapse = ", ")
  Common.PlotCumReturns(toPlot, title,
    sprintf("%s → %s  |  SR: %s", first(index(toPlot)), last(index(toPlot)), srVals),
    filePath, NULL)
  cat(sprintf("  %s\n", basename(filePath)))
}

makeGtTable <- function(fm, title, filePath, reportPath) {
  tblDf <- as.data.frame(t(fm))
  tblDf$Strategy <- rownames(tblDf); rownames(tblDf) <- NULL
  tickerColors <- setNames(viridis_pal(end = 0.9)(nrow(tblDf)), tblDf$Strategy)
  gtTbl <- tblDf |> gt() |>
    tab_header(title = title, subtitle = "") |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())
  for (s in tblDf$Strategy) {
    lc <- lighten(tickerColors[s])
    gtTbl <- gtTbl |>
      tab_style(style = cell_fill(color = lc),
                locations = cells_body(rows = Strategy == s)) |>
      tab_style(style = list(cell_fill(color = tickerColors[s]),
                             cell_text(color = "white", weight = "bold")),
                locations = cells_body(columns = Strategy, rows = Strategy == s))
  }
  htmlPath <- sub(".png", ".html", filePath)
  gtsave(gtTbl, htmlPath)
  webshot2::webshot(paste0("file://", htmlPath), filePath,
                    selector = "table", expand = c(10, 10, 10, 10))
  cat(sprintf("  %s\n", basename(filePath)))
}

makeMonthlyCsv <- function(combined, label, reportPath) {
  monthlySeries <- lapply(colnames(combined), function(cn) {
    x <- combined[, cn]
    mon <- apply.monthly(x, function(r) compoundReturn(r))
    colnames(mon) <- cn; mon
  })
  monMerged <- na.omit(do.call(merge.xts, monthlySeries))
  monDf <- data.frame(date = index(monMerged), coredata(monMerged),
                      row.names = NULL)
  csv <- sprintf("%s/monthly_%s.csv", reportPath, label)
  write.csv(monDf, csv, row.names = FALSE)
  cat(sprintf("  %s (%d months)\n", basename(csv), nrow(monDf)))
}
