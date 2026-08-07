# LIQIM shared configuration — single tier (top 60% FF-mcap)

CFG <- list(
  MCAP_PCT   = 0.60,       # top 60% FF-mcap
  MIN_PRICE  = 30,         # minimum closing price (INR)
  MIN_DVOL   = 1e7,        # minimum median daily dollar volume (₹1cr)
  ILLIQ_LB   = 1L,         # LIQC lookback (months)
  WINSOR_LO  = 0.01,       # winsorization
  WINSOR_HI  = 0.99,
  TOP_N      = 20L,        # stocks per portfolio
  HOLDING_K  = 1L,         # holding period (months)
  SKIP_MONTH = FALSE,      # no skip
  DRAG       = 0.005,      # 50bps per trade
  MOM_LB     = 12L         # momentum lookback (months)
)
