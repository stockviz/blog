# SKEW shared configuration — top 60% FF-mcap momentum + skewness + LIQC
# Used by build.R and momentum.R

CFG <- list(
  # Universe
  MCAP_PCT   = 0.60,       # top 60% FF-mcap
  MIN_PRICE  = 30,         # minimum closing price (INR)
  MIN_DVOL   = 1e7,        # minimum median daily dollar volume (₹1cr)

  # LIQC
  ILLIQ_LB   = 1L,         # LIQC lookback (months)
  WINSOR_LO  = 0.01,       # winsorization
  WINSOR_HI  = 0.99,

  # Momentum
  MOM_LB     = 12L,        # momentum lookback (months)
  TOP_N      = 20L,        # stocks per baseline momentum portfolio
  HOLDING_K  = 1L,         # holding period (months)
  SKIP_MONTH = FALSE,      # no skip month
  DRAG       = 0.005,      # 50bps per trade

  # Skewness
  MIN_DAILY  = 15L         # min daily obs per month for RS/RV calc
)
