for %%x in ("NIFTY 50 TR", "NIFTY MIDCAP 150 TR") do (
 for %%y in (20, 50, 100, 200) do (
   rscript script.R %%x %%y
 )
)
