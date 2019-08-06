library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
render("drawdowns.Rmd", output_file="drawdowns.html")