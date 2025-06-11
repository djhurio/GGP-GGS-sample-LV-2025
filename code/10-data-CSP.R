# CSP data

# install.packages("pak")
# pak::pkg_install("data.table")
# pak::pkg_install("config")
# pak::pkg_install("gt")
# pak::pkg_install("arrow")
# pak::pkg_install("R.utils")

library(data.table)
library(gt)
library(purrr)

# Reset
rm(list = ls())
gc()


# Read CSP frame
frame.csp <- fread(
  file = file.path(config::get("dir.data.csp"), "Ietvars_01042025_LU.csv")
) |>
  setnames(tolower)

frame.csp

# Replace NA with 0
frame.csp[is.na(vc1859_sum), vc1859_sum := 0L]

fwrite(
  x = frame.csp,
  file = file.path(config::get("dir.data"), "frame_csp.ycsv.gz"),
  yaml = TRUE
)
