# CSP data

# install.packages("pak")
# pak::pkg_install("data.table")
# pak::pkg_install("config")
# pak::pkg_install("gt")
# pak::pkg_install("arrow")
# pak::pkg_install("R.utils")

library(data.table)

# Reset
rm(list = ls())
gc()

# Read CSP frame
frame_csp <- fread(
  file = file.path(config::get("dir.data.csp"), "Ietvars_01042025_LU.csv")
) |>
  setnames(tolower) |>
  setkey(adr_kods) |>
  setnames("vc1859_sum", "pers_sk_1859")


# Replace NA with 0
frame_csp[is.na(pers_sk_1859), pers_sk_1859 := 0L]

fwrite(
  x = frame_csp[, .(adr_kods, pers_sk, pers_sk_1859)],
  file = file.path(config::get("dir.data"), "frame_csp.csvy.gz"),
  yaml = TRUE
)
