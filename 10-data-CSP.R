# Test CSP data

# install.packages("pak")

# pak::pkg_install("data.table")
# pak::pkg_install("config")
# pak::pkg_install("gt")

library(data.table)
library(gt)
library(purrr)

frame.csp <- fread(
  file = file.path(config::get("dir.data.csp"), "Ietvars_01042025_LU.csv")
) |> setnames(tolower)

frame.csp

# Replace NA with 0
frame.csp[is.na(vc1859_sum), vc1859_sum := 0L]

# Trim
frame.csp[, pers_kopa_trim := fifelse(pers_sk    < 10L, pers_sk,    10L)]
frame.csp[, pers_1859_trim := fifelse(vc1859_sum < 10L, vc1859_sum, 10L)]

frame.csp[, pers_kopa_trim := factor(pers_kopa_trim, 0:10, c(0:9, "10+"))]
frame.csp[, pers_1859_trim := factor(pers_1859_trim, 0:10, c(0:9, "10+"))]

tab.frame.csp <- frame.csp[, .(n = .N), keyby = .(pers_kopa_trim, pers_1859_trim)]
tab.frame.csp[, p := prop.table(n)]

tab.frame.csp <- rbindlist(
  list(
    tab.frame.csp,
    tab.frame.csp[, map(.SD, sum), .SDcols = c("n", "p"), keyby = .(pers_kopa_trim)],
    tab.frame.csp[, map(.SD, sum), .SDcols = c("n", "p"), keyby = .(pers_1859_trim)],
    tab.frame.csp[, map(.SD, sum), .SDcols = c("n", "p")]
  ),
  fill = TRUE
)

tab.frame.csp[is.na(pers_kopa_trim), pers_kopa_trim := "Kopā"]
tab.frame.csp[is.na(pers_1859_trim), pers_1859_trim := "Kopā"]

tab.frame.csp.n <- dcast.data.table(
  tab.frame.csp,
  pers_kopa_trim ~ pers_1859_trim,
  value.var = "n",
  fill = 0L
)

tab.frame.csp.p <- dcast.data.table(
  tab.frame.csp,
  pers_kopa_trim ~ pers_1859_trim,
  value.var = "p",
  fill = 0
)

gt(tab.frame.csp.n) |>
  tab_header("Privāto mājokļu skaits") |>
  tab_spanner(
    label = "Mājokļa personu skaits vecumā 18-59",
    columns = matches("\\d")
  ) |>
  cols_label(
    pers_kopa_trim = "Mājokļa personu skaits"
  ) |>
  cols_width(
    everything() ~ px(70)
  ) |>
  fmt_number(decimals = 0)

gt(tab.frame.csp.p) |>
  tab_header("Privāto mājokļu īpatsvars") |>
  tab_spanner(
    label = "Mājokļa personu skaits vecumā 18-59",
    columns = matches("\\d")
  ) |>
  cols_label(
    pers_kopa_trim = "Mājokļa personu skaits"
  ) |>
  cols_width(
    everything() ~ px(80)
  ) |>
  fmt_number(decimals = 3)


# frame.csp[, .N, keyby = .(pers_sk)] |> View()
# frame.csp[order(pers_sk)]

frame.csp[, .N, keyby = .(vc1859_sum)] |> View()

frame.csp[, .N]
frame.csp[, .N, keyby = .(vc1859_sum)][, P := prop.table(N)][]
