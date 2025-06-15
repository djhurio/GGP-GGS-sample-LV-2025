# Sampling

# Izlasei vajadzētu atlasīt 25 000 adrešu.
# Ja tomēr izrādīsies, ka atbildētība ir ļoti bēdīga,
# mums vajadzētu vēl 5000 rezerves adreses, ko varam izmantot.

# Packages
library(purrr)
library(data.table)
library(ggplot2)
library(glue)
library(sampling)

# Reset
rm(list = ls())
gc()

# Functions
round_preserve_sum <- function(x) {
  stopifnot(is.numeric(x), all(is.finite(x)))

  floored <- floor(x)
  remainder <- round(sum(x)) - sum(floored)

  # Compute fractional parts and use original index for tie-breaking
  fractional <- x - floored

  # Rank the fractional parts
  # for equal fractional parts smaller values gets rounded up
  indices <- order(-fractional, x)[1:remainder]

  floored[indices] <- floored[indices] + 1
  return(as.integer(floored))
}

# round_preserve_sum(c(1.2, 2.8, 3.5, 4.5))
# round_preserve_sum(c(1.2, 2.8, 4.5, 3.5))
# round_preserve_sum(c(4.5, 3.5, 1.2, 2.8))

# Load
frame_majo <- fread(
  file = file.path(config::get("dir.data"), "frame_majo_ord.csvy.gz"),
  yaml = TRUE,
  key = c("str_expl", "str_impl")
)
key(frame_majo)

# Noņem dubultās pēdiņas
frame_majo[, adrese := gsub('""', '"', adrese)]


# Sample allocation
tab_sam_alloc <- frame_majo[,
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk_1859)
  ),
  keyby = .(str_expl, pil_lauk_name, NUTS3_name)
]

# Total (main & reserve) sample size allocation
tab_sam_alloc[,
  n_sam_total := round_preserve_sum(
    (config::get("sample.size.main") + config::get("sample.size.reserve")) *
      (pers_sk / sum(pers_sk))
  )
]

# Main sample size allocation
tab_sam_alloc[,
  n_sam_main := round_preserve_sum(
    config::get("sample.size.main") * (pers_sk / sum(pers_sk))
  )
]

# Reserve sample size allocation
tab_sam_alloc[, n_sam_res := n_sam_total - n_sam_main]

# Totals
tab_sam_alloc[, map(.SD, sum), .SDcols = is.numeric]

frame_majo <- merge(
  x = frame_majo,
  y = tab_sam_alloc[, .(str_expl, n_sam_total, n_sam_main)]
)

setkey(frame_majo, str_expl, str_impl)
key(frame_majo)

# Size measure (truncated)

frame_majo[, summary(pers_sk_1859)]
frame_majo[, quantile(pers_sk_1859)]
frame_majo[, quantile(pers_sk_1859, probs = seq(.9, 1, .01))]
frame_majo[, quantile(pers_sk_1859, probs = seq(.99, 1, .001))]
frame_majo[, quantile(pers_sk_1859, probs = seq(.999, 1, .0001))]

frame_majo[, size := ifelse(pers_sk_1859 < 10L, pers_sk_1859, 10L)]
frame_majo[, .N, keyby = .(size)][, P := prop.table(N) |> round(3)][]
# 10+ sastāda 0.001 no visas populācijas

frame_majo[size == max(size)][sample(.N, 10), .(size, adrese)]

frame_majo[, as.list(summary(size))]
frame_majo[, as.list(summary(size)), keyby = .(str_expl)]

# Sampling probabilities
frame_majo[,
  c("pik_total", "pik_main") := map(.SD, \(x) {
    sampling::inclusionprobabilities(
      a = size,
      n = x[1]
    )
  }),
  .SDcols = c("n_sam_total", "n_sam_main"),
  by = .(str_expl)
]

frame_majo[, as.list(summary(pik_total))]
frame_majo[, as.list(summary(pik_main))]
frame_majo[, as.list(summary(pik_total)), keyby = .(str_expl)]
frame_majo[, as.list(summary(pik_main)), keyby = .(str_expl)]

frame_majo[, map(.SD, sum), .SDcols = c("pik_total", "pik_main")]

tab_test_pik <- frame_majo[,
  map(.SD, sum),
  .SDcols = patterns("^pik_"),
  by = .(str_expl)
]

stopifnot(
  all.equal(tab_sam_alloc$n_sam_total, tab_test_pik$pik_total),
  all.equal(tab_sam_alloc$n_sam_main, tab_test_pik$pik_main)
)

# Total sample
stopifnot(all.equal(key(frame_majo), c("str_expl", "str_impl")))
set.seed(191617)
frame_majo[,
  sample_total := as.integer(sampling::UPsystematic(pik = pik_total))
]
frame_majo[, sum(sample_total)]

# Main sample as subsample from the total sample
frame_majo[, pik_sub := sample_total * n_sam_main / n_sam_total]
frame_majo[, sum(pik_sub), keyby = .(sample_total)]

stopifnot(frame_majo[
  sample_total == 1L,
  all.equal(pik_main, pik_total * pik_sub)
])

frame_majo[, sample_main := as.integer(sampling::UPsystematic(pik = pik_sub))]
frame_majo[, sum(sample_main)]

frame_majo[, sample_res := sample_total * (1L - sample_main)]
frame_majo[, sum(sample_res)]

frame_majo[, .N, keyby = .(sample_total, sample_main, sample_res)]

tab_test_sam <- frame_majo[,
  map(.SD, sum),
  .SDcols = patterns("^sample_"),
  keyby = .(str_expl)
]

stopifnot(
  all.equal(tab_sam_alloc$n_sam_total, tab_test_sam$sample_total),
  all.equal(tab_sam_alloc$n_sam_main, tab_test_sam$sample_main)
)

### Design weights

# stage 1 (dwellings)
frame_majo[, dw_st1_total := sample_total / pik_total]
frame_majo[, dw_st1_main := sample_main / pik_main]

frame_majo[sample_total == 1L, as.list(summary(dw_st1_total))]
frame_majo[sample_main == 1L, as.list(summary(dw_st1_main))]
frame_majo[
  sample_total == 1L,
  as.list(summary(dw_st1_total)),
  keyby = .(str_expl)
]
frame_majo[
  sample_main == 1L,
  as.list(summary(dw_st1_main)),
  keyby = .(str_expl)
]

# stage 2 (persons)
frame_majo[, pik_st2 := 1 / size]
frame_majo[, dw_st2_total := dw_st1_total / pik_st2]
frame_majo[, dw_st2_main := dw_st1_main / pik_st2]

frame_majo[sample_total == 1L, as.list(summary(dw_st2_total))]
frame_majo[sample_main == 1L, as.list(summary(dw_st2_main))]
frame_majo[
  sample_total == 1L,
  as.list(summary(dw_st2_total)),
  keyby = .(str_expl)
]
frame_majo[
  sample_main == 1L,
  as.list(summary(dw_st2_main)),
  keyby = .(str_expl)
]


# Mājokļi
frame_majo[, .N]
frame_majo[, map(.SD, sum), .SDcols = patterns("^dw_st1_")]
# Precīzi nesakrīt dēļ pps

# Personas (size)
frame_majo[, sum(size)]
frame_majo[, map(.SD, \(x) sum(x * size)), .SDcols = patterns("^dw_st1_")]
frame_majo[, map(.SD, sum), .SDcols = patterns("^dw_st2_")]
# Sakrīt
stopifnot(
  all.equal(
    frame_majo[, rep(sum(size), 4)],
    c(
      frame_majo[, sum(size * dw_st1_main)],
      frame_majo[, sum(size * dw_st1_total)],
      frame_majo[, sum(dw_st2_main)],
      frame_majo[, sum(dw_st2_total)]
    )
  )
)

# Personas (pers_sk_1859)
frame_majo[, sum(pers_sk_1859)]
frame_majo[,
  map(.SD, \(x) sum(x * pers_sk_1859)),
  .SDcols = patterns("^dw_st1_")
]
frame_majo[, map(.SD, sum), .SDcols = patterns("^dw_st2_")]
# Nesakrīt dēļ trunc

# Save
fwrite(
  x = frame_majo,
  file = file.path(config::get("dir.data"), "frame_majo_samp.csvy.gz"),
  yaml = TRUE
)
