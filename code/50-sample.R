# Sampling

# Izlasei vajadzētu atlasīt 25 000 adrešu.
# Ja tomēr izrādīsies, ka atbildētība ir ļoti bēdīga,
# mums vajadzētu vēl 5000 rezerves adreses, ko varam izmantot.

# Packages
library(purrr)
library(data.table)
library(ggplot2)
library(glue)

# Reset
rm(list = ls())
gc()

# Functions
round_preserve_sum <- function(x) {
  floored <- floor(x)
  remainder <- round(sum(x)) - sum(floored)

  # Compute fractional parts and use original index for tie-breaking
  fractional <- x - floored

  # Rank the fractional parts
  # for equal fractional parts smaller values gets rounded up
  indices <- order(-fractional, x)[1:remainder]

  floored[indices] <- floored[indices] + 1
  return(floored)
}

round_preserve_sum(c(1.2, 2.8, 3.5, 4.5))
round_preserve_sum(c(1.2, 2.8, 4.5, 3.5))
round_preserve_sum(c(4.5, 3.5, 1.2, 2.8))


# Load
frame_majo <- fread(
  file = file.path(config::get("dir.data"), "frame_majo_ord.csvy.gz"),
  yaml = TRUE
)

# Noņem dubultās pēdiņas
frame_majo[, adrese := gsub('""', '"', adrese)]


# Sample allocation
tab_sam_alloc <- frame_majo[,
  .(majo_sk = .N, pers_sk = sum(pers_sk_1859)),
  keyby = .(str_expl, pil_lauk_name, NUTS3_name)
]
