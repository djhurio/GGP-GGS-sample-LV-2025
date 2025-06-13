# Ietvara stratifikācija un sakārtojums

# Packages
library(purrr)
library(data.table)
library(ggplot2)
library(glue)
library(TSP)

# Reset
rm(list = ls())
gc()

# TSP sort and plot
source("code/TSP-sort-plot.R")

# Frame load
frame_majo <- fread(
  file = file.path(config::get("dir.data"), "frame_majo.csvy.gz"),
  yaml = TRUE
)

# Noņem dubultās pēdiņas
frame_majo[, adrese := gsub('""', '"', adrese)]

# Stratifikācija
# pil_lauk:
#   1: Rīga
#   2: Valstspilsētu pašvaldības (Daug, Jelg, Jūrm, Liep, Rēze, Vents)
#   3: Novadu pilsētas un valstspilsētas (tai skaitā Jēkabp, Ogre, Valm)
#   4: Novadu pagasti
# NUTS3

frame_majo[, .N, keyby = .(L0_type)]
frame_majo[, .N, keyby = .(L0_type, L0_code, L0_name)]
# 1: Valstspilsētas - nav iekļautas novados
# 5: Novadi

frame_majo[, .N, keyby = .(L1_type)]
frame_majo[, .N, keyby = .(L1_type, L1_code, L1_name)] # |> View()

frame_majo[, .N, keyby = .(L0_type, L1_type)]

frame_majo[,
  pil_lauk := dplyr::case_when(
    L0_code == "LV0001000" ~ 1L,
    L1_type == 1L ~ 2L,
    L1_type == 6L ~ 3L,
    L1_type == 7L ~ 4L
  )
]

frame_majo[,
  pil_lauk_name := factor(
    x = pil_lauk,
    levels = 1:4,
    labels = c("Rīga", "Valstspilsētas", "Pilsētas", "Novadi")
  )
]

frame_majo[, .N, keyby = .(pil_lauk, pil_lauk_name, L0_type, L1_type)]

frame_majo[, .N, keyby = .(NUTS3_code, NUTS3_name)]

frame_majo[, .N, keyby = .(pil_lauk, pil_lauk_name, NUTS3_code, NUTS3_name)]

dcast.data.table(
  data = frame_majo,
  formula = NUTS3_code + NUTS3_name ~ pil_lauk_name,
  fun.aggregate = length
)

frame_majo[,
  strata := glue_data(
    .SD,
    "str_{pil_lauk}_{NUTS3_code}"
  ) |>
    factor()
]

frame_majo[,
  .N,
  keyby = .(strata, pil_lauk, pil_lauk_name, NUTS3_code, NUTS3_name)
]

# Sakārtojam teritorijas
dat_L0 <- frame_majo[,
  .(
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(NUTS3_code, L0_code)
]
set.seed(0)
dat_L0_ord <- sort.DT.by(dat_L0, by = "NUTS3_code", ord.name = "L0_ord")
plot.DT(dat_L0_ord, group = "NUTS3_code", colour = "NUTS3_code")

dat_L1 <- frame_majo[,
  .(
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(L0_code, L1_code)
]
set.seed(1)
dat_L1_ord <- sort.DT(dat_L1)
plot.DT(dat_L1_ord, group = "L0_code", colour = "L0_code")
