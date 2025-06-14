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
# 1: Valstspilsētas, kas nav iekļautas novados
# 5: Novadi

frame_majo[, .N, keyby = .(L1_type)]
frame_majo[, .N, keyby = .(L1_type, L1_code, L1_name)] # |> View()
# 1: Valstspilsētas, kas nav iekļautas novados
# 6: Novadu pilsētas tai skaitā valstspilsētas, kas ir iekļautas novados
# 7: Novadu pagasti

frame_majo[, .N, keyby = .(L0_type, L1_type)]

frame_majo[,
  pil_lauk := dplyr::case_when(
    L0_code == "LV0001000" ~ 1L, # Rīga
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

# Explicit stratification by urban level and region
frame_majo[,
  str_expl := glue_data(
    .SD,
    "str_{pil_lauk}_{NUTS3_code}"
  ) |>
    factor()
]

frame_majo[,
  .N,
  keyby = .(str_expl, pil_lauk, pil_lauk_name, NUTS3_code, NUTS3_name)
]

# Sakārtojums - implicit stratification
# L0 - admin ter
# L1 - ter vienības
# apkaimes
# ēkas
# dzīvokļa numurs

frame_majo[, apk_def := !is.na(apk_code)]

frame_majo[, .N, keyby = .(apk_code, apk_name)]
frame_majo[!(apk_def), apk_code := L1_code]
frame_majo[!(apk_def), apk_name := L1_name]
frame_majo[, .N, keyby = .(apk_code, apk_name)]

frame_majo[,
  .(apk_sk = length(unique(apk_code))),
  keyby = .(L1_code, L1_name)
][order(-apk_sk)]

# L0
dat_L0 <- frame_majo[,
  .(
    N = sum(pers_sk_1859),
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(NUTS3_code, L0_code)
]
set.seed(0)
dat_L0_ord <- sort.DT.by(dat_L0, by = "NUTS3_code", ord.name = "L0_ord")
plot.DT(
  dat_L0_ord,
  group = "NUTS3_code",
  colour = "NUTS3_code",
  size = "N",
  title = "L0 administratīvās teritorijas reģionos"
)

# L1
dat_L1 <- frame_majo[,
  .(
    N = sum(pers_sk_1859),
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(L0_code, L1_code)
]
set.seed(1)
dat_L1_ord <- sort.DT.by(dat_L1, by = "L0_code", ord.name = "L1_ord")
plot.DT(
  dat_L1_ord,
  group = "L0_code",
  colour = "L0_code",
  size = "N",
  title = "L1 teritoriālās vienības administratīvajās teritorijās"
)

# apk
dat_apk <- frame_majo[,
  .(
    N = sum(pers_sk_1859),
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(L1_code, apk_code, apk_def)
]
set.seed(2)
dat_apk_ord <- sort.DT.by(dat_apk, by = "L1_code", ord.name = "apk_ord")
plot.DT.by(
  dat_apk_ord[(apk_def)],
  by = "L1_code",
  size = "N",
  title = "Apkaimes"
)

# eka
dat_eka <- frame_majo[,
  .(
    N = sum(pers_sk_1859),
    koord_x = weighted.mean(koord_x, pers_sk_1859),
    koord_y = weighted.mean(koord_y, pers_sk_1859)
  ),
  keyby = .(apk_code, adr_kods_eka)
]
set.seed(3)
dat_eka_ord <- sort.DT.by(dat_eka, by = "apk_code", ord.name = "eka_ord")
plot.DT.by(
  dat_eka_ord[apk_code %in% sample(unique(apk_code), 5)],
  by = "apk_code",
  size = "N",
  title = "Ēkas"
)

dat_L0_ord[, .(L0_code, L0_ord)]
dat_L1_ord[, .(L1_code, L1_ord)]
dat_apk_ord[, .(apk_code, apk_ord)]
dat_eka_ord[, .(adr_kods_eka, eka_ord)]

frame_majo_ord <- copy(frame_majo) |>
  merge(y = dat_L0_ord[, .(L0_code, L0_ord)], by = "L0_code", all.x = TRUE) |>
  merge(y = dat_L1_ord[, .(L1_code, L1_ord)], by = "L1_code", all.x = TRUE) |>
  merge(
    y = dat_apk_ord[, .(apk_code, apk_ord)],
    by = "apk_code",
    all.x = TRUE
  ) |>
  merge(
    y = dat_eka_ord[, .(adr_kods_eka, eka_ord)],
    by = "adr_kods_eka",
    all.x = TRUE
  )

# frame_majo_ord

tab_dziv_sort_nos <- frame_majo_ord[tips_cd == 109L, .N, keyby = .(sort_nos)]
tab_dziv_sort_nos[grep("A", sort_nos)]
tab_dziv_sort_nos[grep("B", sort_nos)]
tab_dziv_sort_nos[grep("C", sort_nos)]
tab_dziv_sort_nos[grep("D", sort_nos)]
tab_dziv_sort_nos[grep("E", sort_nos)]

anyDuplicated(frame_majo_ord[tips_cd == 109L, .(adr_kods_eka, sort_nos)])

tab_test <- frame_majo_ord[
  tips_cd == 109L,
  .N,
  keyby = .(adr_kods_eka, sort_nos)
][
  N > 1
] |>
  unique()

tab_test

frame_majo_ord[
  adr_kods_eka %in% tab_test$adr_kods_eka[2],
  .(adr_kods_eka, sort_nos, adrese)
]

stopifnot(
  !anyDuplicated(
    frame_majo_ord,
    by = c("str_expl", "L0_ord", "L1_ord", "apk_ord", "eka_ord", "sort_nos")
  )
)

setorder(frame_majo_ord, str_expl, L0_ord, L1_ord, apk_ord, eka_ord, sort_nos)
frame_majo_ord[, str_impl := 1:.N, by = .(str_expl)]

setcolorder(frame_majo_ord, c("str_expl", "str_impl"))

# plot.DT.by(frame_majo_ord, by = "str_expl")

fwrite(
  x = frame_majo_ord,
  file = file.path(config::get("dir.data"), "frame_majo_ord.csvy.gz"),
  yaml = TRUE
)
