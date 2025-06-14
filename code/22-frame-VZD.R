# VZD frame

# Packages
library(purrr)
library(data.table)

# Reset ####
rm(list = ls())
gc()


# Load data ####
load(file.path(config::get("dir.data"), "vzd/data-vzd.Rdata"))


# Dzīvokļi ####
aw_dziv

aw_dziv[, .N, keyby = .(tips_cd)]
# 1:     109 932251

aw_dziv[, .N, keyby = .(statuss)]
# 1:     DEL  16427
# 2:     EKS 845122
# 3:     ERR  70702

aw_dziv[, .N, keyby = .(apstipr, apst_pak)]
# 1:       NA 147744
# 2:      251  10217
# 3:      252 278902
# 4:      253  51075
# 5:      254 444313

aw_dziv[, .N, keyby = .(vkur_tips)]
# 1:       108 932251

# Labojums `sort_nos`
get_sort_nos <- function(nosaukums) {
  stringr::str_replace_all(
    nosaukums,
    pattern = "\\d+",
    replacement = function(x) stringr::str_pad(as.integer(x), 5, pad = "0")
  )
}
get_sort_nos(c("1", "1/11", "1/1A", "9K", "123/4B", "12345", "123456"))

anyDuplicated(aw_dziv[statuss == "EKS"], by = c("vkur_cd", "nosaukums"))
tab_test <- aw_dziv[statuss == "EKS", .N, keyby = .(vkur_cd, nosaukums)][N > 1]
aw_dziv[statuss == "EKS" & vkur_cd %in% tab_test$vkur_cd][order(
  vkur_cd,
  sort_nos
)]

# aw_dziv[statuss == "EKS", .N, keyby = .(nosaukums, sort_nos)]

# Sagatavo labojumu
# Lēna funkcija, tāpēc rēķinam alternatīvas tikai dzīvokļiem,
# kas beidzas ar burtu
aw_dziv[
  statuss == "EKS" & grepl("[A-Z]$", nosaukums),
  sort_nos_alt := get_sort_nos(nosaukums)
]

aw_dziv[
  statuss == "EKS" & sort_nos != sort_nos_alt,
  .(kods, nosaukums, sort_nos, sort_nos_alt, std)
]

# Labojums
aw_dziv[sort_nos != sort_nos_alt, sort_nos := sort_nos_alt]

anyDuplicated(aw_dziv[statuss == "EKS"], by = c("vkur_cd", "sort_nos"))
tab_test <- aw_dziv[statuss == "EKS", .N, keyby = .(vkur_cd, sort_nos)][
  N > 1
]
aw_dziv[
  statuss == "EKS" &
    vkur_cd %in% tab_test$vkur_cd &
    sort_nos %in% tab_test$sort_nos
][order(
  vkur_cd,
  sort_nos
)]

frame_dziv <- aw_dziv[,
  .(
    adr_kods = kods,
    adr_kods_eka = vkur_cd,
    tips_cd,
    statuss,
    apstipr,
    apst_pak,
    sort_nos,
    adrese = std
  )
]

if (anyDuplicated(frame_dziv, by = "adr_kods")) {
  stop("dubl dziv")
}


# Ēkas un zemes ####
aw_eka

aw_eka[, .N, keyby = .(statuss)]
# 1:     DEL  26764
# 2:     EKS 525262
# 3:     ERR  27950

aw_eka[, .N, keyby = .(apstipr, apst_pak)]
# 1:       NA  24399
# 2:      251  13095
# 3:      252 447956
# 4:      253  46617
# 5:      254  47909

aw_eka[, .N, keyby = .(vkur_tips)]
# 1:       104   1826
# 2:       105 119836
# 3:       106 131511
# 4:       107 322030
# 5:       113   4773

# # Pasta indekss
# aw_eka[, .N, keyby = .(atrib)]

# # Pasta nodaļas kods
# aw_eka[, .N, keyby = .(pnod_cd)]

# Pazīme, ka adresācijas objekts  ir apbūvei paredzēta zemes vienība.
# Y – ir apbūvei paredzēta zemes vienība; N – ir ēka
aw_eka[, .N, keyby = .(for_build)]
# 1:         N 435216
# 2:         Y 144760

aw_eka[, .N, keyby = .(plan_adr)]
# 1:        N 535701
# 2:        Y  70040

aw_eka[, .N, keyby = .(statuss, for_build)]

frame_eka <- aw_eka[,
  .(
    adr_kods = kods,
    adr_kods_eka = kods,
    tips_cd,
    statuss,
    apstipr,
    apst_pak,
    for_build,
    plan_adr,
    adrese = std,
    koord_x,
    koord_y,
    dd_n,
    dd_e
  )
]


# Teritorijas ####
aw_ter

aw_ter[, .N, keyby = .(tips_cd)]

aw_ter[, .N, keyby = .(vkur_tips)]

dcast.data.table(
  aw_ter,
  tips_cd ~ vkur_tips,
  fun.aggregate = length
)

aw_ter[, .N, keyby = .(apstipr, apst_pak)]
# 1:       NA  1603
# 2:      251   728
# 3:      252 23006
# 4:      253  5061
# 5:      254   285

aw_ter[, .N, keyby = .(statuss)]
# 1:     DEL  3476
# 2:     EKS 24735
# 3:     ERR  2472

# Ielas un teritorijas ####
frame_ter <- aw_ter[
  statuss == "EKS",
  .(
    adr_kods = kods,
    tips_cd,
    statuss,
    apstipr,
    apst_pak,
    adrese = std
  )
]


# Dzīvokļiem pievieno geo pēc ēkas
frame_eka[, .(adr_kods, koord_x, koord_y, dd_e, dd_n)]
frame_dziv[, all(adr_kods_eka %in% frame_eka$adr_kods)]

frame_dziv <- merge(
  x = frame_dziv,
  y = frame_eka[, .(adr_kods_eka = adr_kods, koord_x, koord_y, dd_e, dd_n)],
  by = "adr_kods_eka",
  all.x = TRUE
)


# Apvieno teritorijas, zemes, ēkas un telpu grupas
names(frame_dziv)
names(frame_eka)
names(frame_ter)

frame_vzd <- rbindlist(
  l = list(
    frame_dziv,
    frame_eka,
    frame_ter
  ),
  use.names = TRUE,
  fill = TRUE
)


# Tipi
tab_tips_cd <- data.table(
  tips_cd = c("101", "102", "104", "105", "106", "107", "108", "109", "113"),
  tips_cd_nos = c(
    "Latvijas Republika",
    "Rajons",
    "Pilsēta",
    "Pagasts",
    "Ciems/mazciems",
    "Iela",
    "Ēka, apbūvei paredzēta zemes vienība",
    "Telpu grupa",
    "Novads"
  )
)

frame_vzd[,
  tips_cd_nos := factor(
    x = tips_cd,
    levels = tab_tips_cd$tips_cd,
    labels = tab_tips_cd$tips_cd_nos
  )
]

frame_vzd[, .N, keyby = .(tips_cd, tips_cd_nos)]

# Sakārto
setkey(frame_vzd, adr_kods)
setcolorder(frame_vzd)

# Save
fwrite(
  x = frame_vzd,
  file = file.path(config::get("dir.data"), "frame_vzd.csvy.gz"),
  yaml = TRUE
)


# Ziņojumam
tab_vzd <- frame_vzd[,
  .N,
  keyby = .(tips_cd, tips_cd_nos)
]

tab_vzd

fwrite(
  x = tab_vzd,
  file = file.path(config::get("dir.data"), "tab_vzd.csvy"),
  yaml = TRUE
)
