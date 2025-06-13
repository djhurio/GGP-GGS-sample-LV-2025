# Population frame

# Packages
library(purrr)
library(data.table)
library(glue)
library(sf)

# Reset ####
rm(list = ls())
gc()

# CSP dati
frame_csp <- fread(
  file = file.path(config::get("dir.data"), "frame_csp.csvy.gz"),
  yaml = TRUE
) |>
  setkey(adr_kods)

# VZD dati
frame_vzd <- fread(
  file = file.path(config::get("dir.data"), "frame_vzd.csvy.gz"),
  yaml = TRUE
) |>
  setkey(adr_kods)

# Noņem dubultās pēdiņas
# frame_vzd[grep('""', adrese), .(adrese)]
frame_vzd[, adrese := gsub('""', '"', adrese)]
# frame_vzd[grep('""', adrese), .(adrese)]

# Apvieno VZD un CSP datus
# map(frame_csp, class) |> unlist()
# map(frame_vzd, class) |> unlist()

stopifnot(
  key(frame_csp) == key(frame_vzd),
  !anyDuplicated(frame_csp, by = key(frame_csp)),
  !anyDuplicated(frame_vzd, by = key(frame_vzd)),
  all(frame_csp$adr_kods %in% frame_vzd$adr_kods)
)

# Veido GGS ietvaru
frame_majo <- merge(
  x = frame_vzd,
  y = frame_csp,
  all = TRUE
)

stopifnot(
  frame_majo[, .N] == frame_vzd[, .N],
  frame_majo[!is.na(pers_sk), .N] == frame_csp[, .N],
  frame_majo[!is.na(pers_sk_1859), .N] == frame_csp[, .N]
)

# Sakārto
frame_majo |>
  setorder(adr_kods_eka, tips_cd, sort_nos) |>
  setkey(adr_kods_eka) |>
  setcolorder()
frame_majo

# Atzīmē mājokļus, kas bija CSP ietvarā
frame_majo[, frame_csp := !is.na(pers_sk)]

stopifnot(
  frame_majo[(frame_csp), .N] == frame_csp[, .N]
)

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  )
]

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, tips_cd_nos)
]

# Adresācijas objekta statuss:
# EKS – eksistējošs;
# DEL – likvidēts;
# ERR – kļūdains
frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, statuss, apstipr, plan_adr)
]

# Atzīmē, kurus mājokļus paturēs
frame_majo[, keep := (tips_cd %in% 108:109 & statuss == "EKS")]

frame_majo[
  (frame_csp) & (keep),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, statuss, apstipr, plan_adr)
]

frame_majo[
  pers_sk_1859 > 0L & (keep) & plan_adr == "Y",
  .(adr_kods_eka, adr_kods, pers_sk_1859, adrese)
]

# Ziņojumam - personas, kas tiks dzēstas
frame_majo_del1 <- frame_majo[(frame_csp) & !(keep)]

frame_majo_del1[,
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  )
]

frame_majo_del1[,
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, tips_cd_nos, statuss)
]

fwrite(
  x = frame_majo_del1,
  file = file.path(config::get("dir.data"), "frame_majo_del1.csvy.gz"),
  yaml = TRUE
)

# Atlasa tikai derīgos mājokļus
pop_n <- frame_majo[(frame_csp), sum(pers_sk_1859)]
pop_n_uc1 <- frame_majo[(frame_csp) & !(keep), sum(pers_sk_1859)]

# UC rate
print(glue("N: {pop_n}"))
print(glue("UC1: {pop_n_uc1}"))
print(glue("N - UC1: {pop_n - pop_n_uc1}"))
print(glue("UC1 rate: {pop_n_uc1 / pop_n}"))


# Atlasa
frame_majo <- frame_majo[(keep)]
frame_majo[, keep := NULL]

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  )
]

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, tips_cd_nos, plan_adr)
]

stopifnot(frame_majo[(frame_csp), sum(pers_sk_1859)] == pop_n - pop_n_uc1)


# Dzēšam tukšās ēkas
frame_majo[, frame_csp_eka := any(frame_csp), by = .(adr_kods_eka)]
frame_majo[, .N, keyby = .(frame_csp_eka, frame_csp)]

frame_majo <- frame_majo[(frame_csp_eka)]
frame_majo[, .N, keyby = .(frame_csp_eka, frame_csp)]
frame_majo[, frame_csp_eka := NULL]


# Aprēķina telpu grupu (dzīvokļu) skaitu ēkās
frame_majo[, .N, keyby = .(tips_cd, tips_cd_nos)]
frame_majo[, dziv_sk := as.integer(sum(tips_cd == 109L)), by = .(adr_kods_eka)]


# Jauns tips, lai atšķirtu ēkas ar un bez dzīvokļiem
frame_majo[, .N, keyby = .(tips_cd, tips_cd_nos, dziv_sk >= 1L)]

frame_majo[tips_cd == 108L & dziv_sk == 0L, tips_ekdz := "108a"]
frame_majo[tips_cd == 108L & dziv_sk >= 1L, tips_ekdz := "108b"]
frame_majo[tips_cd == 109L & dziv_sk >= 1L, tips_ekdz := "109b"]

frame_majo[,
  tips_ekdz_nos := factor(
    x = tips_ekdz,
    levels = c("108a", "108b", "109b"),
    labels = c(
      "Ēka bez dzīvokļiem",
      "Ēka ar dzīvokļiem",
      "Dzīvoklis"
    )
  )
]

frame_majo[, .N, keyby = .(tips_ekdz, tips_ekdz_nos)]

# Personas, kas ir deklarētas ēkā, kurā ir arī dzīvokļi
frame_majo[
  tips_ekdz == "108b" & pers_sk_1859 > 0L,
  .(eku_sk = .N, pers_sk_1859 = sum(pers_sk_1859))
]
frame_majo[
  tips_ekdz == "108b" & pers_sk_1859 > 0L,
  .(adr_kods_eka, dziv_sk, pers_sk_1859, adrese)
][adr_kods_eka %in% sample(adr_kods_eka, 5)]

# Atzīmē daudzdzīvokļu ēkas (un visus tās dzīvokļus),
# kurās ēkas līmenī ir deklarētās personas
frame_majo[,
  problem := any(
    tips_ekdz == "108b" & !is.na(pers_sk_1859) & pers_sk_1859 > 0L
  ),
  by = .(adr_kods_eka)
]

frame_majo[
  (frame_csp),
  .(majo_sk = .N, pers_sk_1859 = sum(pers_sk_1859)),
  keyby = .(tips_ekdz, problem)
]

frame_majo[
  (problem),
  .(majo_sk = .N, pers_sk_1859 = sum(pers_sk_1859)),
  keyby = .(tips_ekdz, problem, frame_csp, pers_sk_1859 > 0)
]

frame_majo[
  (problem),
  .(adr_kods_eka, dziv_sk, pers_sk_1859, adrese)
][adr_kods_eka %in% sample(unique(adr_kods_eka), 3)]

# Dzēšam personas, kas ir deklarētas ēkas līmenī, ja ēkā ir dzīvokļi
frame_majo[, .N, keyby = .(tips_ekdz, tips_ekdz_nos)]
frame_majo[, keep := tips_ekdz %in% c("108a", "109b")]

frame_majo[
  (frame_csp) & (keep),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_ekdz, tips_ekdz_nos)
]

# Ziņojumam - personas, kas tiks dzēstas
frame_majo_del2 <- frame_majo[(frame_csp) & !(keep)]

frame_majo_del2[,
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  )
]

frame_majo_del2[,
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, tips_cd_nos, statuss)
]

fwrite(
  x = frame_majo_del2,
  file = file.path(config::get("dir.data"), "frame_majo_del2.csvy.gz"),
  yaml = TRUE
)

# Atlasa tikai derīgos mājokļus
pop_n_uc2 <- frame_majo[(frame_csp) & !(keep), sum(pers_sk_1859)]

# UC rate
print(glue("N: {pop_n}"))
print(glue("UC1: {pop_n_uc1}"))
print(glue("UC2: {pop_n_uc2}"))
print(glue("N - UC1 - UC2: {pop_n - pop_n_uc1 - pop_n_uc2}"))
print(glue("UC2 rate: {(pop_n_uc1 + pop_n_uc2) / pop_n}"))


# Atlasa
frame_majo <- frame_majo[(keep)]
frame_majo[, keep := NULL]
frame_majo[, problem := NULL]

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  )
]

frame_majo[
  (frame_csp),
  .(
    majo_sk = .N,
    pers_sk = sum(pers_sk),
    pers_sk_1859 = sum(pers_sk_1859)
  ),
  keyby = .(tips_cd, tips_cd_nos, tips_ekdz, tips_ekdz_nos)
]

stopifnot(
  frame_majo[(frame_csp), sum(pers_sk_1859)] == pop_n - pop_n_uc1 - pop_n_uc2
)

# Saglabājam tikai mājokļus ar apsekojumam derīgām personām
frame_majo <- frame_majo[pers_sk_1859 > 0]
frame_majo[, sum(pers_sk_1859), keyby = .(frame_csp)]
frame_majo[, frame_csp := NULL]


# Teritorijas kodu un nosaukumu pievienošana
# ATVK on 2024-01-01
shp_atvk <- read_sf(
  dsn = file.path(
    config::get("dir.data"),
    "geo",
    "Territorial_units_LV_1.2m_(2024.01.01.)"
  )
)

shp_atvk
names(shp_atvk)

shp_atvk[c("L1_code", "geometry")] |> plot()
shp_atvk[c("L0_code", "geometry")] |> plot()
shp_atvk[c("NUTS3_code", "geometry")] |> plot()

st_crs(shp_atvk)


# Apkaimes
shp_apk <- read_sf(
  dsn = file.path(
    config::get("dir.data"),
    "geo",
    "apkaimes"
  )
)

shp_apk
names(shp_apk)
names(shp_apk) <- c("apk_code", "apk_name", "geometry")
names(shp_apk)
shp_apk[c("apk_code", "geometry")] |> plot()
st_crs(shp_apk)


# Spatial join
names(frame_majo)

frame_eka <- frame_majo[, .(adr_kods_eka, koord_x, koord_y)] |> unique()

frame_eka[, .(
  min_x = min(koord_x),
  min_y = min(koord_y),
  max_x = max(koord_x),
  max_y = max(koord_y)
)]

stopifnot(frame_eka[is.na(koord_x) | is.na(koord_y), .N] == 0L)

frame_eka_sf <- st_as_sf(
  x = frame_eka,
  coords = c("koord_y", "koord_x"), # Svarīga secība
  crs = st_crs(shp_apk)
)

frame_eka_sf
class(frame_eka_sf)

stopifnot(
  st_crs(shp_apk) == st_crs(shp_atvk),
  st_crs(shp_apk) == st_crs(frame_eka_sf),
  length(frame_eka_sf$geometry[is.na(frame_eka_sf$geometry)]) == 0L
)


# punktiem pievieno shapes
frame_eka_sf_ter <- st_join(
  x = frame_eka_sf,
  y = shp_atvk,
  join = st_nearest_feature
)

frame_eka_sf_apk <- st_join(
  x = frame_eka_sf_ter[
    frame_eka_sf_ter$L1_name %in%
      c("Rīga", "Daugavpils", "Jūrmala", "Valmiera"),
    c("adr_kods_eka", "geometry")
  ],
  y = shp_apk,
  join = st_nearest_feature
)

setDT(frame_eka_sf_ter, key = "adr_kods_eka")
setDT(frame_eka_sf_apk, key = "adr_kods_eka")

frame_eka_sf_ter[, geometry := NULL]
frame_eka_sf_apk[, geometry := NULL]

frame_eka_sf_ter[, .N]
frame_eka_sf_apk[, .N]

frame_eka_sf_ter[, .N, keyby = .(L0_code, L0_name, L0_type)]
frame_eka_sf_ter[, .N, keyby = .(L1_code, L1_name, L1_type)]
frame_eka_sf_apk[, .N, keyby = .(apk_code, apk_name)]

frame_eka <- merge(
  x = frame_eka_sf_ter,
  y = frame_eka_sf_apk,
  all = TRUE
)

# Pievieno pie mājokļiem
frame_majo <- merge(
  x = frame_majo,
  y = frame_eka,
  all = TRUE
)


# Save
frame_majo <- frame_majo[,
  .(
    adr_kods_eka,
    adr_kods,
    tips_cd,
    sort_nos,
    adrese,
    koord_x,
    koord_y,
    dd_e,
    dd_n,
    pers_sk_1859,
    L1_code,
    L1_name,
    L1_type,
    L0_code,
    L0_name,
    L0_type,
    NUTS3_code,
    NUTS3_name,
    apk_code,
    apk_name
  )
]

frame_majo[, .(majo_sk = .N, pers_sk = sum(pers_sk_1859))]

fwrite(
  x = frame_majo,
  file = file.path(config::get("dir.data"), "frame_majo.csvy.gz"),
  yaml = TRUE
)
