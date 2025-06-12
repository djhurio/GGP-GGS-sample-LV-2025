# Population frame

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()

# CSP dati
frame_csp <- fread(
  file = file.path(config::get("dir.data"), "frame_csp.csvy.gz"),
  yaml = TRUE
)

# VZD dati
frame_vzd <- fread(
  file = file.path(config::get("dir.data"), "frame_vzd.csvy.gz"),
  yaml = TRUE
)

# Noņem dubultās pēdiņas
# frame_vzd[grep('""', adrese), .(adrese)]
frame_vzd[, adrese := gsub('""', '"', adrese)]
# frame_vzd[grep('""', adrese), .(adrese)]

# Apvieno VZD un CSP datus
map(frame_csp, class) |> unlist()
map(frame_vzd, class) |> unlist()

stopifnot(!anyDuplicated(frame_csp, by = "adr_kods"))
stopifnot(!anyDuplicated(frame_vzd, by = "adr_kods"))

stopifnot(all(frame_csp$adr_kods %in% frame_vzd$adr_kods))

# Veido GGS ietvaru
frame_ggs <- merge(
  x = frame_csp[vc1859_sum > 0, .(adr_kods, pers_sk, vc1859_sum)],
  y = frame_vzd,
  by = "adr_kods",
  all.x = TRUE
)

stopifnot(frame_ggs[, .N] == frame_csp[vc1859_sum > 0, .N])

frame_ggs[,
  .(majo_sk = .N, pers_sk = sum(vc1859_sum)),
  keyby = .(tips_cd, tips_cd_nos)
]

frame_ggs[
  tips_cd == 108L & dziv_sk > 0L,
  .(majo_sk = .N, pers_sk = sum(vc1859_sum))
]
frame_ggs[tips_cd == 108L & dziv_sk > 0]

# Atzīmē daudzdzīvokļu ēkas (un visus tās dzīvokļus),
# kuras nav kolektīvās un kurās ēkas līmenī ir deklarētās personas
frame_ggs[,
  problem := any(tips_cd == 108L & dziv_sk > 0),
  by = .(adr_kods_eka)
]
frame_ggs[, .N, keyby = .(problem, tips_cd)]

frame_ggs[
  (problem),
  .(adr_kods_eka, dziv_sk, adrese, vc1859_sum)
][order(adr_kods_eka)]

frame_ggs[, sum_dekl := sum(dekl), by = .(adr_kods_eka)]

# Problemātiskās ēkās, kurās personas ir deklarētas gan ēkas, gan dzīvokļu līmenī,
frame_ggs[tips_cd %in% 108:109, n := .N, by = .(adr_kods_eka)]
frame_ggs[tips_cd == 108L & n > 1L]

tab <- frame_ggs[
  (problem) & sum_dekl < dziv_sk + 1,
  .(adr_kods_eka, tips_cd, ATVK_code, adrese, adrese_sort, dekl, sum_dekl)
][order(ATVK_code, adr_kods_eka, tips_cd, adrese_sort)]
tab[, adrese_sort := NULL]

# Ēkas, kur personas ir tikai ēkas līmenī
tab1 <- tab[sum_dekl == 1]
# Ēkas, kur personas ir gan ēkas, gan dzīvokļa līmenī
tab2 <- tab[sum_dekl > 1]

# Dzīvokļos nav deklarētu personu
tab1[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]
# Dzīvokļu īpatsvars, kuros ir deklarētas personas
tab2[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]

write.xlsx(
  list(tab1, tab2),
  file = "data/ekas-test.xlsx",
  firstRow = T,
  colWidths = "auto"
)

# Lēmums: šajos gadījumos mājokļu ietvarā tiek ietverti visi šo ēku dzīvokļi

tab3 <- frame_ggs[,
  .N,
  keyby = .(kol, tips_cd, ddzeka = dziv_sk > 0, problem, dekl)
]
tab3

write.xlsx(
  x = tab3,
  file = "data/frame-tab.xlsx",
  firstRow = T,
  colWidths = "auto"
)

frame_majo <- frame_ggs[
  !kol &
    ((tips_cd == "108" & dziv_sk == 0 & dekl) |
      (tips_cd == "109" & (dekl | problem)))
]

frame_majo[, .N]

frame_majo[, .N, keyby = .(kol, tips_cd, ddzeka = dziv_sk > 0, problem, dekl)]

# 1. līmenis – 119 administratīvās teritorijas
frame_majo[, .N, keyby = .(ATVK_L1)]
frame_majo[, .N, keyby = .(ATVK_L1)][order(N)]

frame_majo[is.na(ATVK_L2), .N, keyby = .(ATVK_L1)]
frame_majo[is.na(ATVK_L2), .N, keyby = .(ATVK_L1)][order(N)]

# 2. līmenis – 564 novadu teritoriālā iedalījuma vienības
frame_majo[!is.na(ATVK_L2), .N, keyby = .(ATVK_L2)]
frame_majo[!is.na(ATVK_L2), .N, keyby = .(ATVK_L2)][order(N)]

frame_majo[, .N, keyby = .(ATVK_L1)][order(-N)][1:10]
frame_majo[, .N, keyby = .(ATVK_code)][order(-N)][1:10]


frame_majo[, c("kol", "problem", "sum_dekl") := NULL]

sapply(dat_UR, class)

# Reģistrēto uzņēmumu skaits mājoklī
frame_majo <- merge(
  x = frame_majo,
  y = dat_UR[, .(addressid, uzn_sk)],
  by.x = "adr_kods",
  by.y = "addressid",
  all.x = T,
  sort = F
)

frame_majo[is.na(uzn_sk), uzn_sk := 0L]

frame_majo[, uzn_sk_dummy := as.integer(uzn_sk > 0)]

frame_majo[, .N, keyby = .(uzn_sk_dummy)][, P := prop.table(N)][]

# frame_majo[order(-uzn_sk), .(adr_kods, adrese, uzn_sk)][1:5]
# frame_majo[adr_kods == "115070060", .(adr_kods, adrese, uzn_sk)]

frame_majo[, lapply(.SD, sum), .SDcols = patterns("uzn_sk")]
frame_majo[, lapply(.SD, mean), .SDcols = patterns("uzn_sk")]

save(frame_majo, file = "results/frame_majo.Rdata")
