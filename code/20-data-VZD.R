# VZD dati

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()


# Test for usage of the open VZD data
# https://data.gov.lv/dati/lv/dataset/valsts-adresu-registra-informacijas-sistemas-atvertie-dati/resource/1d3cbdf2-ee7d-4743-90c7-97d38824d0bf

dir.data.vzd <- file.path(config::get("dir.data"), "vzd")
dir.data.vzd.csv <- file.path(dir.data.vzd, "csv")
dir.create(path = dir.data.vzd.csv, recursive = T, showWarnings = F)

download.file(
  url = "https://data.gov.lv/dati/dataset/0c5e1a3b-0097-45a9-afa9-7f7262f3f623/resource/1d3cbdf2-ee7d-4743-90c7-97d38824d0bf/download/aw_csv.zip",
  destfile = file.path(dir.data.vzd, "aw_csv.zip")
)

unzip(
  zipfile = file.path(dir.data.vzd, "aw_csv.zip"),
  exdir = dir.data.vzd.csv
)

flist <- list.files(path = dir.data.vzd.csv, pattern = "CSV$", full.names = T)
flist
system2(command = "file", args = flist)

namelist <- tolower(gsub(".*/|.CSV", "", flist))
namelist

# readlist <- paste("iconv -f ISO-8859-13 -t UTF-8", file.path(getwd(), flist))
# readlist

# tmp <- fread(file = flist[1], quote = "#", encoding = "UTF-8")
# tmp
# 
# tmp <- fread(file = flist[1], quote = "#", encoding = "Latin-1")
# tmp
# 
# tmp <- fread("iconv -f ISO-8859-13 -t UTF-8 /home/djhurio/Dropbox/Darbs/ESS9-LV-2018-2019/ESS9-LV/data/VZD/AK_20181211/csv/AW_CIEMS.CSV",
#              quote = "#")
# tmp

# Test
dat <- fread(file = flist[1], quote = "#")

# Read all files
dat <- lapply(
  flist,
  function(x) fread(
    file = x,
    quote = "#",
    colClasses = "character",
    integer64 = "character"
  )
)

length(dat)

names(dat) <- namelist

list2env(dat, globalenv())

rm(dat)
gc()

aw_ternames <- c(
  "aw_rajons",
  "aw_pilseta",
  "aw_novads",
  "aw_pagasts",
  "aw_ciems",
  "aw_iela"
)
aw_ter <- rbindlist(
  mget(aw_ternames),
  use.names = T,
  fill = T
)
rm(list = aw_ternames)
gc()

# aw_geonames <- c("aw_eka_geo", "aw_vieta_centroid_geo")
aw_geonames <- c(
  "aw_vietu_centroidi"
)
aw_geo <- rbindlist(
  mget(aw_geonames),
  use.names = T
)
rm(list = aw_geonames)
gc()

aw_dziv
# aw_nlieta
aw_eka

# aw_iela_geo
# aw_vieta_geo

for (aw in list(aw_ter, aw_dziv, aw_eka, aw_geo)) {
  setnames(x = aw, new = tolower(names(aw)))
}
rm(aw)

# save(aw_ter, aw_dziv, aw_nlieta, aw_geo, file = "data/VZD/data-VZD.Rdata")
save(
  aw_ter,
  aw_dziv,
  aw_eka,
  aw_geo,
  file = file.path(dir.data.vzd, "data-vzd.Rdata")
)
