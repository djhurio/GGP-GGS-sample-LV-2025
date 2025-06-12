# VZD datu ielase, atarhivēšana un saglabāšana
# Valsts adrešu reģistra atvērtie dati
# https://data.gov.lv/dati/lv/dataset/varis-atvertie-dati

# Packages
library(purrr)
library(data.table)


# Reset ####
rm(list = ls())
gc()


dir.data.vzd <- file.path(config::get("dir.data"), "vzd")
dir.data.vzd.csv <- file.path(dir.data.vzd, "csv")
dir.create(path = dir.data.vzd.csv, recursive = T, showWarnings = F)

ulist <- file.path(
  "https://data.gov.lv/dati/dataset/6b06a7e8-dedf-4705-a47b-2a7c51177473",
  "resource",
  c(
    "ee02baa4-2bc3-4f77-a6cb-5427a3e9befe/download/aw_pilseta.csv",
    "c62c60bb-58d4-4f26-82c0-5b630769f9d1/download/aw_novads.csv",
    "6ba8c905-27a1-443a-b9c6-256a0777425b/download/aw_pagasts.csv",
    "0d3810f4-1ac0-4fba-8b10-0188084a361b/download/aw_ciems.csv",
    "a510737a-18ce-400f-ad4b-04fce5228272/download/aw_eka.csv",
    "b83be373-f444-4f50-9b98-28741845325e/download/aw_dziv.csv"
  )
)

ulist
basename(ulist)

flist <- file.path(dir.data.vzd.csv, basename(ulist))
flist

map2(
  .x = ulist,
  .y = flist,
  download.file
)

system2(command = "file", args = flist)

namelist <- tolower(gsub(".*/|.csv", "", flist))
namelist

names(flist) <- namelist
flist

# Test
# dat <- fread(file = flist[1], quote = "#")

# Read all files
dat <- map(flist, fread)

length(flist)
length(dat)

names(dat)

# Rename vars
map(dat, names)
map(dat, function(x) {
  setnames(x, tolower)
  return(names(x))
})
map(dat, names)

# Export to global env
list2env(dat, globalenv())

rm(dat)
gc()

aw_ternames <- c(
  "aw_pilseta",
  "aw_novads",
  "aw_pagasts",
  "aw_ciems"
)

# Vai visas kolonas ir vienādas?
map(mget(aw_ternames), names)

aw_ter <- rbindlist(
  mget(aw_ternames),
  use.names = T,
  fill = T
)
rm(list = aw_ternames)
gc()

names(aw_ter)

map(mget(ls(pattern = "aw_eka")), names)
map(mget(ls(pattern = "aw_dziv")), names)

aw_eka[, std := gsub('""', '"', std)]
aw_dziv[, std := gsub('""', '"', std)]

# Save
save(
  aw_ter,
  aw_eka,
  aw_dziv,
  file = file.path(dir.data.vzd, "data-vzd.Rdata")
)
