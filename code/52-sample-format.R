# Sample formatting

# Packages
library(purrr)
library(data.table)
library(ggplot2)
library(glue)
library(openxlsx2)

# Reset
rm(list = ls())
gc()

# Load
frame_majo <- fread(
  file = file.path(config::get("dir.data"), "frame_majo_samp.csvy.gz"),
  yaml = TRUE,
  key = c("str_expl", "str_impl")
)
key(frame_majo)

# Noņem dubultās pēdiņas
frame_majo[, adrese := gsub('""', '"', adrese)]

# Sample
sample_majo <- frame_majo[
  sample_total == 1L,
  .(
    id = adr_kods,
    str_expl,
    str_impl,
    pil_lauk,
    pil_lauk_name,
    NUTS3_code,
    NUTS3_name,
    L0_code,
    L0_name,
    L1_code,
    L1_name,
    apk_code,
    apk_name,
    adr_kods_eka,
    koord_x,
    koord_y,
    dd_e,
    dd_n,
    adr_kods,
    tips_cd,
    adrese,
    pers_sk_1859,
    size,
    sample_total,
    pik_total,
    dw_st1_total,
    sample_main,
    pik_main,
    dw_st1_main,
    sample_res
  )
]

# Save
fwrite(
  x = sample_majo,
  file = file.path(config::get("dir.data"), "GGS-LV-2025-sample.csvy.gz"),
  yaml = TRUE
)
fwrite(
  x = sample_majo,
  file = file.path(config::get("dir.data"), "GGS-LV-2025-sample.csv")
)
openxlsx2::write_xlsx(
  x = sample_majo,
  file = file.path(config::get("dir.data"), "GGS-LV-2025-sample.xlsx"),
  first_row = TRUE,
  with_filter = TRUE,
  widths = "auto",
  overwrite = TRUE
)
