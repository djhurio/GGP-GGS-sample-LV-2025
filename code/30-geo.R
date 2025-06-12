# Ģeneralizētas teritoriālo vienību robežas
# Territorial_units_LV_1.2m_(2024.01.01.).zip
# https://data.gov.lv/dati/dataset/robezas/resource/49c1b275-efd5-4784-af87-5182397bd453

# Lejuplāde un atarhivēšana
download.file(
  url = file.path(
    "https://data.gov.lv/dati/dataset/e3d606f2-6d38-444d-b767-119fdcc435fe",
    "resource/49c1b275-efd5-4784-af87-5182397bd453",
    "download/territorial_units_lv_1.2m_2024.01.01..zip"
  ),
  destfile = file.path(
    config::get("dir.data"),
    "territorial_units_lv_1.2m.zip"
  )
)

unzip(
  zipfile = file.path(
    config::get("dir.data"),
    "territorial_units_lv_1.2m.zip"
  ),
  exdir = file.path(
    config::get("dir.data")
  )
)


# Pilsētu apkaimes

# Pilsētu apkaimju robežas, kas papildinātas ar CSP statistisko datu publicēšanā izmantotajiem teritoriju kodiem.
# - Rīgai izmantoti Latvijas Atvērto datu portālā publicētie dati,
# - Daugavpilij pašvaldības sniegti dati (robežas atbilst teritorijas plānojumā attēlotajām),
# - Jūrmalai pašvaldības sniegti dati,
# - Valmierai pašvaldības sniegti dati (robežas lielākoties sakrīt ar NĪVKIS zemes vienību robežām 07.01.2020.).
# Pilsētu ārējās robežas atbilstoši situācijai 01.01.2024.

# Lejuplāde un atarhivēšana
download.file(
  url = file.path(
    "https://data.gov.lv/dati/dataset/ecab8e2e-adac-4dae-a4ce-d41e09b58aaf",
    "resource/b46124a9-e780-4bc8-8b37-2271fb0d8e6e",
    "download/apkaimes.zip"
  ),
  destfile = file.path(
    config::get("dir.data"),
    "apkaimes.zip"
  )
)

dir.create(
  file.path(
    config::get("dir.data"),
    "apkaimes"
  ),
  showWarnings = FALSE
)

unzip(
  zipfile = file.path(
    config::get("dir.data"),
    "apkaimes.zip"
  ),
  exdir = file.path(
    config::get("dir.data"),
    "apkaimes"
  )
)
