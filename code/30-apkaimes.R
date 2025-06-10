# Pilsētu apkaimes

# Pilsētu apkaimju robežas, kas papildinātas ar CSP statistisko datu publicēšanā izmantotajiem teritoriju kodiem.
# - Rīgai izmantoti Latvijas Atvērto datu portālā publicētie dati,
# - Daugavpilij pašvaldības sniegti dati (robežas atbilst teritorijas plānojumā attēlotajām),
# - Jūrmalai pašvaldības sniegti dati,
# - Valmierai pašvaldības sniegti dati (robežas lielākoties sakrīt ar NĪVKIS zemes vienību robežām 07.01.2020.).
# Pilsētu ārējās robežas atbilstoši situācijai 01.01.2024.

# Lejuplāde un atarhivēšana
download.file(
  url = "https://data.gov.lv/dati/dataset/ecab8e2e-adac-4dae-a4ce-d41e09b58aaf/resource/b46124a9-e780-4bc8-8b37-2271fb0d8e6e/download/apkaimes.zip",
  destfile = file.path(config::get("dir.data"), "apkaimes.zip")
)

dir.create(file.path(config::get("dir.data"), "apkaimes"), showWarnings = FALSE)

unzip(
  zipfile = file.path(config::get("dir.data"), "apkaimes.zip"),
  exdir = file.path(config::get("dir.data"), "apkaimes")
)
