library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)

resp_jambi <-GET("https://data.covid19.go.id/public/api/prov_detail_JAMBI.json")
cov_jambi_raw <- content(resp_jambi, as = "parsed", simplifyVector = TRUE)


cov_jambi <- cov_jambi_raw$list_perkembangan
str(cov_jambi)
head(cov_jambi)

new_cov_jambi <-
  cov_jambi %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jambi)
 
ggplot(new_cov_jambi, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

ggplot(new_cov_jambi, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jambi",
    subtitle = "Terjadi pelonjakan kasus di awal bulan Oktober akibat klaster Secapa Kota Jambi",
    caption = "Sumber data: covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 10,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position = "plot")

ggplot(new_cov_jambi, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jambi",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 10, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

ggplot(new_cov_jambi, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jambi",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 10, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

cov_jambi_pekanan <- new_cov_jambi %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jambi_pekanan)

cov_jambi_pekanan <-
  cov_jambi_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jambi_pekanan)

ggplot(cov_jambi_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik))+
  geom_col(show.legend = FALSE)+
  scale_x_continuous(breaks = 9:29, expand = c(0,0))+
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon"))+
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jambi",
    subtitle = "Kolom Hijau menunjukkan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position = "plot")

cov_jambi_akumulasi <- 
  new_cov_jambi %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jambi_akumulasi)

ggplot(data = cov_jambi_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()


dim(cov_jambi_akumulasi)

cov_jambi_akumulasi_pivot <- 
  cov_jambi_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_jambi_akumulasi_pivot)

glimpse(cov_jambi_akumulasi_pivot)


ggplot(cov_jambi_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c("aktif" = "salmon","meninggal" = "darkslategray4","sembuh" = "olivedrab2"),
    labels = c("Aktif", "Meninggal", "Sembuh")) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jambi",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
