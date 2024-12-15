library(readr)
asf <- read.csv2("D:/015_africanswinefever_rf/003_sm_asf/data/asf_cleandate01.csv")
kabupaten_borneo01 <- read.csv2("D:/013_text_mining/kalimantan_bow/kabupaten_bow.csv") 
kabupaten_borneo02 <- readLines("D:/013_text_mining/kalimantan_bow/kabupaten_bow.txt")

View(asf)
Encoding(asf$news_content)

View(kabupaten_borneo02)
print(kabupaten_borneo02)

names(asf)
asf <- read.csv2("D:/015_africanswinefever_rf/003_sm_asf/data/asf_cleandate01.csv")
content_asf <- asf$news_content
typeof(content_asf)

install.packages("tidyr")
library(dplyr)       #manipulasi data
library(stringr)     #manipulasi teks
library(tidytext)    #olahdata
library(magrittr)    #aktivasi pipe (fungsi dalam fungsi)
library(stringi)     #manipulasi dan pengolahan string
library(purrr)      #deteksi kesalahan atau nilai NA
library(tidyr)      #megubah format data



#standardization teks pada kolom
content_standard <- 
  content_asf %>%
  str_to_lower() %>%                            # Mengubah teks menjadi huruf kecil
  str_replace_all("[[:punct:]]", "") %>%         # Menghapus tanda baca
  stri_replace_all_regex("\\d+", "") %>%         # Menghapus nomor dengan stringi
  stri_replace_all_regex("[^[:print:]]", "") %>% # Menghapus karakter non-printable dengan stringi
  str_trim()                                    # Mengurangi spasi berlebih
print(content_standard)

# Buat kolom baru untuk menyimpan hasil pencarian kabupaten
asf$kabupaten01 <- NA

# Loop untuk setiap baris di content_standard
for (i in seq_along(content_standard)) {
  # Lewati jika content_standard[i] adalah NA
  if (is.na(content_standard[i])) next
  
  # Cari kabupaten yang cocok di content_standard[i]
  matches <- kabupaten_borneo02[!is.na(kabupaten_borneo02) & str_detect(content_standard[i], kabupaten_borneo02)]
  
  # Jika ditemukan kecocokan, simpan hasil di kabupaten01
  if (length(matches) > 0) {
    asf$kabupaten01[i] <- paste(matches, collapse = ", ")  # Gabungkan jika ada beberapa kecocokan
  }
}

# Memisahkan kabupaten01 menjadi baris baru jika ada lebih dari satu kabupaten
asf_expanded <- asf %>%
  separate_rows(kabupaten01, sep = ",\\s*") %>% # Pisahkan kabupaten01 menjadi baris baru
  mutate(kabupaten01 = str_trim(kabupaten01))  # Trim spasi tambahan di setiap baris

# Hasil akhir
asf_final <- asf_expanded %>%
  select(date_publish, province, source_web, url, key_word, date_access, species, kabupaten01, quantity, news_publish, news_title, news_content, clean_date01)

# Cetak hasil
print(asf_final)
View(asf_final)


write.csv2(asf_final,"D:/015_africanswinefever_rf/003_sm_asf/data/asf_kabupaten01.csv")
asf_kabupaten01 <- read.csv2("D:/015_africanswinefever_rf/003_sm_asf/data/asf_kabupaten01.csv")

View(asf_kabupaten01)

# Count frequency news of asf outbreak by kabupaten

kabupaten_freq <- asf_kabupaten01 %>%
  count(kabupaten01) %>%
  mutate(persen = n / sum(n) * 100) %>%
  arrange(desc(n))  # Mengurutkan berdasarkan frekuensi (n) secara menurun

kabupaten_freq

##############analisa frequency asf outbreak ###################################


# Menghitung frekuensi 'domestic' dan 'wild' untuk setiap kabupaten
species_comparison <- asf_kabupaten01 %>%
  group_by(kabupaten01, species) %>%         # Mengelompokkan berdasarkan kabupaten dan species
  count() %>%                               # Menghitung jumlah setiap kombinasi kabupaten dan species
  spread(species, n, fill = 0) %>%           # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild,            # Menambahkan kolom total frekuensi
         domestic_pct = domestic / total * 100,  # Menghitung persentase 'domestic'
         wild_pct = wild / total * 100)      # Menghitung persentase 'wild'

# Menampilkan hasil
print(species_comparison)

# Memuat paket yang diperlukan
library(dplyr)
library(lubridate)

# Menghitung frekuensi 'domestic' dan 'wild' berdasarkan tahun pada kolom clean_date01
species_comparison_yearly <- asf_kabupaten01 %>%
  mutate(year = year(clean_date01)) %>%           # Ekstrak tahun dari clean_date01
  group_by(year, species) %>%                     # Mengelompokkan berdasarkan tahun dan species
  count() %>%                                     # Menghitung jumlah setiap kombinasi tahun dan species
  spread(species, n, fill = 0) %>%                # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild,                 # Menambahkan kolom total frekuensi
         domestic_pct = domestic / total * 100,   # Menghitung persentase 'domestic'
         wild_pct = wild / total * 100)           # Menghitung persentase 'wild

# Memuat paket yang diperlukan
library(dplyr)
library(lubridate)
library(ggplot2)

# Menghitung frekuensi 'domestic' dan 'wild' berdasarkan tahun
species_comparison_yearly <- asf_kabupaten01 %>%
  mutate(year = year(clean_date01)) %>%           # Ekstrak tahun dari clean_date01
  group_by(year, species) %>%                     # Mengelompokkan berdasarkan tahun dan species
  count() %>%                                     # Menghitung jumlah setiap kombinasi tahun dan species
  spread(species, n, fill = 0) %>%                 # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild)                 # Menambahkan kolom total frekuensi

species_comparison_yearly

# Membuat grafik perbandingan jumlah frekuensi 'domestic' dan 'wild' berdasarkan tahun
ggplot(species_comparison_yearly, aes(x = as.factor(year))) +
  geom_bar(aes(y = domestic, fill = "Domestic"), stat = "identity", position = position_dodge(width = 0.9)) +  # Grafik batang untuk 'domestic'
  geom_bar(aes(y = wild, fill = "Wild"), stat = "identity", position = position_dodge(width = 0.9)) +  # Grafik batang untuk 'wild'
  geom_text(aes(y = domestic, label = domestic, group = "Domestic"), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Menambahkan label untuk 'domestic'
  geom_text(aes(y = wild, label = wild, group = "Wild"), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Menambahkan label untuk 'wild'
  labs(x = "Year", y = "Frequency", fill = "Species") +  # Label sumbu dan legenda
  scale_x_discrete(name = "Year") +  # Menambahkan nama sumbu x sebagai kategori
  theme_minimal() +  # Tema minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Memiringkan teks pada sumbu x agar mudah dibaca





