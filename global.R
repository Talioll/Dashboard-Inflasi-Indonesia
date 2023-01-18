# -----------------------------------------------LIBRARY PREPARATION
library(readxl)
library(tidyverse)
library(webshot)
library(tsibble)
library(lubridate)
library(reshape2)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(glue)
library(DT)


#----------------------------------------------- DATA PREPARATION


# ------------------------------------------- DATA INFLASI BI------------------
inflasi <- read_xlsx("BPS Dataset/Data Inflasi.xlsx", skip = 4)

inflasi <- inflasi %>% 
  rename(dataInflasi = `Data Inflasi`) %>% 
  mutate(Year = str_split(Periode, " ", simplify = T)[,2],
         Month = str_split(Periode, " ", simplify = T)[,1],
         Month = factor(Month, 
                        levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember")),
         dataInflasi = str_replace(dataInflasi, "%", ""),
         Year = as.factor(Year),
         dataInflasi = as.numeric(dataInflasi)) %>% 
  select(Periode, Year, Month, dataInflasi) %>% 
  arrange(desc(Year), Month)

inflasi$Month <- factor(inflasi$Month,
                        levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))

raiseInflasiSub <- inflasi %>%
  summarise(`Naik/Turun Inflasi` = dataInflasi[Year == "2022" & Month == "Februari"] - dataInflasi[Year == "2022" & Month == "Januari"]) %>%
  mutate(`Dari Tahun` = "2022",
         `Dari Bulan` = "Januari", 
         `Ke Tahun` = "2022",
         `Ke Bulan` = "") %>% 
  select(`Dari Tahun`,`Dari Bulan`, `Ke Tahun`, `Ke Bulan`, `Naik/Turun Inflasi`)





#--------------------------------Transportasi-----------------------------------

#--------Transportasi 2022
Transportasi2022 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 06 Transportasi (2).xlsx", skip = 1,n_max = 93))
Transportasi2022 <- t(Transportasi2022)
Transportasi2022 <- data.frame(Transportasi2022)
Transportasi2022[1,1] <- "Year"
Transportasi2022[1,2] <-  "Month"
Transportasi2022 <- rownames_to_column(Transportasi2022)
names(Transportasi2022) <- Transportasi2022[1, ]
Transportasi2022 <-  Transportasi2022[-1, ]
Transportasi2022 <- Transportasi2022 %>% 
  rename(Category = ...1)

Transportasi2022[2:13,1] <- "Transportasi (Summary All Cat)"
Transportasi2022[,2] <- "2022"
Transportasi2022[14:26,1] <- "Pembelian Kendaraan"
Transportasi2022[27:39, 1] <-  "PPTP"
Transportasi2022[41:52, 1] <- "Jasa Angkutan Penumpang"
Transportasi2022[54:65, 1] <- "Jasa Pengiriman Barang"
colnames(Transportasi2022) <- str_replace_all(colnames(Transportasi2022), "KOTA ", "")
colnames(Transportasi2022) <- str_to_title(colnames(Transportasi2022))


#---------Transportasi 2021
Transportasi2021 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 06 Transportasi (1).xlsx", skip = 1,n_max = 93))
Transportasi2021 <- t(Transportasi2021)
Transportasi2021 <- data.frame(Transportasi2021)
Transportasi2021[1,1] <- "Year"
Transportasi2021[1,2] <-  "Month"
Transportasi2021 <- rownames_to_column(Transportasi2021)
names(Transportasi2021) <- Transportasi2021[1, ]
Transportasi2021 <-  Transportasi2021[-1, ]
Transportasi2021 <- Transportasi2021 %>% 
  rename(Category = ...1)

Transportasi2021[2:13,1] <- "Transportasi (Summary All Cat)"
Transportasi2021[,2] <- "2021"
Transportasi2021[14:26,1] <- "Pembelian Kendaraan"
Transportasi2021[27:39, 1] <-  "PPTP"
Transportasi2021[41:52, 1] <- "Jasa Angkutan Penumpang"
Transportasi2021[54:65, 1] <- "Jasa Pengiriman Barang"
colnames(Transportasi2021) <- str_replace_all(colnames(Transportasi2021), "KOTA ", "")
colnames(Transportasi2021) <- str_to_title(colnames(Transportasi2021))



#----------Transportasi 2020
Transportasi2020 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 06 Transportasi.xlsx", skip = 1, n_max = 93))
Transportasi2020 <- t(Transportasi2020)
Transportasi2020 <- data.frame(Transportasi2020)
Transportasi2020[1,1] <- "Year"
Transportasi2020[1,2] <-  "Month"
Transportasi2020 <- rownames_to_column(Transportasi2020)
names(Transportasi2020) <- Transportasi2020[1, ]
Transportasi2020 <-  Transportasi2020[-1, ]
Transportasi2020 <- Transportasi2020 %>% 
  rename(Category = ...1)

Transportasi2020[2:13,1] <- "Transportasi (Summary All Cat)"
Transportasi2020[,2] <- "2020"
Transportasi2020[14:26,1] <- "Pembelian Kendaraan"
Transportasi2020[27:39, 1] <-  "PPTP"
Transportasi2020[41:52, 1] <- "Jasa Angkutan Penumpang"
Transportasi2020[54:65, 1] <- "Jasa Pengiriman Barang"
colnames(Transportasi2020) <- str_replace_all(colnames(Transportasi2020), "KOTA ", "")
colnames(Transportasi2020) <- str_to_title(colnames(Transportasi2020))


# ---------- Merge all year Transportation

all.transportasi <- rbind(Transportasi2022, Transportasi2021, Transportasi2020)
#---- change data types
all.transportasi[, 1:3] <- lapply(all.transportasi[, 1:3], as.factor)
all.transportasi[, 4:94] <- lapply(all.transportasi[, 4:94], as.numeric)
all.transportasi[is.na(all.transportasi)] <- 0
all.transportasi <- all.transportasi[all.transportasi$Month != "Tahunan",] 
all.transportasi$Month <- factor(all.transportasi$Month,
                                 levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))


#--------------------------------Restoran-------------------------------------

#-------------Restoran 2022
Resto2022 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 10 Penyediaan Makanan dan Minuman _ Restoran (2).xlsx", skip = 1, n_max = 93))
Resto2022 <- t(Resto2022)
Resto2022 <- data.frame(Resto2022)
Resto2022[1,1] <- "Year"
Resto2022[1,2] <-  "Month"
Resto2022 <- rownames_to_column(Resto2022)
names(Resto2022) <- Resto2022[1, ]
Resto2022 <-  Resto2022[-1, ]
Resto2022 <- Resto2022 %>% 
  rename(Category = ...1)

Resto2022[,2] <- "2022"
Resto2022[14:26,1] <- "Jasa Pelayanan Makanan & Minuman"
Resto2022 <- Resto2022[-(1:13),]
colnames(Resto2022) <- str_replace_all(colnames(Resto2022), "KOTA ", "")
colnames(Resto2022) <- str_to_title(colnames(Resto2022))



#--------------Restoran 2021
Resto2021 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 10 Penyediaan Makanan dan Minuman _ Restoran (1).xlsx", skip = 1, n_max = 93))
Resto2021 <- t(Resto2021)
Resto2021 <- data.frame(Resto2021)
Resto2021[1,1] <- "Year"
Resto2021[1,2] <-  "Month"
Resto2021 <- rownames_to_column(Resto2021)
names(Resto2021) <- Resto2021[1, ]
Resto2021 <-  Resto2021[-1, ]
Resto2021 <- Resto2021 %>% 
  rename(Category = ...1)

Resto2021[,2] <- "2021"
Resto2021[14:26,1] <- "Jasa Pelayanan Makanan & Minuman"
Resto2021 <- Resto2021[-(1:13),]
colnames(Resto2021) <- str_replace_all(colnames(Resto2021), "KOTA ", "")
colnames(Resto2021) <- str_to_title(colnames(Resto2021))



#---------------Restoran 2020
Resto2020 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 10 Penyediaan Makanan dan Minuman _ Restoran.xlsx", skip = 1, n_max = 93))
Resto2020 <- t(Resto2020)
Resto2020 <- data.frame(Resto2020)
Resto2020[1,1] <- "Year"
Resto2020[1,2] <-  "Month"
Resto2020 <- rownames_to_column(Resto2020)
names(Resto2020) <- Resto2020[1, ]
Resto2020 <-  Resto2020[-1, ]
Resto2020 <- Resto2020 %>% 
  rename(Category = ...1)

Resto2020[,2] <- "2020"
Resto2020[14:26,1] <- "Jasa Pelayanan Makanan & Minuman"
Resto2020 <- Resto2020[-(1:13),]
colnames(Resto2020) <- str_replace_all(colnames(Resto2020), "KOTA ", "")
colnames(Resto2020) <- str_to_title(colnames(Resto2020))



# ------------------- MERGE ALL YEAR Resto inflasi
all.resto <- rbind(Resto2022, Resto2021, Resto2020)
#---- change data types
all.resto[, 1:3] <- lapply(all.resto[, 1:3], as.factor)
all.resto[, 4:94] <- lapply(all.resto[, 4:94], as.numeric)
all.resto[is.na(all.resto)] <- 0
all.resto <- all.resto[all.resto$Month != "Tahunan",] 
all.resto$Month <- factor(all.resto$Month,
                          levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))


#--------------------------Perumahan----------------------------------------


#--------------------------Perumahan 2022
Perumahan2022 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 03 Perumahan, Air, Listrik dan Bahan Bakar Rumah Tangga (2).xlsx", skip = 1, n_max = 93))
Perumahan2022 <- t(Perumahan2022)
Perumahan2022 <- data.frame(Perumahan2022)
Perumahan2022[1,1] <- "Year"
Perumahan2022[1,2] <-  "Month"
Perumahan2022 <- rownames_to_column(Perumahan2022)
names(Perumahan2022) <- Perumahan2022[1, ]
Perumahan2022 <-  Perumahan2022[-1, ]
Perumahan2022 <- Perumahan2022 %>% 
  rename(Category = ...1)

Perumahan2022[1:13,1] <-  "Perumahan (Summary All Cat)"
Perumahan2022[,2] <- "2022"
Perumahan2022[14:26,1] <- "Sewa & Kontrak Rumah"
Perumahan2022[28:39, 1] <- "Pemeliharaan, Perbaikan dan Keamanan"
Perumahan2022[41:52, 1] <- "Penyediaan Air dan Layanan Perumahan Lainnya"
Perumahan2022[54:65, 1] <- "Listrik dan Bahan Bakar Rumah Tangga"
colnames(Perumahan2022) <- str_replace_all(colnames(Perumahan2022), "KOTA ", "")
colnames(Perumahan2022) <- str_to_title(colnames(Perumahan2022))




#-------------Perumahan 2021
Perumahan2021 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 03 Perumahan, Air, Listrik dan Bahan Bakar Rumah Tangga (1).xlsx", skip = 1, n_max = 93))
Perumahan2021 <- t(Perumahan2021)
Perumahan2021 <- data.frame(Perumahan2021)
Perumahan2021[1,1] <- "Year"
Perumahan2021[1,2] <-  "Month"
Perumahan2021 <- rownames_to_column(Perumahan2021)
names(Perumahan2021) <- Perumahan2021[1, ]
Perumahan2021 <-  Perumahan2021[-1, ]
Perumahan2021 <- Perumahan2021 %>% 
  rename(Category = ...1)

Perumahan2021[1:13,1] <- "Perumahan (Summary All Cat)"
Perumahan2021[,2] <- "2021"
Perumahan2021[14:26,1] <- "Sewa & Kontrak Rumah"
Perumahan2021[28:39, 1] <- "Pemeliharaan, Perbaikan dan Keamanan"
Perumahan2021[41:52, 1] <- "Penyediaan Air dan Layanan Perumahan Lainnya"
Perumahan2021[54:65, 1] <- "Listrik dan Bahan Bakar Rumah Tangga"
colnames(Perumahan2021) <- str_replace_all(colnames(Perumahan2021), "KOTA ", "")
colnames(Perumahan2021) <- str_to_title(colnames(Perumahan2021))




#-------------Perumahan 2020
Perumahan2020 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok 03 Perumahan, Air, Listrik dan Bahan Bakar Rumah Tangga.xlsx", skip = 1, n_max = 93))
Perumahan2020 <- t(Perumahan2020)
Perumahan2020 <- data.frame(Perumahan2020)
Perumahan2020[1,1] <- "Year"
Perumahan2020[1,2] <-  "Month"
Perumahan2020 <- rownames_to_column(Perumahan2020)
names(Perumahan2020) <- Perumahan2020[1, ]
Perumahan2020 <-  Perumahan2020[-1, ]
Perumahan2020 <- Perumahan2020 %>% 
  rename(Category = ...1)

Perumahan2020[1:13,1] <- "Perumahan (Summary All Cat)"
Perumahan2020[,2] <- "2020"
Perumahan2020[14:26,1] <- "Sewa & Kontrak Rumah"
Perumahan2020[28:39, 1] <- "Pemeliharaan, Perbaikan dan Keamanan"
Perumahan2020[41:52, 1] <- "Penyediaan Air dan Layanan Perumahan Lainnya"
Perumahan2020[54:65, 1] <- "Listrik dan Bahan Bakar Rumah Tangga"
colnames(Perumahan2020) <- str_replace_all(colnames(Perumahan2020), "KOTA ", "")
colnames(Perumahan2020) <- str_to_title(colnames(Perumahan2020))





# ------------------- MERGE ALL YEAR Perumahan inflasi
all.perumahan <- rbind(Perumahan2022, Perumahan2021, Perumahan2020)
#-change data types
all.perumahan[, 1:3] <- lapply(all.perumahan[, 1:3], as.factor)
all.perumahan[, 4:94] <- lapply(all.perumahan[, 4:94], as.numeric)
all.perumahan[is.na(all.perumahan)] <- 0
all.perumahan <- all.perumahan[all.perumahan$Month != "Tahunan",] 
all.perumahan$Month <- factor(all.perumahan$Month,
                              levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))





# -------------------Makanan, Minuman dan Tembakau------------------------

#--------Makanan, Minuman dan Tembakau 2022
TembakauAlko2022 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok  01 Makanan, Minuman dan Tembakau (2).xlsx", skip = 1, n_max = 93))
TembakauAlko2022 <- t(TembakauAlko2022)
TembakauAlko2022 <- data.frame(TembakauAlko2022)
TembakauAlko2022[1,1] <- "Year"
TembakauAlko2022[1,2] <-  "Month"
TembakauAlko2022 <- rownames_to_column(TembakauAlko2022)
names(TembakauAlko2022) <- TembakauAlko2022[1, ]
TembakauAlko2022 <-  TembakauAlko2022[-1, ]
TembakauAlko2022 <- TembakauAlko2022 %>% 
  rename(Category = ...1)

TembakauAlko2022[1:13,1] <- "Makanan, Minuman & Tembakau (Summary All Cat)"
TembakauAlko2022[,2] <- "2022"
TembakauAlko2022[14:26,1] <- "Makanan"
TembakauAlko2022[28:39, 1] <- "Minuman Non-Alkohol"
TembakauAlko2022[41:52, 1] <- "Minuman Beralkohol"
TembakauAlko2022[54:65, 1] <- "Rokok/Tembakau"
colnames(TembakauAlko2022) <- str_replace_all(colnames(TembakauAlko2022), "KOTA ", "")
colnames(TembakauAlko2022) <- str_to_title(colnames(TembakauAlko2022))




#--------------------Makanan, Minuman dan Tembakau 2021
TembakauAlko2021 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok  01 Makanan, Minuman dan Tembakau (1).xlsx", skip = 1, n_max = 93))
TembakauAlko2021 <- t(TembakauAlko2021)
TembakauAlko2021 <- data.frame(TembakauAlko2021)
TembakauAlko2021[1,1] <- "Year"
TembakauAlko2021[1,2] <-  "Month"
TembakauAlko2021 <- rownames_to_column(TembakauAlko2021)
names(TembakauAlko2021) <- TembakauAlko2021[1, ]
TembakauAlko2021 <-  TembakauAlko2021[-1, ]
TembakauAlko2021 <- TembakauAlko2021 %>% 
  rename(Category = ...1)

TembakauAlko2021[1:13,1] <- "Makanan, Minuman & Tembakau (Summary All Cat)"
TembakauAlko2021[,2] <- "2021"
TembakauAlko2021[14:26,1] <- "Makanan"
TembakauAlko2021[28:39, 1] <- "Minuman Non-Alkohol"
TembakauAlko2021[41:52, 1] <- "Minuman Beralkohol"
TembakauAlko2021[54:65, 1] <- "Rokok/Tembakau"
colnames(TembakauAlko2021) <- str_replace_all(colnames(TembakauAlko2021), "KOTA ", "")
colnames(TembakauAlko2021) <- str_to_title(colnames(TembakauAlko2021))





#------------------Makanan, Minuman dan Tembakau 2020
TembakauAlko2020 <- as.data.frame(read_xlsx("BPS Dataset/Inflasi Tahun Kalender (2018=100) Menurut Kelompok dan Sub Kelompok  01 Makanan, Minuman dan Tembakau.xlsx", skip = 1, n_max = 93))
TembakauAlko2020 <- t(TembakauAlko2020)
TembakauAlko2020 <- data.frame(TembakauAlko2020)
TembakauAlko2020[1,1] <- "Year"
TembakauAlko2020[1,2] <-  "Month"
TembakauAlko2020 <- rownames_to_column(TembakauAlko2020)
names(TembakauAlko2020) <- TembakauAlko2020[1, ]
TembakauAlko2020 <-  TembakauAlko2020[-1, ]
TembakauAlko2020 <- TembakauAlko2020 %>% 
  rename(Category = ...1)

TembakauAlko2020[1:13,1] <- "Makanan, Minuman & Tembakau (Summary All Cat)"
TembakauAlko2020[,2] <- "2020"
TembakauAlko2020[14:26,1] <- "Makanan"
TembakauAlko2020[28:39, 1] <- "Minuman Non-Alkohol"
TembakauAlko2020[41:52, 1] <- "Minuman Beralkohol"
TembakauAlko2020[54:65, 1] <- "Rokok/Tembakau"
colnames(TembakauAlko2020) <- str_replace_all(colnames(TembakauAlko2020), "KOTA ", "")
colnames(TembakauAlko2020) <- str_to_title(colnames(TembakauAlko2020))



# ------------------- MERGE All YEAR Tembakau inflasi
all.tembakau <- rbind(TembakauAlko2022, TembakauAlko2021, TembakauAlko2020)
#---- change data types
all.tembakau[, 1:3] <- lapply(all.tembakau[, 1:3], as.factor)
all.tembakau[, 4:94] <- lapply(all.tembakau[, 4:94], as.numeric)
all.tembakau[is.na(all.tembakau)] <- 0
all.tembakau <- all.tembakau[all.tembakau$Month != "Tahunan",] 
all.tembakau$Month <- factor(all.tembakau$Month,
                             levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))



#------------------------------- MERGE ALL CATEGORY--------------------------
allSub <- rbind(all.perumahan, all.transportasi, all.resto, all.tembakau)
allSub



df4pivot <- allSub %>% 
  select(-(Indonesia))

allSubPivot <- pivot_longer(df4pivot, 
                            cols = -c(Category, Year, Month), 
                            names_to = "Kota", 
                            values_to = "Inflasi")


# LIST of Summary Category, Kota

category.summary <- c("Perumahan (Summary All Cat)", "Transportasi (Summary All Cat)", "Jasa Pelayanan Makanan & Minuman", "Makanan, Minuman & Tembakau (Summary All Cat)" )


Kota <- names(allSub[4:94])


detailCtgList <- unique(allSub$Category[allSub$Category !=  "Perumahan (Summary All Cat)" &
                                          allSub$Category != "Transportasi (Summary All Cat)" &
                                          allSub$Category != "Makanan, Minuman & Tembakau (Summary All Cat)"])
listPeriode <- unique(inflasi$Periode)

yearlist <- unique(inflasi$Year)
MonthList <- unique(inflasi$Month)
year3 <- unique(allSub$Year)












#-----------------------------------------------PLOTTING

