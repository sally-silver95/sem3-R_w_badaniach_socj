# Laboratorium 1 w RStudio: czy Polacy ufają Sejmowi?
# Imię i nazwisko: Sylwia Szuszkiewicz
# Zapisz ten plik jako lab_Nazwisko.R i oddaj go w Moodle.
#
# Pod każdym pytaniem wpisz kod, a pod kodem jedno zdanie odpowiedzi w komentarzu.
# Pytania 1-7 na zajęciach, 8-10 w domu. Treść pytań:
# https://i-sulzhi.github.io/r-krok-po-kroku/lab/

# Dane: Adapted from ESS Rounds 1-11 (European Social Survey ERIC), licencja CC BY-NC-SA 4.0.
# Plik ess_polska.sav to tylko Polska i 18 z 2747 zmiennych; wartości bez zmian.
# Pełne dane są bezpłatne na https://ess.sikt.no. Kurs nie jest związany z ESS ERIC.

# Jeśli pakiet nie jest zainstalowany, uruchom raz w konsoli:
# install.packages(c("tidyverse", "haven"))
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

ess_polska <- read_sav("lab1-rstudio/ess_polska.sav")

# ---- Pytanie 1. Ile osób zbadano? ------------------------------------------------

nrow(ess_polska)
ncol(ess_polska)
ess_polska |> group_by(essround) |> summarise(licz_osob = n()) #liczba osob w kazdej rundzie
ess23 <- ess_polska |> filter(essround == 11)
nrow(ess23)

# Odpowiedź: Jest 19131 osob w tabeli i 18 kolumn. W 2023 roku było 1442 osob.


# ---- Pytanie 2. Ile kobiet i mężczyzn? -------------------------------------------

ess23$plec <- factor(ess23$gndr, levels = 1:2, labels = c("Mężczyzna", "Kobieta"))
sum(ess23$plec == "Mężczyzna")
sum(ess23$plec == "Kobieta")

# Odpowiedź: 675 mężczyzn i 767 kobiet


# ---- Pytanie 3. Jak bardzo Polacy ufają Sejmowi? ---------------------------------

mean(ess23$trstprl, na.rm = TRUE)
sum(is.na(ess23$trstprl))

# Odpowiedź: średnie zaufanie do Sejmu: 3.910905 w skali od 0 do 10, 39 osob nie odpowiedzialo. 
# Na początku wychodzi NA bo w tabeli przez które nie da sie wyliczyć średniej


# ---- Pytanie 4. Czy kobiety i mężczyźni inaczej ufają policji? -------------------

ess23 |> 
  group_by(plec) |> 
  summarise(sr_policja = mean(trstlgl, na.rm = TRUE), licz_odp = sum(trstlgl, na.rm = TRUE))

# Odpowiedź: kobiety bardziej ufają policji (ich srednia to 4.15 a u mężczyzn to 3.97) 
#   plec      sr_policja licz_odp
#1 Mężczyzna       3.97     2612
#2 Kobieta         4.15     3036


# ---- Pytanie 5. Kto ufa Sejmowi? -------------------------------------------------

ess23 |>
    mutate(gr_wiekowa = case_when(
      agea >= 15 & agea <= 29 ~ "15-29",
      agea >= 30 & agea <= 44 ~ "30-44",
      agea >= 45 & agea <= 59 ~ "45-59",
      agea >= 60 ~ "60+",
      is.na(agea) ~ "brak danych"
    )) |>
   mutate(
    ufa_sejm = if_else(trstprl >= 6 & trstprl <= 10, TRUE, FALSE)     
    ) |>
    count(gr_wiekowa, ufa_sejm) |>
    mutate(procent = n / sum(n) * 100)
  
sum(is.na(ess23$agea))

# Odpowiedź: 15-29 lat - 3,4%, 30-44 - 4,92%, 45-59 - 5,41% i 60+ - 9,02%


# ---- Pytanie 6. Zadowolenie z życia: wiek i płeć ---------------------------------

ess23 |>
  mutate(gr_wiekowa = case_when(
    agea >= 15 & agea <= 29 ~ "15-29",
    agea >= 30 & agea <= 44 ~ "30-44",
    agea >= 45 & agea <= 59 ~ "45-59",
    agea >= 60 ~ "60+",
    is.na(agea) ~ "brak danych"
  )) |>
  group_by(gr_wiekowa, plec) |>
  summarise(sr_stflife = mean(stflife, na.rm = TRUE)) |>
  pivot_wider(names_from = plec, values_from = sr_stflife)

# Odpowiedź: Najniższe zadowolenie z życia mają kobiety z przedzialu wiekowego 15-29 (7,16), 
#            a najwyższe mają kobiety z przedziału 30-44 (7,65)


# ---- Pytanie 7. Której instytucji ufamy najbardziej? -----------------------------

ess23 |>
  pivot_longer(trstprl:trstun, names_to = "instytucja", values_to = "zaufanie") |>
  group_by(instytucja, plec) |>
  summarise(srednia = mean(zaufanie, na.rm = TRUE)) |>
  arrange(desc(srednia))

# Odpowiedź: Mężczyźni i kobiety maja tendencje do ufania najbardziej tym samym instytujom (policji i ONZ - mają znaczonco wyzsze
# zaufanie od reszty instytucji). Kobiety zazwyczaj maja wiekszą średnia zaufania do danej instytucji niż mężczyźni (wyjątkiem jest zaufanie do sejmu)


# ---- Pytanie 8. Zaufanie do Sejmu w województwach: 2018 i 2023 (w domu) ----------

regiony <- read.csv("lab1-rstudio/regiony.csv")

ess_z_woj <- ess_polska |>
  left_join(regiony, by = "region")

sr_2018 <- ess_z_woj |> 
  filter(essround == 9) |> 
  group_by(wojewodztwo) |>
  summarise(sr_2018 = mean(trstprl, na.rm = TRUE), number_2018 = sum(!is.na(trstprl)))

sr_2023 <- ess_z_woj |> 
  filter(essround == 11) |> 
  group_by(wojewodztwo) |>
  summarise(sr_2023 = mean(trstprl, na.rm = TRUE), number_2023 = sum(!is.na(trstprl)))

porownanie_18_23 <- sr_2018 |>
  left_join(sr_2023, by = "wojewodztwo") |>
  mutate(roznica = sr_2023 - sr_2018) |>
  arrange(desc(roznica))

porownanie_18_23

# Odpowiedź: Część regionów napoczatku nie miaal pary ponieważ pomiedzy dbadaniami zmienily sie niektore oznaczenia (PL'numer')
# wojewodztw się zmieniły. Zaufanie spadło najbardziej w wojewodztwie Podlaskim (o 1,63 punkta), a najbardziej wzrosło w 
# województwie Warmińsko-mazurksim (o 1,36 pk). W obu przypadkach te gwałtowne zmiany mogą wynikać z faktu, żę badano tam po 
# 45-44 osoby (w Podlaskim) i 59-33 osoby (w Warmińsko-Mazurskim), co jest dosyć małą próbą.


# ---- Pytanie 9. Wiek a internet (w domu) -----------------------------------------

wiek_2023 <- ess23 |>
  mutate(gr_wiekowa = case_when(
    agea >= 15 & agea <= 29 ~ "15-29",
    agea >= 30 & agea <= 44 ~ "30-44",
    agea >= 45 & agea <= 59 ~ "45-59",
    agea >= 60 ~ "60+",
    #is.na(agea) ~ "brak danych"
  )) 

ggplot(wiek_2023, aes(x = gr_wiekowa)) +
  geom_histogram()

# Odpowiedź:


# ---- Pytanie 10. Zaufanie do Sejmu od 2002 do 2023 roku (w domu) -----------------


# Odpowiedź:


# ---- Dla chętnych: wagi ----------------------------------------------------------


# Odpowiedź:
