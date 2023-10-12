###############################
## R-intro på ISF 12.10.2023 ##
###############################

# KALKULATOR -------------------------------------------------------------------
# R kan brukes som en kalkulator
2 + 2
2 * 2

# Informasjon kan lagres i objekter ved hjelp av <-
eple <- 10

2 + eple

# Vi fjerner objekter ved hjelp av rm(objektnavn) 
rm(eple)


# Opprett prosjekt og last ned data --------------------------------------------


# Pakker -----------------------------------------------------------------------
# For å innstallere pakken tidyverse (bare første gang du skal bruke den):
install.packages("tidyverse")

# For å laste inn pakken tidyverse (hver gang du åpner RStudio på nytt):
library(tidyverse)


# Laste inn data ---------------------------------------------------------------
# Vi skal bruke en .dta-fil med et fiktivt valg-datasett i dag. 

# For å laste inn .dta-filer må vi 1) installere pakken haven:
install.packages("haven")

# og 2) last pakker inn:
library(haven)

# Funksjonen for å laste inn .dta-filer heter read_dta. Vi lagrer data i et 
# objekt som vi kaller valg:
valg <- read_dta("valgdata.dta")

# Deskriptiv statistikk -------------------------------------------------------
# Først skal vi bli litt kjent med datasettet:
summary(valg)
# summary() gir informasjon om variablenes klasse samt deskriptiv statistikk for
# numeriske variabler

# Beskrivelse av det fiktive datasettet:
# kjonn: Mann, Kvinne
# utd: 0 = Grunnskole, 1 = Videregående, 2 = Universitet/høyskole
# stemte ved valget: 0 = Nei, 1 = Ja
# alder: antall år
# demo_mis: Hvor fornøyd med måten demokratiet virker på Norge? 0 = meget 
# fornøyd og 3 = ikke fornøyd i det hele tatt.

# Vi kan bruke tabeller til å se fordelinger av verdier:
table(valg$utd)  # $ henter ut variabelen utd i datasettet valg

# Ber R rapportere missingverdier i tabellen:
table(valg$utd, useNA = "always")

# Dersom vi ønsker prosentfordelingen så kan vi sette prop.table() rundt en table()
prop.table(table(valg$utd, useNA = "always"))

# Vi kan også lage krysstabeller
table(valg$stemte, valg$utd) 

prop.table(table(valg$stemte, valg$utd), 1) # 1 angir % av rad

prop.table(table(valg$stemte, valg$utd), 2) # 2 angir % av kolonne

# summary() kan også ta enkeltvariabler som argument, men da må vi bruke $ til å
# indikere hvilken variabel i datasettet vi er interessert i:

summary(valg$demo_mis)

# Det finnes også mange andre funksjoner, men her må vi huske å spesifisere 
# hvordan R skal håndtere missingverdiene
sd(valg$demo_mis)
sd(valg$demo_mis, na.rm = TRUE)

# OPPGAVE: 1) Finn standardavviket til alder 
# ved å bruke sd(). 2) Finn også gjennomsnittet 
# ved å bruke mean() som har samme oppbyging som sd(). 

## Grupperte data -------------------------------------------------------------
valg %>% 
  group_by(kjonn) %>%  # Grupperingsvariabel
  summarise(# nyttnavn = funksjon(variabel),
    snitt = mean(demo_mis, na.rm = TRUE),
    sd = sd(demo_mis, na.rm = TRUE))

# Merk at her lagrer vi ikke objektet i environment med <-, men det kunne vi
# gjort om vi ville bruke objektet videre. 

# Barbeide data ----------------------------------------------------------------

## Logiske operatorer ----------------------------------------------------------

# Disse brukes ofte i kombinasjon med select(), filter() og ved omkodinger:
# == er lik
# <  mindre enn
# >  større enn
# <= mindre eller lik
# >= større eller lik
# != ikke lik
# !x ikke x
# &  og
# |  eller


## Velge observasjoner og variabler ---------------------------------------------

# Bruker <- til å lage et nytt objekt som heter valg_menn. Objektet inneholder 
# 1) bare observasjoner av menn og 2) bare variablene demo_mis og stemte. 

valg_menn <- valg %>%  
  # filter() er for rader/observasjoner
  # Dersom variabelen du filtrere på er character så må du sette verdien i " "
  filter(kjonn == "Mann") %>% 
  # select() er for kolonner/variabler
  # du kan enten liste opp variabler du vil ha med som vi gjør her eller du kan
  # velge variabler du vil ta ut ved å skrive -var1, -var2. 
  select(demo_mis, stemte)

summary(valg_menn)


## Omkoding ---------------------------------------------------------------------
# Her skriver vi over det opprinnelige valgobjektet
valg <- valg %>% 
  mutate(# varnavn = funksjon() eller formel,
    utd_chr = case_when(utd == 0 ~ "Grunnskole",
                        utd == 1 ~ "VGS",
                        utd == 2 ~ "Universitet/hoyskole"),
    alder_sentr = alder - mean(alder, na.rm = TRUE),
    alder_chr = as.character(alder))

# OPPGAVE: 1) Lag et datasett som bare 
# inneholder observasjoner av kvinner. 
# Lagre objektet i environment. 
# 2) Lag en ny variabel som tar verdien
# 1 dersom personen har utdannelse på
# universitet/høyskole og 0 dersom de 
# har utdannelse fra grunnskole eller
# VGS. 

## Objektklasser ----------------------------------------------------------------
mean(valg$kjonn)
mean(valg$alder)

class(valg$kjonn)
class(valg$alder)

valg %>% 
  select(alder, alder_chr) %>%  # Velger ut variablene alder og alder_chr
  head(., 3) 

# Dette virker ikke:
mean(valg$alder_chr, na.rm = TRUE)

# Variabler kan "se" ut som tall, men ha klassen "character":
class(valg$alder_chr)


# Visualisering ----------------------------------------------------------------

## Steg 1: Angi datakilde
ggplot(data = valg) 

## Steg 2: Angi sammenheng
ggplot(data = valg,                     # Angir datakilde
       mapping = aes(x = utd_chr))      # Angir variabel

## Steg 3: Angi type visualisering
ggplot(data = valg,                     # Angir datakilde
       mapping = aes(x = utd_chr)) +    # Angir variabel
  geom_bar()                            # Angir type visualisering

## Steg 4: legg til flere geoms (om du vil)


## Steg 5: Juster skala, etiketter etc. 
ggplot(data = valg,                     # Angir datakilde
       mapping = aes(x = utd_chr)) +    # Angir variabel
  geom_bar() +                          # Angir type visualisering
  labs(x = element_blank(),             # Fjerner tittel på x-aksen
       y = "Antall observasjoner",      # Legger til tittel på y-aksen
       title = "Et enkelt plot") +      # Legger til tittel
  theme_bw()                            # Endrer design


## Plotter alder 

# Steg 1
ggplot(data = valg)

# Steg 2
ggplot(data = valg, 
       mapping = aes(x = alder))

# Steg 3
ggplot(data = valg, 
       mapping = aes(x = alder, col = demo_mis)) + 
  geom_boxplot()

ggplot(data = valg, 
       mapping = aes(x = alder, col = as.factor(demo_mis))) + 
  geom_boxplot()


## Plotter alder og demo_mis

ggplot(data = valg)

ggplot(data = valg, 
       mapping = aes(x = alder, y = demo_mis)) + 
  geom_point() + 
  geom_smooth()

# Eksempel på regresjon --------------------------------------------------------

## Kjører modellene og lagrer i objekter
reg1 <- lm(demo_mis ~ utd, 
           data = valg)

reg2 <- lm(demo_mis ~ utd + kjonn, 
           data = valg)

summary(reg2)

## Bruker stargazer til å lage en oversiktlig tabell
install.packages("stargazer")
library(stargazer)
stargazer(reg1, reg2, type = "text")

## Bruker sjPlot til å visalisere predikerte verdier
# sjPlot bygger på ggplot og man kan kombinere med ggplot-kode

install.packages("sjPlot")
library(sjPlot)

plot_model(reg2,                           # modellobjekt
           terms = "utd",                  # variabel du vil visualisere
           type = "pred") +                # type plot
  labs(title = "Predikerte verdier",       # tittel på visualisering
       x = "Utdanningsnivå",               # tittel på x-aksen
       y = "Misnøye med demokrati i NO") + # tittel på y-akse
  theme_bw()                               # endrer design


# Undersøker bivariate sammenhenger --------------------------------------------
install.packages("corrr")
library(corrr)
correlate(valg)

cor.test(valg$demo_mis, valg$stemte, use = "pairwise")
