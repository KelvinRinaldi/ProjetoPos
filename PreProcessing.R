### Script to load and format the data used in the project ###

# Libraries
library(tidyverse)

# Load CSV from 2017
acidentes2017 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2017_todas_causas_tipos.csv',
                          sep = ";") 

# Load CSV from 2018
acidentes2018 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2018_todas_causas_tipos.csv',
                          sep = ";")

# Load CSV from 2019
acidentes2019 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2019_todas_causas_tipos.csv',
                          sep = ";")

# Merge the years
dados_acidentes_rs <- rbind(acidentes2017, acidentes2018, acidentes2019)

# Translate the data to english
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "dia_semana"] <- "day_week"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "causa_acidente"] <- "origin_accident"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "tipo_acidente"] <- "type_accident"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "tipo_envolvido"] <- "type_involved"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "uso_solo"] <- "driver_only"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "fase_dia"] <- "day_phase"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "fase_dia"] <- "day_period"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "idade"] <- "age"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "tracado_via"] <- "track_layout"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "tipo_pista"] <- "track_type"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "sexo"] <- "gender"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "tipo_veiculo"] <- "type_vehicle"
names(dados_acidentes_rs)[names(dados_acidentes_rs) == "ano_fabricacao_veiculo"] <- "vehicle_manufacture_year"

# Filtering just the data of RS
dados_acidentes_rs <- subset.data.frame(dados_acidentes_rs, uf=="RS")

# Turns days of the week into numbers
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                day_week = factor(day_week, 
                                                    levels = c("sunday", 
                                                               "monday", 
                                                               "tuesday", 
                                                               "wednesday", 
                                                               "thursday", 
                                                               "friday", 
                                                               "saturday"),
                                                    labels = c(1, 2, 3, 4, 
                                                               5, 6, 7)))

# Filtering only Male and Female
dados_acidentes_rs <- filter(dados_acidentes_rs, (gender=="Masculino") | (gender=="Feminino"))

dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                gender = factor(gender, 
                                              levels = c("Feminino", 
                                                         "Masculino"),
                                              labels = c(0, 1)))


dados_acidentes_rs <- filter(dados_acidentes_rs, (estado_fisico=="Ileso") | (estado_fisico=="Lesões Graves") |
                               (estado_fisico=="Lesões Leves") | (estado_fisico== "Óbito"))

# Adjust the latitude and longitude fields
dados_acidentes_rs$latitude <- gsub(",", ".", dados_acidentes_rs$latitude)
dados_acidentes_rs$longitude <- gsub(",", ".", dados_acidentes_rs$longitude)

# Creates age range by age ranges
dados_acidentes_rs$AgeRange <- cut(dados_acidentes_rs$age, breaks=c(0,19,64,Inf))

# Converts age ranges to sequential
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                AgeRange = factor(AgeRange, 
                                                     levels = c("(0,19]", 
                                                                "(19,64]", 
                                                                "(64,Inf]"),
                                                     labels = c(1, 2, 3)))

# Creates variable Vehicle Group
dados_acidentes_rs <- mutate(dados_acidentes_rs, group_vehicle = ifelse(type_vehicle %in% c("Automóvel", "Camioneta", "Utilitário", "Ônibus", "Micro-Ônibus", "Micro-ônibus", "Motoneta", "Motocicleta", "Triciclo", "Ciclomotor"), "Transp. de Passageiros",
                                                  ifelse(type_vehicle %in% c("Caminhão", "Caminhonete", "Reboque", "Semireboque"), "Transp. de Carga",
                                                         ifelse(type_vehicle %in% c("Trator de rodas", "Caminhão-trator"), "Tração", "Não Informado"))))

# Filters only vehicles manufactured after 1960
dados_acidentes_rs <- filter(dados_acidentes_rs, (vehicle_manufacture_year >= 1960))

# Changes physical state to numeral
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                health_condition_seq = factor(estado_fisico, 
                                                         levels = c("Ileso", 
                                                                    "Lesões Leves", 
                                                                    "Lesões Graves", 
                                                                    "Óbito"),
                                                         labels = c(1, 2, 3, 4)))

# Change drivers only
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                driver_only = factor(driver_only, 
                                                  levels = c("Não", 
                                                             "Sim"),
                                                  labels = c(0, 1)))

# Turning the Dead column into a Factor
dados_acidentes_rs$mortos <- as.factor(dados_acidentes_rs$mortos)


# Turns latitude and longitude data into numerals
dados_acidentes_rs$latitude <- as.numeric(as.character(dados_acidentes_rs$latitude))
dados_acidentes_rs$longitude <- as.numeric(as.character(dados_acidentes_rs$longitude))

# Create month variable
dados_acidentes_rs$month_occurrence <- format(as.Date(dados_acidentes_rs$data_inversa), "%m")

# Create year-month variable
dados_acidentes_rs$year_month_occurrence <- format(as.Date(dados_acidentes_rs$data_inversa), "%Y-%m")

# Create season variable
dados_acidentes_rs <- mutate(dados_acidentes_rs, 
                             estacao = ifelse(month_occurrence %in% c('03', '04', '05'), "Outono",
                                                                        ifelse(month_occurrence %in% c('06', '07', '08'), "Inverno",
                                                                               ifelse(month_occurrence %in% c('09', '10', '11'), "Primavera", 
                                                                                      "Verão"))))

# Removing different occurrences of the same accident
dados_acidentes_rs <- subset(dados_acidentes_rs, ordem_tipo_acidente == 1)
dados_acidentes_rs <- subset(dados_acidentes_rs, causa_principal == "Sim")



