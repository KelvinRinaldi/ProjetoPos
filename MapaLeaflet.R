### Criação de Mapas de Calor ###

# Instalação de Pacotes
install.packages('leaflet.extras')
install.packages('leaflet.minicharts')
install.packages('manipulateWidget')

# Bibliotecas
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(tidyverse)
library(leaflet.minicharts)
library(manipulateWidget)

#########################################################################################
# Acidentes Fatais
teste2 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# retorna do dataframe apenas os campos desejados
teste2 <- teste2 %>% select(latitude, longitude, municipio)

# remove todos os registros duplicados
teste2 <- teste2[!duplicated(teste2), ]

teste2$latitude <- as.numeric(as.character(teste2$latitude))
teste2$longitude <- as.numeric(as.character(teste2$longitude))

# MAPA DE CALOR
leaflet(data = teste2) %>% addTiles() %>%
  #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15)

#########################################################################################

# Acidentes Fatais - Por Estacao 
teste3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

teste3Verao <- teste3 %>% filter(teste3$estacao == "Verão" )
teste3Outono <- teste3 %>% filter(teste3$estacao == "Outono" )
teste3Inverno <- teste3 %>% filter(teste3$estacao == "Inverno" )
teste3Primavera <- teste3 %>% filter(teste3$estacao == "Primavera" )

# retorna do dataframe apenas os campos desejados
teste3Verao <- teste3Verao %>% select(latitude, longitude, municipio)
teste3Outono <- teste3Outono %>% select(latitude, longitude, municipio)
teste3Inverno <- teste3Inverno %>% select(latitude, longitude, municipio)
teste3Primavera <- teste3Primavera %>% select(latitude, longitude, municipio)

# MAPA DE CALOR

mapaVerao <- leaflet(data = teste3Verao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Verão', position = "topleft")

mapaOutono <- leaflet(data = teste3Outono, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Outono')

mapaInverno <- leaflet(data = teste3Inverno, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Inverno')

mapaPrimavera <- leaflet(data = teste3Primavera, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Primavera')


combineWidgets(title = 'Mapa de Calor Por Estação',
               mapaVerao, mapaOutono, 
               mapaInverno, mapaPrimavera)


#mapaVerao
leaflet(data = teste3Verao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Verão', position = "topleft")

#mapaOutono
leaflet(data = teste3Outono, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Outono')

#mapaInverno
leaflet(data = teste3Inverno, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Inverno')

#mapaPrimavera
leaflet(data = teste3Primavera, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Primavera')

#########################################################################################
# Acidentes Fatais - Por Grupo de Veiculo 
teste3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )


acidentesGVTransPassageiros <- teste3 %>% filter(teste3$grupo_veiculo == "Transp. de Passageiros" )
acidentesGVTransCarga <- teste3 %>% filter(teste3$grupo_veiculo == "Transp. de Carga" )
acidentesGVNI <- teste3 %>% filter(teste3$grupo_veiculo == "Não Informado" )
acidentesGVTracao <- teste3 %>% filter(teste3$grupo_veiculo == "Tração" )

# retorna do dataframe apenas os campos desejados
acidentesGVTransPassageiros <- acidentesGVTransPassageiros %>% select(latitude, longitude, municipio)
acidentesGVTransCarga <- acidentesGVTransCarga %>% select(latitude, longitude, municipio)
acidentesGVNI <- acidentesGVNI %>% select(latitude, longitude, municipio)
acidentesGVTracao <- acidentesGVTracao %>% select(latitude, longitude, municipio)

mapaGVTP <- leaflet(data = acidentesGVTransPassageiros, options = leafletOptions(zoomControl = FALSE, minZoom = 3, maxZoom = 6)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Passageiros', position = "topleft")

mapaGVTC <- leaflet(data = acidentesGVTransCarga, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Carga')

mapaGVNI <- leaflet(data = acidentesGVNI, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Nao Informado')

mapaGVT <- leaflet(data = acidentesGVTracao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Tracao')


combineWidgets(title = 'Mapa de Calor Por Grupo de Veículo',
               mapaGVTP, mapaGVTC, 
               mapaGVNI, mapaGVT)

#Passageiros
leaflet(data = acidentesGVTransPassageiros, options = leafletOptions(zoomControl = FALSE, minZoom = 3, maxZoom = 6)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Passageiros', position = "topleft")

#Carga
leaflet(data = acidentesGVTransCarga, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Carga') 

#Não Informado
leaflet(data = acidentesGVNI, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Nao Informado')

#Tracao
leaflet(data = acidentesGVTracao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Tracao')
