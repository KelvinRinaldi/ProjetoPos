### Heat Maps ###

# Packages
install.packages('leaflet.extras')
install.packages('leaflet.minicharts')
install.packages('manipulateWidget')

library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(tidyverse)
library(leaflet.minicharts)
library(manipulateWidget)

#########################################################################################
# Fatal Accidents
teste2 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# returns from the dataframe only the desired fields
teste2 <- teste2 %>% select(latitude, longitude, municipio)

# removes all duplicate records
teste2 <- teste2[!duplicated(teste2), ]

teste2$latitude <- as.numeric(as.character(teste2$latitude))
teste2$longitude <- as.numeric(as.character(teste2$longitude))

# Heat Map
leaflet(data = teste2) %>% addTiles() %>%
  #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15)

#########################################################################################

# Fatal Accidents - By Season
teste3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

teste3Verao <- teste3 %>% filter(teste3$estacao == "Verão" )
teste3Outono <- teste3 %>% filter(teste3$estacao == "Outono" )
teste3Inverno <- teste3 %>% filter(teste3$estacao == "Inverno" )
teste3Primavera <- teste3 %>% filter(teste3$estacao == "Primavera" )

# returns from the dataframe only the desired fields
teste3Verao <- teste3Verao %>% select(latitude, longitude, municipio)
teste3Outono <- teste3Outono %>% select(latitude, longitude, municipio)
teste3Inverno <- teste3Inverno %>% select(latitude, longitude, municipio)
teste3Primavera <- teste3Primavera %>% select(latitude, longitude, municipio)

# Heat map

mapaVerao <- leaflet(data = teste3Verao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Summer', position = "topleft")

mapaOutono <- leaflet(data = teste3Outono, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Autumn')

mapaInverno <- leaflet(data = teste3Inverno, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Winter')

mapaPrimavera <- leaflet(data = teste3Primavera, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Spring')


combineWidgets(mapaVerao, mapaOutono, 
               mapaInverno, mapaPrimavera)


# Summer map
leaflet(data = teste3Verao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Verão', position = "topleft")

# autumn Map
leaflet(data = teste3Outono, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Outono')

# Winter map
leaflet(data = teste3Inverno, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Inverno')

# Spring map
leaflet(data = teste3Primavera, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Estação Primavera')

#########################################################################################

# Fatal Accidents - By Vehicle Group
teste3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

acidentesGVTransPassageiros <- teste3 %>% filter(teste3$grupo_veiculo == "Transp. de Passageiros" )
acidentesGVTransCarga <- teste3 %>% filter(teste3$grupo_veiculo == "Transp. de Carga" )
acidentesGVNI <- teste3 %>% filter(teste3$grupo_veiculo == "Não Informado" )
acidentesGVTracao <- teste3 %>% filter(teste3$grupo_veiculo == "Tração" )

# returns from the dataframe only the desired fields
acidentesGVTransPassageiros <- acidentesGVTransPassageiros %>% select(latitude, longitude, municipio)
acidentesGVTransCarga <- acidentesGVTransCarga %>% select(latitude, longitude, municipio)
acidentesGVNI <- acidentesGVNI %>% select(latitude, longitude, municipio)
acidentesGVTracao <- acidentesGVTracao %>% select(latitude, longitude, municipio)

mapaGVTP <- leaflet(data = acidentesGVTransPassageiros, options = leafletOptions(zoomControl = FALSE, minZoom = 3, maxZoom = 6)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Passengers', position = "topleft")

mapaGVTC <- leaflet(data = acidentesGVTransCarga, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Charge')

mapaGVNI <- leaflet(data = acidentesGVNI, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Uninformed')

mapaGVT <- leaflet(data = acidentesGVTracao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Traction')


combineWidgets(title = 'Mapa de Calor Por Grupo de Veículo',
               mapaGVTP, mapaGVTC, 
               mapaGVNI, mapaGVT)

# Passengers
leaflet(data = acidentesGVTransPassageiros, options = leafletOptions(zoomControl = FALSE, minZoom = 3, maxZoom = 6)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% 
  addControl('Passengers', position = "topleft")

# Cargo
leaflet(data = acidentesGVTransCarga, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Cargo transport') 

# Uninformed
leaflet(data = acidentesGVNI, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Uninformed')

# Traction
leaflet(data = acidentesGVTracao, options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude,
             blur = 20, max = 0.05, radius = 15) %>% addControl('Traction')
