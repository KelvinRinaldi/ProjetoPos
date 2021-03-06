### Script para carregar e formatar os dados utilizados no projeto ###

# Bibliotecas
library(tidyverse)

# Carrega CSV de 2017
acidentes2017 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2017_todas_causas_tipos.csv',
                          sep = ";") 

# Carrega CSV de 2018
acidentes2018 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2018_todas_causas_tipos.csv',
                          sep = ";")

# Carrega CSV de 2019
acidentes2019 <- read.csv(file = 'C:/unisinos/dados_projaplicado/acidentes2019_todas_causas_tipos.csv',
                          sep = ";")

# Concatenando os anos
dados_acidentes_rs <- rbind(acidentes2017, acidentes2018, acidentes2019)

# Filtrando apenas dados do RS
dados_acidentes_rs <- subset.data.frame(dados_acidentes_rs, uf=="RS")

# Transforma os dias da semana em numeros
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                dia_semana = factor(dia_semana, 
                                                    levels = c("domingo", 
                                                               "segunda-feira", 
                                                               "ter�a-feira", 
                                                               "quarta-feira", 
                                                               "quinta-feira", 
                                                               "sexta-feira", 
                                                               "s�bado"),
                                                    labels = c(1, 2, 3, 4, 
                                                               5, 6, 7)))

# Filtrando apenas Masculino e Feminino
dados_acidentes_rs <- filter(dados_acidentes_rs, (sexo=="Masculino") | (sexo=="Feminino"))

dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                sexo = factor(sexo, 
                                              levels = c("Feminino", 
                                                         "Masculino"),
                                              labels = c(0, 1)))


dados_acidentes_rs <- filter(dados_acidentes_rs, (estado_fisico=="Ileso") | (estado_fisico=="Les�es Graves") |
                               (estado_fisico=="Les�es Leves") | (estado_fisico== "�bito"))

# Ajusta os campos de latitude e longitude
dados_acidentes_rs$latitude <- gsub(",", ".", dados_acidentes_rs$latitude)
dados_acidentes_rs$longitude <- gsub(",", ".", dados_acidentes_rs$longitude)

# Cria faixa et�ria por intervalos de idade
dados_acidentes_rs$faixaEtaria <- cut(dados_acidentes_rs$idade, breaks=c(0,19,64,Inf))

# Converte faixas et�rias para sequencial
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                faixaEtaria = factor(faixaEtaria, 
                                                     levels = c("(0,19]", 
                                                                "(19,64]", 
                                                                "(64,Inf]"),
                                                     labels = c(1, 2, 3)))

# Cria vari�vel Grupo Ve�culo
dados_acidentes_rs <- mutate(dados_acidentes_rs, grupo_veiculo = ifelse(tipo_veiculo %in% c("Autom�vel", "Camioneta", "Utilit�rio", "�nibus", "Micro-�nibus", "Motoneta", "Motocicleta", "Triciclo", "Ciclomotor"), "Transp. de Passageiros",
                                                  ifelse(tipo_veiculo %in% c("Caminh�o", "Caminhonete", "Reboque", "Semireboque"), "Transp. de Carga",
                                                         ifelse(tipo_veiculo %in% c("Trator de rodas", "Caminh�o-trator"), "Tra��o", "N�o Informado"))))

# Filtra apenas ve�culos fabricados a partir de 1960
dados_acidentes_rs <- filter(dados_acidentes_rs, (ano_fabricacao_veiculo >= 1960))

# Altera o estado f�sico para numeral
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                estado_fisico_seq = factor(estado_fisico, 
                                                         levels = c("Ileso", 
                                                                    "Les�es Leves", 
                                                                    "Les�es Graves", 
                                                                    "�bito"),
                                                         labels = c(1, 2, 3, 4)))

# Altera o usoSolo
dados_acidentes_rs <- transform(dados_acidentes_rs, 
                                uso_solo = factor(uso_solo, 
                                                  levels = c("N�o", 
                                                             "Sim"),
                                                  labels = c(0, 1)))

# Transformando a coluna de Mortos em Fator
dados_acidentes_rs$mortos <- as.factor(dados_acidentes_rs$mortos)


# Transforma os dados de latitude e longitude em numerais
dados_acidentes_rs$latitude <- as.numeric(as.character(dados_acidentes_rs$latitude))
dados_acidentes_rs$longitude <- as.numeric(as.character(dados_acidentes_rs$longitude))

# Criando variavel mes
dados_acidentes_rs$mes_ocorrencia <- format(as.Date(dados_acidentes_rs$data_inversa), "%m")

# Criando variavel ano-mes
dados_acidentes_rs$ano_mes_ocorrencia <- format(as.Date(dados_acidentes_rs$data_inversa), "%Y-%m")

# Criando variavel estacao
dados_acidentes_rs <- mutate(dados_acidentes_rs, 
                             estacao = ifelse(mes_ocorrencia %in% c('03', '04', '05'), "Outono",
                                                                        ifelse(mes_ocorrencia %in% c('06', '07', '08'), "Inverno",
                                                                               ifelse(mes_ocorrencia %in% c('09', '10', '11'), "Primavera", 
                                                                                      "Ver�o"))))

# Removendo diferentes ocorrencias do mesmo acidente
dados_acidentes_rs <- subset(dados_acidentes_rs, ordem_tipo_acidente == 1)
dados_acidentes_rs <- subset(dados_acidentes_rs, causa_principal == "Sim")



