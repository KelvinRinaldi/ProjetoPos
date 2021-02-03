### Graphics ###

# Libraries
library(tidyverse)
library(RColorBrewer)

# Identifies the day of the week with the highest number of occurrences
volumePorDia <- table(dados_acidentes_rs$day_week)
volumePorDia

# Creates color list from gray to blue
listaDeCores1 <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identify how many colors we will need
listaDeCores1 <- listaDeCores1(n = nrow(x = volumePorDia))

listaDeCores1

# Binds colors to the value of each associated variable
cores_VolumePorDia <-
  as.character(
    x = cut(
      x = rank( x = volumePorDia )  
      , breaks = nrow( x = volumePorDia )
      , labels = listaDeCores1
    )
  )


# Bar graph to show the number of accidents per day of the week
volumePorDiaX <- barplot(volumePorDia, main="Accidents By Day Of The Week", 
                         xlab="Days Of The Week",
                         names.arg=c("Sunday","Monday","Tuesday","Wednesday",
                                     "Thursday", "Friday", "Saturday"),
                         ylab="Volume",
                         ylim=c(0,7500),
                         col=cores_VolumePorDia)

volumePorDiaY <-as.matrix(volumePorDia)

text(volumePorDiaX,volumePorDiaY,labels=as.character(volumePorDiaY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Searching for number of occurrences by gender
sexoEnvolvidos <- table(dados_acidentes_rs$gender)
sexoEnvolvidos
perSexoEnvolvidos<- round(100*sexoEnvolvidos/sum(sexoEnvolvidos), 1)
perSexoEnvolvidos=paste(perSexoEnvolvidos, "%")

#Gera gráfico de pizza
pie(sexoEnvolvidos, labels = perSexoEnvolvidos, , main = "Accident Volume by Gender",
    col=c("Gray","Blue"))
legend(1, c("Female", "Male"), 
       cex = 1.0,
       fill = c("Gray","Blue"))

#####################################################################################

# Chart by Age Group - General
gruposDeIdade <- cut(dados_acidentes_rs$age, breaks=c(0,20,30,40,50,60,Inf))

# Grouping the data
table(gruposDeIdade)

# Creates color list from gray to blue
listaDeCoresGrupoIdade <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identify how many colors we will need
listaDeCoresGrupoIdade <- listaDeCoresGrupoIdade(n = nrow(x = table(gruposDeIdade)))

listaDeCoresGrupoIdade

# Binds colors to the value of each associated variable
cores_GrupoIdade <-
  as.character(
    x = cut(
      x = rank( x = table(gruposDeIdade) )  # used to assign order in the event of ties
      , breaks = nrow( x = table(gruposDeIdade) )  # same as the 'n' supplied in color.function()
      , labels = listaDeCoresGrupoIdade  # label the groups with the color in color.ramp
    )
  )

volumeFaixaEtariaX <- barplot(table(gruposDeIdade), 
                              main="Accidents By Age Group - General",
                              ylim=c(0,10000),
                              ylab="Accident volume",
                              col=cores_GrupoIdade,
                              names.arg=c("0 to 20","20 to 30","30 to 40","40 to 50","50 to 60","Older than 60"))

volumeFaixaEtariaY <-as.matrix(table(gruposDeIdade))

text(volumeFaixaEtariaX,volumeFaixaEtariaY,labels=as.character(volumeFaixaEtariaY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Chart by Age Range - Drivers Only

# Filters only drivers  
dfCondutores <- dados_acidentes_rs %>% filter(dados_acidentes_rs$tipo_envolvido == "Condutor" )

gruposDeIdadeCondutores <- cut(dfCondutores$age, breaks=c(0,20,30,40,50,60,Inf))

table(gruposDeIdadeCondutores)

# Creates color list from gray to blue
listaDeCoresGrupoIdadeCondutores <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identify how many colors we will need
listaDeCoresGrupoIdadeCondutores <- listaDeCoresGrupoIdadeCondutores(n = nrow(x = table(gruposDeIdadeCondutores)))

listaDeCoresGrupoIdadeCondutores

# Binds colors to the value of each associated variable
cores_GrupoIdadeCondutores <-
  as.character(
    x = cut(
      x = rank( x = table(gruposDeIdadeCondutores) )  # used to assign order in the event of ties
      , breaks = nrow( x = table(gruposDeIdadeCondutores) )  # same as the 'n' supplied in color.function()
      , labels = listaDeCoresGrupoIdadeCondutores  # label the groups with the color in color.ramp
    )
  )

volumeFaixaEtariaCondutorX <- barplot(table(gruposDeIdadeCondutores), 
                              main="Accidents By Age Group - Drivers Only",
                              ylim=c(0,8000),
                              ylab="Accident volume",
                              col=cores_GrupoIdadeCondutores,
                              names.arg=c("0 to 20","20 to 30","30 to 40","40 to 50","50 to 60","Older than 60"))

volumeFaixaEtariaCondutorY <-as.matrix(table(gruposDeIdadeCondutores))

text(volumeFaixaEtariaCondutorX,volumeFaixaEtariaCondutorY,labels=as.character(volumeFaixaEtariaCondutorY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Pie Chart with meteorological information of accidents with deaths

# Filters only cases of death
df1 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforms it into a table so that we have the frequency
df1 <-table(df1$condicao_metereologica)

# Transforms data frame again
df1 <- as.data.frame(df1)

df1 <- transform(df1, 
                    Var1 = factor(Var1, 
                                 levels = c("Céu Claro", "Chuva", "Garoa/Chuvisco", "Granizo", "Ignorado", "Neve", "Nevoeiro/Neblina", 
                                            "Nublado", "Sol", "Vento"),
                                 labels = c("Clear sky", "Rain", "Drizzle", "Hail", "Ignored", "Snow", "Fog", "Cloudy", "Sunny", "Wind")))


names(df1)[1] <- "MeteorologicalCond"

# Calculates the percentage
df1$percentual = df1$Freq / sum(df1$Freq)

# Auxiliary Variables
df1$ymax = cumsum(df1$percentual)
df1$ymin = c(0, head(df1$ymax, n=-1))

ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, 
                fill=MeteorologicalCond)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  ggtitle("Volume of Fatal Accidents by Meteorological Condition")

# Generates pie chart
coresGraficoCondMetereologica <- brewer.pal(8, "Set2") 

# Filters only cases of death
df1 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforms it into a table so that we have the frequency
df1 <- table(df1$condicao_metereologica)
df1
perdf1<- round(100*df1/sum(df1), 1)

perdf1

# Generates pie chart
pie(df1, labels = perdf1, , main = "Volume de Acidentes Fatais Por Condição Meteorológica")

pie(df1 , labels = perdf1, border="white", 
    col=coresGraficoCondMetereologica,
    main = "Volume de Acidentes Fatais Por Condição Meteorológica")

#####################################################################################

# Point Graph that Returns the Volume of Fatal Accidents due to Accident

# Filters only cases of death
df2 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforms it into a table so that we have the frequency
df2 <-table(df2$causa_acidente)

# Transforms data frame again
df2 <- as.data.frame(df2)

df2 <- transform(df2, 
                 Var1 = factor(Var1, 
                               levels = c("Agressão Externa", "Carga excessiva e/ou mal acondicionada", "Deficiência ou não Acionamento do Sistema de Iluminação/Sinalização do Veículo", "Ingestão de Substâncias Psicoativas", "Ingestão de álcool e/ou substâncias psicoativas pelo pedestre", "Sinalização da via insuficiente ou inadequada", "Objeto estático sobre o leito carroçável", "Avarias e/ou desgaste excessivo no pneu", "Defeito na Via", "Fenômenos da Natureza", "Restrição de Visibilidade", "Desobediência às normas de trânsito pelo pedestre", "Animais na Pista", "Defeito Mecânico no Veículo", "Não guardar distância de segurança", "Pista Escorregadia", "Mal Súbito", "Ultrapassagem Indevida", "Ingestão de Álcool", "Condutor Dormindo", "Falta de Atenção do Pedestre", "Desobediência às normas de trânsito pelo condutor", "Velocidade Incompatível", "Falta de Atenção à Condução"),
                               labels = c("External Aggression", "Excessive and/or poorly packed load", "Deficiency or not Activation of the Vehicle Lighting/Signaling System", "Ingestion of Psychoactive Substances", "Ingestion of alcohol and/or psychoactive substances by the pedestrian", "Insufficient or inadequate track signaling", "Static object on the bed base", "Damage and/or excessive tire wear", "Track defect", "Nature Phenomena", "Visibility Restriction", "Disobedience to the rules of pedestrian traffic","Animals on the Track","Mechanical Fault in the Vehicle","Do not keep safety distance","Slippery Track", "Sudden Illness", "Undue Overtaking", "Alcohol Intake", "Sleeping Driver" , "Pedestrian Lack of Attention", "Driver's Disobedience to Traffic Rules", "Incompatible Speed", "Lack of Attention to Driving")))

# Sort the data frame
df2 <- df2[order(df2$Freq),]

dotchart(df2$Freq,labels=df2$Var1,cex=.9,
         xlab="No. of Deaths",
         pch = 19,
         col = c("darkblue","dodgerblue"),
         cex.main = 2.0, cex.lab = 2.0, xlim = c(0,300))

#####################################################################################

# Horizontal Bar Graph - Fatal Accidents by Type of Accident

df3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforms it into a table so that we have the frequency
df3 <-table(df3$tipo_acidente)

# Transforms data frame again
df3 <- as.data.frame(df3)

df3 <- subset.data.frame(df3, Freq != 0)

df3 <- transform(df3, 
                 Var1 = factor(Var1, 
                               levels = c("Atropelamento de Animal", "Atropelamento de Pedestre", "Capotamento", "Colisão com objeto em movimento", "Colisão com objeto estático", "Colisão frontal", "Colisão lateral", "Colisão transversal", "Colisão traseira", "Danos eventuais", "Engavetamento", "Queda de ocupante de veículo", "Saída de leito carroçável", "Tombamento"),
                               labels = c("Animal trampling", "Pedestrian trampling", "Rollover", "Collision with moving object", "Collision with static object", "Frontal collision", "Side collision", "Cross collision", "Rear collision" , "Possible damage", "Locking up", "Falling vehicle occupant", "Exit from bed bed", "Tipping")))


df3 <- df3[order(df3$Freq),]


df3 %>%
  mutate(name=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_segment( aes(xend=Var1, yend=0)) +
  geom_point( size=3, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Fatal Accidents by Type of Accident") +
  ylim(0,400)

#####################################################################################

# Graph by Vehicle Group
volumeGrupoVeiculo <- table(dados_acidentes_rs$grupo_veiculo)
volumeGrupoVeiculo

volumeGrupoVeiculoX <- barplot(volumeGrupoVeiculo,
        main="Acidentes Por Grupo de Veículo", 
        xlab="Grupos",
        ylab="Volume",
        ylim=c(0,35000),
        col = "#104E8B")

volumeGrupoVeiculoY <-as.matrix(volumeGrupoVeiculo)

text(volumeGrupoVeiculoX,volumeGrupoVeiculoY,labels=as.character(volumeGrupoVeiculoY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Assessing accidents by season

# Summer

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoVerao <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Verão" )

volGrupoVeiculoFatalPorEstacaoVerao <- table(fatalidadePorEstacaoVerao$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoVerao

volBRFatalPorEstacaoVerao <- table(fatalidadePorEstacaoVerao$br)
volBRFatalPorEstacaoVerao <- as.data.frame(volBRFatalPorEstacaoVerao)

volBRFatalPorEstacaoVerao$Percentual <- round(volBRFatalPorEstacaoVerao$Freq/sum(volBRFatalPorEstacaoVerao$Freq)*100,2)

# Autumn

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoOutono <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Outono" )

volGrupoVeiculoFatalPorEstacaoOutono <- table(fatalidadePorEstacaoOutono$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoOutono

volBRFatalPorEstacaoOutono <- table(fatalidadePorEstacaoOutono$br)
volBRFatalPorEstacaoOutono <- as.data.frame(volBRFatalPorEstacaoOutono)

volBRFatalPorEstacaoOutono$Percentual <- round(volBRFatalPorEstacaoOutono$Freq/sum(volBRFatalPorEstacaoOutono$Freq)*100,2)

# Winter

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoInverno <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Inverno" )

volGrupoVeiculoFatalPorEstacaoInverno <- table(fatalidadePorEstacaoInverno$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoInverno

volBRFatalPorEstacaoInverno <- table(fatalidadePorEstacaoInverno$br)
volBRFatalPorEstacaoInverno <- as.data.frame(volBRFatalPorEstacaoInverno)

volBRFatalPorEstacaoInverno$Percentual <- round(volBRFatalPorEstacaoInverno$Freq/sum(volBRFatalPorEstacaoInverno$Freq)*100,2)

# Spring

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoPrimavera <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Primavera" )

volGrupoVeiculoFatalPorEstacaoPrimavera <- table(fatalidadePorEstacaoPrimavera$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoPrimavera

volBRFatalPorEstacaoPrimavera <- table(fatalidadePorEstacaoPrimavera$br)
volBRFatalPorEstacaoPrimavera <- as.data.frame(volBRFatalPorEstacaoPrimavera)

volBRFatalPorEstacaoPrimavera$Percentual <- round(volBRFatalPorEstacaoPrimavera$Freq/sum(volBRFatalPorEstacaoPrimavera$Freq)*100,2)
