### Gráficos do Projeto Aplicado ###

# Bibliotecas
library(tidyverse)
library(RColorBrewer)

# Identifica o dia da semana com maior número de ocorrências
volumePorDia <- table(dados_acidentes_rs$dia_semana)
volumePorDia

# Cria lista de cores do cinza até o azul
listaDeCores1 <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identifica quantas cores iremos precisar 
listaDeCores1 <- listaDeCores1(n = nrow(x = volumePorDia))

# Apresenta as cores
listaDeCores1

# Vincula as cores ao valor de cada váriavel associada
cores_VolumePorDia <-
  as.character(
    x = cut(
      x = rank( x = volumePorDia )  
      , breaks = nrow( x = volumePorDia )
      , labels = listaDeCores1
    )
  )


# Gráfico de barras para apresentar o n° de acidentes por dia da semana
volumePorDiaX <- barplot(volumePorDia, main="Acidentes Por Dia da Semana", 
                         xlab="Dias da Semana",
                         names.arg=c("Domingo","Segunda","Terça","Quarta",
                                     "Quinta", "Sexta", "Sábado"),
                         ylab="Volume",
                         ylim=c(0,7500),
                         col=cores_VolumePorDia)

volumePorDiaY <-as.matrix(volumePorDia)

text(volumePorDiaX,volumePorDiaY,labels=as.character(volumePorDiaY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Buscando quantidade de ocorrencias por sexo
sexoEnvolvidos <- table(dados_acidentes_rs$sexo)
sexoEnvolvidos
perSexoEnvolvidos<- round(100*sexoEnvolvidos/sum(sexoEnvolvidos), 1)
perSexoEnvolvidos=paste(perSexoEnvolvidos, "%")

#Gera gráfico de pizza
pie(sexoEnvolvidos, labels = perSexoEnvolvidos, , main = "Volume de Acidentes Por Sexo",
    col=c("Gray","Blue"))
legend("topright", c("Feminino", "Masculino"), 
       cex = 0.8,
       fill = c("Gray","Blue"))

#####################################################################################

# Gráfico por Faixa Etária - Geral
gruposDeIdade <- cut(dados_acidentes_rs$idade, breaks=c(0,20,30,40,50,60,Inf))
#Agrupando os dados
table(gruposDeIdade)

# Cria lista de cores do cinza até o azul
listaDeCoresGrupoIdade <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identifica quantas cores iremos precisar 
listaDeCoresGrupoIdade <- listaDeCoresGrupoIdade(n = nrow(x = table(gruposDeIdade)))

# Apresenta as cores
listaDeCoresGrupoIdade

# Vincula as cores ao valor de cada váriavel associada
cores_GrupoIdade <-
  as.character(
    x = cut(
      x = rank( x = table(gruposDeIdade) )  # used to assign order in the event of ties
      , breaks = nrow( x = table(gruposDeIdade) )  # same as the 'n' supplied in color.function()
      , labels = listaDeCoresGrupoIdade  # label the groups with the color in color.ramp
    )
  )

#Gerando gráfico do n° de acidentes por faixa etária
#barplot(table(gruposDeIdade),
#        main = "Acidentes Por Grupo de Idade",
#        names.arg=c("0-20","20-30","30-40", 
#                    "40-50", "50-60", "60 >"),
#        xlim=c(0,10000),
#        las=1, 
#        cex.names=0.8,
#        horiz = T,
#        col=cores_GrupoIdade
#)

volumeFaixaEtariaX <- barplot(table(gruposDeIdade), 
                              main="Acidentes Por Grupo de Idade - Geral",
                              ylim=c(0,10000),
                              ylab="Volume de Acidentes",
                              col=cores_GrupoIdade)

volumeFaixaEtariaY <-as.matrix(table(gruposDeIdade))

text(volumeFaixaEtariaX,volumeFaixaEtariaY,labels=as.character(volumeFaixaEtariaY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Gráfico por Faixa Etária - Só Condutores

# Filtra apenas os condutores  
dfCondutores <- dados_acidentes_rs %>% filter(dados_acidentes_rs$tipo_envolvido == "Condutor" )

gruposDeIdadeCondutores <- cut(dfCondutores$idade, breaks=c(0,20,30,40,50,60,Inf))
#Agrupando os dados
table(gruposDeIdadeCondutores)

# Cria lista de cores do cinza até o azul
listaDeCoresGrupoIdadeCondutores <- colorRampPalette(c("#CCCCCC", "#104E8B"))

# Identifica quantas cores iremos precisar 
listaDeCoresGrupoIdadeCondutores <- listaDeCoresGrupoIdadeCondutores(n = nrow(x = table(gruposDeIdadeCondutores)))

# Apresenta as cores
listaDeCoresGrupoIdadeCondutores

# Vincula as cores ao valor de cada váriavel associada
cores_GrupoIdadeCondutores <-
  as.character(
    x = cut(
      x = rank( x = table(gruposDeIdadeCondutores) )  # used to assign order in the event of ties
      , breaks = nrow( x = table(gruposDeIdadeCondutores) )  # same as the 'n' supplied in color.function()
      , labels = listaDeCoresGrupoIdadeCondutores  # label the groups with the color in color.ramp
    )
  )

volumeFaixaEtariaCondutorX <- barplot(table(gruposDeIdadeCondutores), 
                              main="Acidentes Por Grupo de Idade - Apenas Condutores",
                              ylim=c(0,10000),
                              ylab="Volume de Acidentes",
                              col=cores_GrupoIdadeCondutores)

volumeFaixaEtariaCondutorY <-as.matrix(table(gruposDeIdadeCondutores))

text(volumeFaixaEtariaCondutorX,volumeFaixaEtariaCondutorY,labels=as.character(volumeFaixaEtariaCondutorY), 
     pos = 3, cex = 0.8, col = "black")

#####################################################################################

# Gráfico Cirular com informações meterológicas dos acidentes com óbitos

# Filtra apenas os casos de óbito 
df1 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforma em tabela para que tenhamos a frequência
df1 <-table(dados_acidentes_rs$condicao_metereologica)

# Transforma em data frame novamente
df1 <- as.data.frame(df1)

names(df1)[1] <- "condMetereologica"

# Calcula o percentual
df1$percentual = df1$Freq / sum(df1$Freq)

# Variaveis Auxiliares
df1$ymax = cumsum(df1$percentual)
df1$ymin = c(0, head(df1$ymax, n=-1))

ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, 
                fill=condMetereologica)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  ggtitle("Volume de Acidentes Fatais Por Condição Meteorológica")

#Gera gráfico de pizza
coresGraficoCondMetereologica <- brewer.pal(8, "Set2") 

# Filtra apenas os casos de óbito 
df1 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforma em tabela para que tenhamos a frequência
df1 <- table(df1$condicao_metereologica)
df1
perdf1<- round(100*df1/sum(df1), 1)

perdf1

#Gera gráfico de pizza
pie(df1, labels = perdf1, , main = "Volume de Acidentes Fatais Por Condição Meteorológica")

pie(df1 , labels = perdf1, border="white", 
    col=coresGraficoCondMetereologica,
    main = "Volume de Acidentes Fatais Por Condição Meteorológica")

#####################################################################################

# Gráfico de Pontos que Retorna o Volume de Acidentes Fatais por Causa do Acidente

# Filtra apenas os casos de óbito 
df2 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforma em tabela para que tenhamos a frequência
df2 <-table(dados_acidentes_rs$causa_acidente)

# Transforma em data frame novamente
df2 <- as.data.frame(df2)

# Ordena o data frame
df2 <- df2[order(df2$Freq),]

dotchart(df2$Freq,labels=df2$Var1,cex=.6,
         main="Acidentes Fatais por Causa da Origem do Acidente",
         xlab="N° de Óbitos",
         pch = 19,
         col = c("darkblue","dodgerblue"),
         cex.main = 1.0, cex.lab = 1.5, xlim = c(0,13500))

#####################################################################################

# Gráfico de Barras Horizontais - Acidentes Fatais por Tipo de Acidente

df3 <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )

# Transforma em tabela para que tenhamos a frequência
df3 <-table(dados_acidentes_rs$tipo_acidente)

# Transforma em data frame novamente
df3 <- as.data.frame(df3)

df3 <- subset.data.frame(df3, Freq != 0)


df3 %>%
  mutate(name=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_segment( aes(xend=Var1, yend=0)) +
  geom_point( size=3, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Acidentes Fatais por Tipo de Acidente") +
  ylim(0,8000)

#####################################################################################

# Gráfico por Grupo de Veículo
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

#Avaliando acidentes por estação
#Verao

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoVerao <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Verão" )

volGrupoVeiculoFatalPorEstacaoVerao <- table(fatalidadePorEstacaoVerao$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoVerao

volBRFatalPorEstacaoVerao <- table(fatalidadePorEstacaoVerao$br)
volBRFatalPorEstacaoVerao <- as.data.frame(volBRFatalPorEstacaoVerao)

#Outono

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoOutono <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Outono" )

volGrupoVeiculoFatalPorEstacaoOutono <- table(fatalidadePorEstacaoOutono$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoOutono

volBRFatalPorEstacaoOutono <- table(fatalidadePorEstacaoOutono$br)
volBRFatalPorEstacaoOutono <- as.data.frame(volBRFatalPorEstacaoOutono)

#Inverno

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoInverno <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Inverno" )

volGrupoVeiculoFatalPorEstacaoInverno <- table(fatalidadePorEstacaoInverno$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoInverno

volBRFatalPorEstacaoInverno <- table(fatalidadePorEstacaoInverno$br)
volBRFatalPorEstacaoInverno <- as.data.frame(volBRFatalPorEstacaoInverno)

#Primavera

fatalidadePorEstacao <- dados_acidentes_rs %>% filter(dados_acidentes_rs$estado_fisico_seq == 4 )
fatalidadePorEstacaoPrimavera <- fatalidadePorEstacao %>% filter(fatalidadePorEstacao$estacao == "Primavera" )

volGrupoVeiculoFatalPorEstacaoPrimavera <- table(fatalidadePorEstacaoPrimavera$grupo_veiculo)
volGrupoVeiculoFatalPorEstacaoPrimavera

volBRFatalPorEstacaoPrimavera <- table(fatalidadePorEstacaoPrimavera$br)
volBRFatalPorEstacaoPrimavera <- as.data.frame(volBRFatalPorEstacaoPrimavera)
