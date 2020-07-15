### Script desenvolvido para criar o modelo preditivo com Random Forest ###

# Bibliotecas
library(tidyverse)
library(randomForest)

# Selecionando as variaveis para utilizar no algoritmo e criando um novo data frame
dados_rf <- dados_acidentes_rs %>% 
  select(mortos, idade, causa_acidente, tipo_acidente, fase_dia, 
         sexo, uso_solo, tipo_pista, tracado_via, tipo_envolvido)

# Analisando distribuição entre mortos e vivos
prop.table(table(dados_rf$mortos))

# Selecionando os dados com maior frequencia (Acidentes sem vitimas fatais)
# vlMaiorOcorr = Valor Maior Ocorrencia
vlMaiorOcorr <- dados_rf[dados_rf$mortos == 0, ]

# Limitando o numero de sobreviventes para x vezes o volume de mortos
amostraDadosRF <- sample(1:dim(vlMaiorOcorr)[1], size=921*3, replace=FALSE)

# Criando lista contendo os casos de fatalidade 
# e a amostra criada anteriormente dos sobreviventes
dados_rf_balanceado <- rbind(dados_rf[dados_rf$mortos==1,],vlMaiorOcorr[amostraDadosRF,])

# Analisando dados após o rebalanceamento
table(dados_rf_balanceado$mortos)

prop.table(table(dados_rf_balanceado$mortos))

set.seed(333)

# Selecionando dados para treino
amostraTreino <- sample(1:dim(dados_rf_balanceado)[1], size=(dim(dados_rf_balanceado)[1]/3*2), replace=FALSE)
amostraTreino

amostraTreino <- sample(nrow(dados_rf_balanceado), 0.7*nrow(dados_rf_balanceado), replace = FALSE)

# Criando dfs de treino e teste
treino <- dados_rf_balanceado[amostraTreino,]
teste <- dados_rf_balanceado[-amostraTreino,]

# Criando df para validação a partir do df original
amostraValidacao <- sample(1:dim(dados_rf)[1], size=dim(dados_rf)[1]/20, replace=FALSE)
dfValidacao <- dados_rf[amostraValidacao,]

# Verificando o número de dados nos dfs criados até agora
sum(treino$mortos==1)
sum(teste$mortos==1)
sum(dfValidacao$mortos==1)

table(treino$mortos)
table(teste$mortos)
table(dfValidacao$mortos)

set.seed(1234)

# Criando o modelo preditivo com RandomForest
rf.model3 <- randomForest(mortos ~., data=treino, ntree = 300, mtry = 3, importance = TRUE, prox=TRUE, strata=treino$mortos, sampsize=c(100,50))
rf.model3

# Aplicando o modelo preditivo no dataframe de teste
teste.pred1 <- predict(rf.model3,teste[,-1])

# Criando Matriz de Confusão
matrizConfusao <- table(observed=teste$mortos,predicted=teste.pred1)
matrizConfusao

# Indicador class error para mortos
matrizConfusao[2,1]/sum(matrizConfusao[2,])

# Matriz de Confusão Visual
fourfoldplot(matrizConfusao, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Matriz de Confusão - Base de Teste")


#### Analise de Métricas na base de Teste ####
TP = 198
FN = 95
FP = 157
TN = 656

#acuracia
acuracia <- (198 + 656) / (198 + 656 + 95 + 157)
acuracia

#especificidade - capacidade de identificar classes negativas
especificidade <- 656 / (656 + 157)
especificidade

#sensibilidade/RECALL - capacidade de identificar classes positivas
sensibilidade <- 198 / (198 + 95)
sensibilidade

# Precision - Percentual de tuplas marcadas como positivas corretamente
precision <- 198 / (198 + 157)
precision

fscore <- (2 * 0.6757679 * 0.5577465) / (0.6757679 + 0.5577465)
fscore

###########################################

# Aplicando o modelo preditivo no df de validação
dfValidacao.pred1 <- predict(rf.model3,dfValidacao[,-1])
# Criando Matriz de Confusão
matrizConfusaoValidacao <- table(observed=dfValidacao$mortos,predicted=dfValidacao.pred1)
# Indicador class error para mortos
matrizConfusaoValidacao[2,1]/sum(matrizConfusaoValidacao[2,])

# Matriz de Confusão Validacao Visual
fourfoldplot(matrizConfusaoValidacao, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Matriz de Confusão - Base de Validação")


# Variaveis Preditoras
result <-importance(rf.model3,)[,"MeanDecreaseAccuracy"]
importance(rf.model3)[order(result,decreasing=TRUE),]

# Avaliando importancia das variaveis preditoras 
varImpPlot(rf.model3, main = "Variáveis Preditoras")

#### Analise de Métricas na base de Validacao ####
TP = 198
FN = 95
FP = 157
TN = 656

TP = 34
FN = 17
FP = 342
TN = 1304

#acuracia
acuraciaValidacao <- (34 + 1303) / (34 + 1303 + 17 + 343)
acuraciaValidacao

#especificidade - capacidade de identificar classes negativas
especificidadeValidacao <- 1303 / (1304 + 343)
especificidadeValidacao

#sensibilidade/RECALL - capacidade de identificar classes positivas
sensibilidadeValidacao <- 34 / (34 + 17)
sensibilidadeValidacao

# Precision - Percentual de tuplas marcadas como positivas corretamente
precisionValidacao <- 34 / (34 + 342)
precisionValidacao

fscore <- (2 * sensibilidadeValidacao * precisionValidacao) / (sensibilidadeValidacao + precisionValidacao)
fscore

###########################################

