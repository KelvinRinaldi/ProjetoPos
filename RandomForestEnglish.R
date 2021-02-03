### Script developed to create the predictive model with Random Forest ###

# Libraries
library(tidyverse)
library(randomForest)
library(ggplot2)

# Selecting the variables to use in the algorithm and creating a new data frame
dados_rf <- dados_acidentes_rs %>% 
  select(mortos, age, origin_accident, type_accident, day_phase, 
         gender, driver_only, track_type, track_layout, type_involved)

# Analyzing distribution between the dead and the living
prop.table(table(dados_rf$mortos))

# Selecting data more frequently (Accidents without fatalities)
# vlMaiorOcorr = Valor Maior Ocorrencia
vlMaiorOcorr <- dados_rf[dados_rf$mortos == 0, ]

# Limiting the number of survivors to x times the death toll
amostraDadosRF <- sample(1:dim(vlMaiorOcorr)[1], size=921*3, replace=FALSE)

# Creating list containing the fatality cases
# e a amostra criada anteriormente dos sobreviventes
dados_rf_balanceado <- rbind(dados_rf[dados_rf$mortos==1,],vlMaiorOcorr[amostraDadosRF,])

# Analyzing data after rebalancing
table(dados_rf_balanceado$mortos)

prop.table(table(dados_rf_balanceado$mortos))

set.seed(333)

# Selecting training data
amostraTreino <- sample(1:dim(dados_rf_balanceado)[1], size=(dim(dados_rf_balanceado)[1]/3*2), replace=FALSE)
amostraTreino

amostraTreino <- sample(nrow(dados_rf_balanceado), 0.7*nrow(dados_rf_balanceado), replace = FALSE)

# Creating training and test dfs
treino <- dados_rf_balanceado[amostraTreino,]
teste <- dados_rf_balanceado[-amostraTreino,]

# Creating df for validation from the original df
amostraValidacao <- sample(1:dim(dados_rf)[1], size=dim(dados_rf)[1]/20, replace=FALSE)
dfValidacao <- dados_rf[amostraValidacao,]

# Checking the number of data in the dfs created so far
sum(treino$mortos==1)
sum(teste$mortos==1)
sum(dfValidacao$mortos==1)

table(treino$mortos)
table(teste$mortos)
table(dfValidacao$mortos)

treino <- na.exclude(treino)

set.seed(1234)

# Creating the predictive model with Random Forest
rf.model3 <- randomForest(mortos ~., data=treino, ntree = 300, mtry = 3, importance = TRUE, prox=TRUE, strata=treino$mortos, sampsize=c(100,50))
rf.model3

# Applying the predictive model to the test dataframe
teste.pred1 <- predict(rf.model3,teste[,-1])

# Creating Confusion Matrix
matrizConfusao <- table(observed=teste$mortos,predicted=teste.pred1)
matrizConfusao


# Class error indicator for dead
matrizConfusao[2,1]/sum(matrizConfusao[2,])

# Visual Confusion Matrix 1
fourfoldplot(matrizConfusao, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Matriz de Confusão - Base de Teste")


# Visual Confusion Matrix 2

matrizConfusaoFrame <- as.data.frame(matrizConfusao)

ggplot(data =  matrizConfusaoFrame,
       mapping = aes(x = observed, y = predicted)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%5.0f", Freq)), vjust = 1, fontface  = "bold", size = 5.00) +
  scale_fill_gradient(low = "grey70", high = "grey95") +
  theme_bw() + theme(legend.position = "none")


#### Analysis of Metrics in the Test base ####
TP = 198
FN = 95
FP = 157
TN = 656

# accuracy
acuracia <- (198 + 656) / (198 + 656 + 95 + 157)
acuracia

# specificity - ability to identify negative classes
especificidade <- 656 / (656 + 157)
especificidade

# sensitivity / RECALL - ability to identify positive classes
sensibilidade <- 198 / (198 + 95)
sensibilidade

# Precision - Percentage of tuples correctly marked as positive
precision <- 198 / (198 + 157)
precision

fscore <- (2 * 0.6757679 * 0.5577465) / (0.6757679 + 0.5577465)
fscore

###########################################

# Applying the predictive model in the validation df
dfValidacao.pred1 <- predict(rf.model3,dfValidacao[,-1])
# Creating Confusion Matrix
matrizConfusaoValidacao <- table(observed=dfValidacao$mortos,predicted=dfValidacao.pred1)
# Class error indicator for dead
matrizConfusaoValidacao[2,1]/sum(matrizConfusaoValidacao[2,])

# Confusion Matrix Visual Validation 1
fourfoldplot(matrizConfusaoValidacao, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Matriz de Confusão - Base de Validação")

# Confusion Matrix Visual Validation 2

matrizConfusaoValidacaoFrame <- as.data.frame(matrizConfusaoValidacao)

ggplot(data =  matrizConfusaoValidacaoFrame,
       mapping = aes(x = observed, y = predicted)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, fontface  = "bold", size = 5.00) +
  scale_fill_gradient(low = "grey70", high = "grey95") +
  theme_bw() + theme(legend.position = "none")

# Predictor Variables
result <-importance(rf.model3,)[,"MeanDecreaseAccuracy"]
importance(rf.model3)[order(result,decreasing=TRUE),]

result

# Assessing the importance of predictor variables 
varImpPlot(rf.model3, main = "Variáveis Preditoras")

#### Analysis of Metrics on the Validation basis ####
TP = 198
FN = 95
FP = 157
TN = 656

TP = 34
FN = 17
FP = 342
TN = 1304

acuraciaValidacao <- (34 + 1303) / (34 + 1303 + 17 + 343)
acuraciaValidacao

especificidadeValidacao <- 1303 / (1304 + 343)
especificidadeValidacao

sensibilidadeValidacao <- 34 / (34 + 17)
sensibilidadeValidacao


precisionValidacao <- 34 / (34 + 342)
precisionValidacao

fscore <- (2 * sensibilidadeValidacao * precisionValidacao) / (sensibilidadeValidacao + precisionValidacao)
fscore

###########################################

