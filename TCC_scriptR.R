################################### Importacao dos dados ######################################
dados_anatel <- read.csv("C:/Herika/Enap/TCC/Dados_Anatel.csv", sep=";",head=TRUE)
head(dados_anatel)

dados_anatel$Pagamento_multa <- as.factor(dados_anatel$Pagamento_multa)

############################ Geracao tabela de treino e teste ################################
treino_index <-  sample(1:nrow(dados_anatel), 0.4*nrow(dados_anatel), replace = FALSE)
treino <- data.frame()
treino  <- dados_anatel[treino_index,]
head(treino)
teste  <- data.frame()
teste  <- dados_anatel[-treino_index,]
head(teste)

############################ Construcao da arvore de decisao ##################################
library(rpart)
treino_arvore <- rpart(Pagamento_multa ~ ., data = treino, method = "class")
summary(treino_arvore)
library(rpart.plot)
rpart.plot(treino_arvore)

######################## Previsao a partir do modelo construido ##############################
previsao <- predict(treino_arvore, teste)
previsao

