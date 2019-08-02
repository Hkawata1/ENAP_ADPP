####################################################### Importacao dos dados #######################################################
dados_anatel <- read.csv("https://raw.githubusercontent.com/Hkawata1/ENAP_ADPP/master/Base_Pados_Anatel.csv", sep = ";",head = TRUE)
head(dados_anatel)
str(dados_anatel)
dados_anatel$Valor_multa <- as.numeric(dados_anatel$Valor_multa)
dados_anatel$Saldo_devedor <- as.numeric(dados_anatel$Saldo_devedor)  
  
###################################################### Analise descritiva ##########################################################
library(ggplot2)
library(corrplot)
summary(dados_anatel)
graf_renuncia <- ggplot(dados_anatel, aes(x = Renuncia, fill = Renuncia)) +
                  geom_bar() +
                  xlab("Renúncia") +
                  ylab("Quantidade de processos") +
                  ggtitle("Variável Resposta") +
                  scale_fill_manual(values = c("lightblue", "blue"))
graf_renuncia
for(i in c("Superintendencia",
          "Tipologia_processo",
          "Servico_telecom",
          "Situacao_multa",
          "Recurso_juridico",
          "Cadin",
          "Divida_ativa"))
{
  print(i)
  print(table(dados_anatel$Renuncia,dados_anatel[,i]))
  print(chisq.test(table(dados_anatel$Renuncia,dados_anatel[,i])))
}
# Duvida: retirar "Recurso_juridico" justificando pelo valor de p desta variavel.

graf_duracao_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Duracao_processo)) +
                      geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                      stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                      xlab("Renúncia") +
                      ylab("Duração em dias") +                    
                      ggtitle("Duração do processo") +
                      scale_fill_manual(values = c("lightblue", "blue"))
graf_duracao_boxplot
# Apesar dos outliers verificados no grafico acima, o modelo escolhido não necessita tratar esses dados.
graf_multa_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Valor_multa)) +
                      geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                      stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                      xlab("Renúncia") +
                      ylab("Valores em R$") +                    
                      ggtitle("Valor da multa aplicada") +
                      scale_fill_manual(values = c("lightblue", "blue"))
graf_multa_boxplot
graf_saldo_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Saldo_devedor)) +
                      geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                      stat_boxplot(geom ='errorbar', width = 0.5, size= 0.7) +
                      xlab("Renúncia") +
                      ylab("Valores em R$") +                    
                      ggtitle("Saldo devedor principal") +
                      scale_fill_manual(values = c("lightblue", "blue"))
graf_saldo_boxplot
graf_serv_telecom <- ggplot(dados_anatel, aes(x = Servico_telecom, fill = Renuncia) ) +
                      geom_bar(position="fill") +
                      xlab("Tipos de serviços de telecomunicações") +
                      ylab("Quantidade de processos") +
                      ggtitle("Serviços de Telecomunicações") +
                      scale_fill_manual(values = c("lightblue", "blue")) 
graf_serv_telecom
graf_sit_multa <- ggplot(dados_anatel, aes(x = Situacao_multa, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Situação da multa") +
                  ylab("Quantidade de processos") +
                  ggtitle("Situação da multa aplicada") +
                  scale_fill_manual(values = c("lightblue", "blue")) 
graf_sit_multa
graf_rec_jur <- ggplot(dados_anatel, aes(x = Recurso_juridico, fill = Renuncia) ) +
                geom_bar(position="fill") +
                xlab("Processos com Recurso jurídico") +
                ylab("Quantidade de processos") +
                ggtitle("Processos em 2ª instância") +
                scale_fill_manual(values = c("lightblue", "blue")) 
graf_rec_jur
graf_cadin <- ggplot(dados_anatel, aes(x = Cadin, fill = Renuncia) ) +
              geom_bar(position="fill") +
              xlab("Processos em Cadin") +
              ylab("Quantidade de processos") +
              ggtitle("Registro no Cadin") +
              scale_fill_manual(values = c("lightblue", "blue")) 
graf_cadin
graf_div_ativa <- ggplot(dados_anatel, aes(x = Divida_ativa, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Processos em Dívida Ativa") +
                  ylab("Quantidade de processos") +
                  ggtitle("Inscrição em Dívida Ativa") +
                  scale_fill_manual(values = c("lightblue", "blue")) 
graf_div_ativa
graf_tipo_proc <- ggplot(dados_anatel, aes(x = Tipologia_processo, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Tipologias de processo") +
                  ylab("Quantidade de processos") +
                  ggtitle("Tipos de processos") +
                  scale_fill_manual(values = c("lightblue", "blue")) +
                  coord_flip()
graf_tipo_proc
graf_multa <- qplot(Valor_multa, data = dados_anatel, fill=Renuncia) +
              scale_fill_manual(values = c("lightblue", "blue")) +
              xlab("Valor da multa aplicada") +
              ylab("Quantidade de processos") +
              ggtitle("Processos com multa aplicada")
graf_multa
graf_duracao_proc <- qplot(Duracao_processo, data = dados_anatel, geom = "density", color = Renuncia) +
                      scale_color_manual(values = c("lightblue", "blue")) +
                      xlab("Duração em dias") +
                      ylab("Quantidade de processos") +
                      ggtitle("Duração dos processos")
graf_duracao_proc

# Teste: sugestao de graficos adicionais
base_cor_dados_anatel <- dados_anatel[,c("Duracao_processo", "Valor_multa", "Saldo_devedor")]
corrplot(cor(base_cor_dados_anatel), method="color",order="hclust",addCoef.col = "black",type="upper",diag=FALSE)
heatmap(x = cor(base_cor_dados_anatel), symm = TRUE, Rowv = FALSE,cexRow = 0.9,cexCol = 0.9,margins=c(7,7))

######################################## Balanceamento e Geracao da tabela de treino e teste ########################################
library(lme4)
library(ROSE)
dados_anatel_balanceados <- ovun.sample(Renuncia ~ ., data = dados_anatel, method = "over", N = 3104, seed = 1)$data
table(dados_anatel_balanceados$Renuncia)
set.seed(123)  
amostra <- sample.int(n = nrow(dados_anatel_balanceados), size = floor(.70*nrow(dados_anatel_balanceados)), replace = F)
treino <- dados_anatel_balanceados[amostra, ]
teste  <- dados_anatel_balanceados[-amostra, ]
table(treino$Renuncia)

################################################## Construcao da arvore de decisao #################################################
library(rpart)
library(rpart.plot)
library(partykit)

# Arvore com a variavel "Renuncia" para criacao de modelo de classificacao
fit <- rpart(Renuncia ~., data=treino)
prp(fit, main = "Classificação de multas aplicadas",type = 4, extra = 4,box.palette = "auto",faclen = 0)
fit <- rpart(Renuncia~., data=treino)
printcp(fit)
plotcp(fit)
pfit<- prune(fit, cp=0.011)
summary(pfit)
prp(pfit, main = "Classificação de multas aplicadas",type = 4, extra = 4,box.palette = "auto",faclen = 0)
plot(as.party(pfit), type="simple")

# Teste: Arvore com a variavel "Valor_multa" para criacao de modelo de regressao
fit2 <- rpart(Valor_multa ~., data = treino)
rpart.plot(fit2)
prp(fit2, main = "Classificação de multas aplicadas")
printcp(fit2)
plotcp(fit2)
pfit2<- prune(fit2, cp=0.014)
summary(pfit2)
plot(as.party(pfit2), type="extended")

# Teste: Modelo RandomForest
library(randomForest)
rf <- randomForest(Renuncia ~., data = treino, importance = TRUE)
rf
plot(rf$err.rate[,1],type="l")
importance(rf)
varImpPlot(rf)
# Verifica-se que a ordenacao da importancia relativa das variaveis altera conforme o metodo considerado.
a=c()
i=10
for (i in 2:12) {
  set.seed(123)
  rf2 <- randomForest(Renuncia~ ., data = treino, ntree = 176, mtry = i, importance = TRUE)
  predValid <- predict(rf2, teste, type = "class")
  a[i-1] = mean(predValid == teste$Renuncia)
}
plot(2:12,a,type='l',ylab="Assertividade")
set.seed(123)
rf3 <- randomForest(Renuncia ~ ., data = treino, ntree = 176, mtry = 10)
rf3

################################################### Avaliacao dos modelos construidos ##############################################
# Modelo Arvore de decisao com variavel "Renuncia"
p.Treino1 <- predict(pfit, type = "class", treino)
c.treino1 <- table(treino$Renuncia, p.Treino1)
c.treino1
sum(diag(prop.table(c.treino1)))
p.Teste1 <- predict(pfit, type = "class", teste)
c.teste1 <- table(teste$Renuncia, p.Teste1)
c.teste1
sum(diag(prop.table(c.teste1)))
# Comparando a taxa de acerto do modelo nos dois conjuntos (de treinamento e de teste), é possível afirmar que o modelo não 
# apresenta overfitting.

# Modelo Arvore de decisao com variavel "Valor_multa"
p.Treino2 <- predict(fit2, type = "prob", treino)
c.treino2 <- table(treino$Renuncia, p.Treino2)
c.treino2
sum(diag(prop.table(c.treino2)))
p.Teste2 <- predict(pfit2, type = "prob", teste)
c.teste2 <- table(teste$Renuncia, p.Teste2)
c.teste2
sum(diag(prop.table(c.teste2)))

# Modelo RandomForest
p.Treino.rf <- predict(rf3, type = "class", treino)
c.treino.rf <- table(treino$Renuncia, p.Treino.rf)
c.treino.rf
sum(diag(prop.table(c.treino.rf)))

p.Teste.rf <- predict(rf3, type = "class",teste)
c.teste.rf <- table(teste$Renuncia, p.Teste.rf)
c.teste.rf
sum(diag(prop.table(c.teste.rf)))
# Comparando a taxa de acerto do modelo nos dois conjuntos (de treinamento e de teste), há uma certa diferença consideravel
# na taxa de acerto indicando que o modelo apresenta overfitting.

# Teste: avaliacao dos tipos de classificadores
library(InformationValue)
cart <- ifelse(teste$Renuncia == "Sim",1,0)
pred.cart <- predict(pfit, type = "prob", teste)
confusionMatrix(cart, pred.cart[,2], threshold = 0.5)
precision(cart, pred.cart[,2], threshold = 0.5)
sensitivity(cart, pred.cart[,2], threshold = 0.5)
specificity(cart, pred.cart[,2], threshold = 0.5)
ks_stat(cart, pred.cart[,2])
plotROC(cart, pred.cart[,2])
AUROC(cart, pred.cart[,2])
ks_plot(cart, pred.cart[,2])

ranfor <- ifelse(teste$Renuncia == "Sim",1,0)
pred.ranfor <- predict(rf3, type = "prob", teste)
confusionMatrix(ranfor, pred.ranfor[,2], threshold = 0.5)
precision(ranfor, pred.ranfor[,2], threshold = 0.5)
sensitivity(ranfor, pred.ranfor[,2], threshold = 0.5)
specificity(ranfor, pred.ranfor[,2], threshold = 0.5)
ks_stat(ranfor, pred.ranfor[,2])
plotROC(ranfor, pred.ranfor[,2])
AUROC(ranfor, pred.ranfor[,2])
ks_plot(ranfor, pred.ranfor[,2])

library(pROC)
roc.cart <- plot.roc(cart, pred.cart[,1], main="Comparação de modelos", percent=TRUE, xlim=c(100,0), col="blue")
roc.ranfor <- lines.roc(ranfor, pred.ranfor[,2], percent=TRUE, col="red")
legend("bottomright", legend = c("Árvore de decisão", "Random Forest"),col = c("blue", "red"),lwd = 2)
roc.cart.ci <- plot.roc(cart, pred.cart[,1], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.cart <- ci.se(roc.cart, specificities = seq(0, 100, 5)) 
plot(ci.cart, type = "shape", col = "#1c61b6AA")
roc.ranfor.ci <- plot.roc(ranfor, pred.ranfor[,1], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.ranfor <- ci.se(roc.ranfor, specificities = seq(0, 100, 5)) 
plot(ci.ranfor, type = "shape", col = "#1c61b6AA")